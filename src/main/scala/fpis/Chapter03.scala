package fpis

object Chapter03 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def sum_fl(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => foldLeft(xs, x)((x, y) => x + y)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def product_fl(ds: List[Double]): Double = ds match {
      case Nil => 0.0
      case Cons(0.0, _) => 0.0 // short circuit
      case Cons(x, xs) => foldLeft(xs, x)((x, y) => x * y)
    }

    def product2(ds: List[Double]): Double = {
      foldRight(ds, 1.0)((x, y) => {
//        println(x + ", acc=" + y)
        if (x == 0.0) return 0.0
        x * y
      })
    }

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, y) => y + 1)
    }

    def length_fl[A](as: List[A]): Int = {
      foldLeft(as, 0)((x, _) => x + 1)
    }

    def reverse[A](as: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]
      foldRight(as, buf)((x, y) => y += x)
      List(buf.toList:_*)
    }

    def append[A](as: List[A]): String = {
      foldLeft(as, "")(_+_)
    }

    def map[A,B](as: List[A])(f: A => B): List[B] = {
      foldRight(as, Nil:List[B])((x, y) => Cons(f(x), y))
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
      foldRight(as, Nil:List[B])((a, b) => List.append(f(a), b))
    }


    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil:List[A])((a, b) => {
        if (f(a)) Cons(a, b) else b
      })
    }

    def filter_fm[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => {
        if (f(a)) Cons(a, Nil:List[A]) else Nil:List[A]
      })
    }

    //    @annotation.tailrec
    def addList(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addList(t1, t2))
    }

    //    @annotation.tailrec
    def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
      case (Nil, _) => Nil
      case(_, Nil) => Nil
      case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case(_, Nil) => true // if sub advances to end then sub found; this case needs to be first
      case(Nil, _) => false // if sup reaches the end then sub not found
      case(Cons(h1, t1), Cons(h2, t2)) => {
        if (h1 == h2) {
          hasSubsequence(t1, t2)
        } else {
          hasSubsequence(t1, sub)
        }
      }
    }

    def tail[A](ds: List[A]): List[A] = ds match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

    def setHead[A](a: A, l: List[A]): List[A] = {
      if (l == Nil)
      sys.error("setHead on empty list")
      else
      Cons(a, List.tail(l))
    }

    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] =  {
      if (n < 1) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n-1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    // book tailrec version
    def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A] // the secret sauce
      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => List(buf.toList: _*)
        case Cons(h,t) => buf += h; go(t)
      }
      go(l)
    }

    //    @annotation.tailrec
    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
      // not tail recursive
      //      case Nil => z
      //      case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
      @annotation.tailrec
      def go(cur: List[A], acc: B): B = cur match {
        case Nil => acc
        case Cons(x, xs) => go(xs, f(acc, x))
      }
      go(as, z)
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =  a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

    def concat[A](as: List[List[A]]): List[A] = {
      foldRight(as, Nil:List[A])((a, z) =>  append(a, z))
    }

    def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  object Tree {
    //    @annotation.tailrec
    def size[A](t: Tree[A]): Int = t match {
      case (Branch(a, b)) => 1 + size(a) + size(b)
      case (Leaf(_)) => 1
    }

    def maximum(t: Tree[Int]): Int = t match {
      case(Branch(a, b)) => maximum(a) max maximum(b)
      case(Leaf(a)) => a
    }

    def depth[A](t: Tree[A]): Int = t match {
      case(Branch(a, b)) => 1 + depth(a).max(depth(b))
      case(Leaf(_)) => 1
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case(Branch(a, b)) => Branch(map(a)(f), map(b)(f))
      case(Leaf(a)) => Leaf(f(a))
    }

    def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Branch(a, b) => g(fold(a)(f)(g), fold(b)(f)(g))
      case Leaf(a) => f(a)
    }
  }
}

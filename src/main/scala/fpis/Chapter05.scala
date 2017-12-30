package fpis

import scala.annotation.tailrec
import Stream._

/**
  * FPIS Chapter 5 problems
  *
  * @author markvaldez@gmail.com
  */
object Chapter05 {

  /**
    * Stream class:
    *
    * +A means that if T isA subclass of A then Stream[T] isA subclass of Stream[A]
    *
    */
  trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    // 5.14
    def startsWith[A](s: Stream[A]): Boolean = {
      this.zipAll(s).takeWhile_uf {
        case ((Some(_), Some(_))) => true
        case((None, None)) => false
        case((None, Some(_))) => true
        case((Some(_), None)) => false
      } forAll {
        case (h1, h2) => h1 == h2
      }
    }

    // 5.13
    def map_uf[B](f: A => B): Stream[B] = {
      unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _  => None
      }
    }

    def take_uf(n: Int): Stream[A] = {
     unfold((this, n)) {
       case (Cons(h, t), 1) => Some(h(), (t(), 0))
       case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i -1)))
       case _ => None
     }
    }

    def takeWhile_uf(p: A => Boolean): Stream[A] = {
      unfold(this) {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }
    }

    def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
      unfold((this, b)) {
          case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
          case _ => None
      }
    }

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, s)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty, t2())))
        case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Stream.empty))
        case _ => None
      }
    }

    // 5.12
    // again, unable to figure fibs out
    // explicitly defining function with p => p match {...}
    def fibs_uf(): Stream[Int] = {
      unfold((0, 1))(p => p match {
        case (f0, f1) => Some((f0, (f1, f0 + f1)))
      })
    }

    // unfold is constructing the stream for us
    // so we just have to pass in the current state
    // and the next state
    def from_uf(n: Int): Stream[Int] = {
      unfold(n)(n => Some(n, n+1))
    }

    def constant_uf[A](a: A): Stream[A] = {
      unfold(a)(a => Some(a, a))
    }

    def ones_uf(): Stream[Int] = {
      constant_uf(1)
    }

    def ones: Stream[Int] = Stream.cons(1, ones)

    // Exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((h, s)) => Stream.cons(h, unfold(s)(f))
        case None => Stream.empty
      }
    }

    // Exercise 5.10
    // I couldn't figure fibs out.  This is the book
    def fibs: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] =
        Stream.cons(a, go(b, a+b))
      go(0, 1)
    }

    // Exercise 5.9
    def from(n: Int): Stream[Int] = {
      Stream.cons(n, from(n + 1))
    }

    // Exercise 5.8
    def constant[A](a: A): Stream[A] = {
      Stream.cons(a, constant(a))
    }

    // Exercise 5.7
    def filter(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
    }

    def append[B>:A](s: Stream[B]): Stream[B] = {
      foldRight(s)((h, t) => Stream.cons(h, t))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Stream.empty[B])((h, t) => f(h).append(t))
    }

    // Exercise 5.6
    def headOption_fr: Option[A] = {
      foldRight(None: Option[A])((a, _) => Some(a))
    }

    // Exercise 5.5
    def takeWhile_fr(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => {
        if (p(a))
          Stream.cons(a, b)
        else
         Stream.empty // only traverses while p(_) == true
//        b // traverses till p(_) == true; initially I misunderstand the spec
      })
    }

    // Exercise 5.4
    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    // Exercise 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) => {
        if (p(h()))
          Stream.cons(h(), t().takeWhile(p))
        else
          t().takeWhile(p)
      }
      case _ => Stream.empty
    }

    // Exercise 5.2 (part 2)
    @tailrec
    final def drop(n: Int): Stream[A] = {
      if (n == 0) this else this match {
        case Cons(_, t) => t().drop(n - 1)
        case _ => Stream.empty
      }
    }

    // Exercise 5.2 (part 1)
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }


    // Exercise 5.1
    def toList: List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @annotation.tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Cons(h, t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }

      go(this)
    }

    def exists(p: A => Boolean): Boolean = {
      foldRight(false)((a,b) => p(a) || b)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  // companion
  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }

}

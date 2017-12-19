package fpis

import fpis.Chapter03.{Branch, Cons, Leaf, List, Nil, Tree}
import org.scalatest.FunSuite

class Chapter03Test extends FunSuite {

  test("exercise.3.1") {
    val result = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(result == 3)
  }

  test("exercise.3.2") {
    val x = List(1, 2, 3, 4, 5)
    val y = List.tail(x)
    assert(List.sum(y) == 14)

    val z = List.tail(y)
    assert(List.sum(z) == 12)

    val i = List(1)
    val j = List.tail(i)
    assert(List.sum(j) == 0)

    val k = List()
    assert(k == Nil)
  }

  test("exercise.3.3") {
    val x = List.setHead(0, List(1, 2, 3, 4, 5))
    assert(List.sum(x) == 14)

    val y = List.setHead(0, List(1, 2))
    assert(List.sum(y) == 2)

    val z = List.setHead(0, List(2))
    assert(List.sum(z) == 0)
  }

  test("exercise.3.4") {
    val x = List.drop(List(1, 2, 3, 4, 5), 2)
    assert(List(3, 4, 5) == x)

    val y = List.drop(List(1, 2, 3), 4)
    assert(List() == y)
  }

  test("exercise.3.5") {
    val list1 = List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3)
    assert(list1 == List(3, 4, 5))

    val list2 = List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 3)
    assert(list2 == List(4, 5))

    // cannot remove value in middle of list
//    val list3 = List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x == 3)
//    assert(List(1, 2, 4, 5) == list3)
  }

  test("exercise.3.6") {
    assert(List.init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))

    assert(List.init(List(1, 2)) == List(1))
  }

  test("exercise.3.7") {
    assert(List.product2(List(1, 2, 3, 4)) == 24)
    println()
    assert(List.product2(List(1, 2, 3, 0, 4, 5)) == 0.0)
  }

  test("exercise.3.8") {
    val result = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    assert(result == Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("exercise.3.9") {
    assert(List.length(List(1, 2, 3, 4)) == 4)
    assert(List.length(List(1, 2, 3, 4, 5)) == 5)
    assert(List.length(List(1, 2, 3, 4, 5, 5)) == 6)
  }

  test("exercise.3.10") {
    assert(List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
    assert(List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) == 10)
  }

  test("exercise.3.11") {
    // sum
    assert(List.sum_fl(List(1, 2, 3, 4)) == 10)
    assert(List.sum_fl(List(1, 2, 3, 4, 5)) == 15)

    // product
    assert(List.product_fl(List(1, 2, 3)) == 6)
    assert(List.product_fl(List(1, 2, 3, 4)) == 24)

    // length
    assert(List.length_fl(List(1, 2, 3, 4)) == 4)
    assert(List.length_fl(List(1, 2, 3, 4, 5)) == 5)
    assert(List.length_fl(List(1, 2, 3, 4, 5, 5)) == 6)
  }

  test("exercise.3.12") {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("exercise.3.13") {
    // TODO
  }

  test("exercise.3.14") {
    assert(List.append(List(1, 2, 3, 4)) == "1234")
  }

  test("exercise.3.15") {
    // todo: used list of list; is there a better way?
    assert(List.concat(List(List(1, 2), List(3, 4), List(5, 6))) == List(1, 2, 3, 4, 5, 6))
  }

  test("exercise.3.16") {
    assert(List.foldRight(List(1, 2, 3, 4), Nil: List[Int])((x, y) => Cons(x + 1, y)) == List(2, 3, 4, 5))
  }

  test("exercise.3.17") {
    assert(List.foldRight(List(0.0, 1.0, 2.0), Nil: List[String])((x, y) => Cons(x.toString, y)) == List("0.0", "1.0", "2.0"))
  }

  test("exercise.3.18") {
    assert(List.map(List(1, 2, 3))((x) => 1.0 * x) == List(1.0, 2.0, 3.0))
  }

  test("exercise.3.19") {
    assert(List.filter(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 0) == List(2, 4, 6))
  }

  test("exercise.3.20") {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  test("exercise.3.21") {
    assert(List.filter_fm(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 0) == List(2, 4, 6))
  }

  test("exercise.3.22.0") {
    assert(List(5, 7, 9) == List.addList(List(1, 2, 3), List(4, 5, 6)))
  }

  test("exercise.3.23.0") {
    // todo: this uses types A, B, C; explanation
    assert(List(5, 7, 9) == List.zipWith(List(1, 2, 3), List(4, 5, 6))(_+_))
  }

  test("exercise.3.24.0.hard") {
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(List.hasSubsequence(List(1, 2, 3, 4), List()))
  }

  test("exercise.3.25") {
    assert(Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) == 7)
  }

  test("exercise.3.26") {
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(3)))) == 4)
  }

  test("exercise.3.27") {
    assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(4), Leaf(6)), Leaf(3)))) == 4)
  }

  test("exercise.3.28") {
    assert(Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(3))))(a => a+1) ==
      Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(5), Leaf(4))))
  }

  test("exercise.3.29") {
    val s_tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val i_tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(3)))
    val d_tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(4), Leaf(6)), Leaf(3)))
    val m_tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(3)))
    val m_tree_expected = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(5), Leaf(4)))

    val size = Tree.fold(s_tree)((_) => 1)((a, b) => 1 + a + b)
    assert(size == 7)

    // max
    val max = Tree.fold(i_tree)((a) => a)((a, b) => a max b)
    assert(max == 4)

    // depth
    val depth = Tree.fold(d_tree)((_) => 1)((a, b) => 1 + (a max b))
    assert (depth == 4)

    //                             secret sauce ---+
    //                                             |
    // map                                         v
    assert(Tree.fold(m_tree)((a) => Leaf(a + 1): Tree[Int])((x, y) => Branch(x, y)) == m_tree_expected)
  }
  
  test("examples") {
    println("examples")
    val ar = Array[Int](1, 2, 3, 4, 5)

    println(ar.mkString(","))

    println("reduceLeft")
    assert(15 == ar.reduceLeft(_+_))
    ar.reduceLeft((a,b) => {println(a + ", " + b); a + b})

    println("reduceRight")
    ar.reduceRight((a,b) => {println(a + ", " + b); a + b})

    println("foldLeft")
    assert(15 == ar.foldLeft(0)(_+_))
    println(ar.foldLeft("")(_+_+","))

    println("foldRight")
    assert(15 == ar.foldRight(0)(_+_))
    println(ar.foldRight(0)((a,b) => {println(a + ", acc=" + b); a + b}))
  }
}

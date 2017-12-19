package fpis

import org.scalatest.FunSuite
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import Chapter02._

class Chapter02Test extends FunSuite {
  // fibo(n) where n > 0
  test("fibo") {
    assert(fibo(1) == 0)
    assert(fibo(2) == 1)
    assert(fibo(3) == 1)
    assert(fibo(4) == 2)
    assert(fibo(5) == 3)
    assert(fibo(6) == 5)
  }

  test("isSorted") {
    assert(isSorted[Int](Array(1, 3, 5, 7, 9), (a,b) => a < b))
    assert(!isSorted[Int](Array(8, 7, 6, 5, 4, 3, 2, 1), (a,b) => a < b))
    assert(!isSorted[Int](Array(8, 7, 6, 5, 4, 3, 2), (a,b) => a < b))
    assert(!isSorted[Int](Array(2, 1), (a,b) => a < b))
    assert(isSorted[Int](Array(100), (a,b) => a < b))
    assert(isSorted[Char](Array('a', 'b', 'c'), (a,b) => a < b))
    assert(!isSorted[Char](Array('z', 'b', 'c'), (a,b) => a < b))
  }
}

object Chapter02Properties extends Properties("Chapter02") {
  property("curry") = forAll({
    (f: (String, Char) => Int, s: String, c: Char) =>
      curry(f)(s)(c) == f(s, c)
  })

  property("uncurry") = forAll({
    (f: String => Char => Int, s: String, c: Char) =>
      uncurry(f)(s, c) == f(s)(c)
  })

  property("compose") = forAll({
    (f: String => Int, g: Char => String, c: Char) =>
      compose(f, g)(c) == f(g(c))
  })
}

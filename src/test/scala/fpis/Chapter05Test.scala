package fpis

import org.scalatest.FunSuite

/**
  *
  * @author { @link "markvaldez@gmail.com"}
  */
class Chapter05Test extends FunSuite {
  test("Stream.toList") {
    assert(Chapter05.Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("Stream.take") {
    assert(Chapter05.Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Chapter05.Stream(1, 2, 3).take(5).toList == List(1, 2, 3))
  }

  test("Stream.drop") {
    assert(Chapter05.Stream(1, 2, 3).drop(1).toList == List(2, 3))
    assert(Chapter05.Stream(1, 2, 3).drop(5).toList == List())
    assert(Chapter05.Stream().drop(5).toList == List())
  }

  test("Stream.takeWhile") {
    assert(Chapter05.Stream(1, 2, 3).takeWhile(p => p == 2).toList == List(2))
    assert(Chapter05.Stream(1, 2, 3).takeWhile(p => p > 1).toList == List(2, 3))
    assert(Chapter05.Stream(1, 2, 3).takeWhile(p => p == 1).toList == List(1))
  }

  test("Stream.exists") {
     assert(!Chapter05.Stream(1, 2, 3).exists((a) => a == 5))
     assert(Chapter05.Stream(1, 2, 3).exists((a) => a == 2))
  }

  test("Stream.forAll") {
    assert(Chapter05.Stream(1, 2, 3).forAll(a => a > 0))
    assert(!Chapter05.Stream(1, 2, 3).forAll(a => a == 1))
  }

  test("Stream.takeWhile_fr") {
    assert(Chapter05.Stream(1, 2, 3).takeWhile_fr(p => p == 2).toList == List())
    assert(Chapter05.Stream(1, 2, 3).takeWhile_fr(p => p > 0).toList == List(1, 2, 3))
    assert(Chapter05.Stream(1, 2, 3).takeWhile_fr(p => p == 1).toList == List(1))
  }

  test("Stream.headOption") {
    assert(Chapter05.Stream(1, 2, 3).headOption.toList == List(1))
    assert(Chapter05.Stream().headOption.toList == List())
  }

  test("Stream.headOption_fr") {
    assert(Chapter05.Stream(1, 2, 3).headOption_fr.toList == List(1))
    assert(Chapter05.Stream().headOption_fr.toList == List())
  }

  test("Stream.filter") {
    assert(Chapter05.Stream(1, 2, 3).filter(_ > 1).toList == List(2, 3))
    assert(Chapter05.Stream(1, 2, 3).filter(_ == 2).toList == List(2))
  }

  test("Stream.map") {
    assert(Chapter05.Stream(1, 2, 3).map(_ + 10).toList == List(11, 12, 13))
  }

  test("Stream.flatMap") {
    assert(Chapter05.Stream(1, 2, 3).flatMap(a => Chapter05.Stream(a + 1)).toList == List(2, 3, 4))
  }

  test("Stream.constant") {
    assert(Chapter05.Stream().constant(1).take(5).toList == List(1, 1, 1, 1, 1))
  }

  test("Stream.from") {
    assert(Chapter05.Stream().from(10).take(4).toList == List(10, 11, 12, 13))
  }

  test("Stream.fibs") {
    assert(Chapter05.Stream().fibs.take(5).toList == List(0, 1, 1, 2, 3))
  }

  test("Stream.unfold") {
    assert(Chapter05.Stream().unfold(1)(a => Some(a, a + 1)).take(3).toList == List(1, 2, 3))
  }

  test("Stream.fibs_uf") {
     assert(Chapter05.Stream().fibs_uf().take(5).toList == List(0, 1, 1, 2, 3))
  }

  test("Stream.from_uf") {
     assert(Chapter05.Stream().from_uf(10).take(4).toList == List(10, 11, 12, 13))
  }

  test("Stream.constant_uf") {
    assert(Chapter05.Stream().constant_uf(1).take(5).toList == List(1, 1, 1, 1, 1))
  }

  test("Stream.ones_uf") {
    assert(Chapter05.Stream().ones_uf().take(3).toList == List(1, 1, 1))
  }

  test("Stream.map_uf") {
    assert(Chapter05.Stream(1, 2, 3).map_uf(_ + 10).toList == List(11, 12, 13))
  }

  test("Stream.take_uf") {
    assert(Chapter05.Stream(1, 2, 3).take_uf(2).toList == List(1, 2))
    assert(Chapter05.Stream(1, 2, 3).take_uf(5).toList == List(1, 2, 3))
  }

  test("Stream.takeWhile_uf") {
    assert(Chapter05.Stream(1, 2, 3).takeWhile_uf(p => p == 2).toList == List())
    assert(Chapter05.Stream(1, 2, 3).takeWhile_uf(p => p > 0).toList == List(1, 2, 3))
    assert(Chapter05.Stream(1, 2, 3).takeWhile_uf(p => p == 1).toList == List(1))
  }

  test("Stream.zipWith") {
    assert(Chapter05.Stream(1, 2, 3).zipWith(Chapter05.Stream(1, 2, 3))((a, b) => a + b).toList == List(2, 4, 6))
    assert(Chapter05.Stream(1, 2, 3, 4).zipWith(Chapter05.Stream(1, 2, 3))((a, b) => a + b).toList == List(2, 4, 6))
    assert(Chapter05.Stream(1, 2, 3).zipWith(Chapter05.Stream(1, 2))((a, b) => a + b).toList == List(2, 4))
  }

  test("Stream.zipAll") {
    assert(Chapter05.Stream(1, 2, 3).zipAll(Chapter05.Stream(1, 2, 3)).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3))))
    assert(Chapter05.Stream(1, 2).zipAll(Chapter05.Stream(1, 2, 3)).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (None, Some(3))))
    assert(Chapter05.Stream(1, 2, 3).zipAll(Chapter05.Stream(1, 2)).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None)))

  }

  test("Stream.startsWith") {
    assert(Chapter05.Stream(1, 2, 3).startsWith(Chapter05.Stream(1, 2)))
    assert(!Chapter05.Stream(1, 2, 3).startsWith(Chapter05.Stream(1, 4)))
    assert(!Chapter05.Stream(1, 2).startsWith(Chapter05.Stream(1, 2, 3)))
  }
}

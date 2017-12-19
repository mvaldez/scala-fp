package fpis

import fpis.Chapter04.Option
import org.scalatest.FunSuite

/**
  * FP Club Questions:
  *   Explain trait? sealed trait?
  *   Review class versus object
  *   Walk through flatmap (i.e. a flatMap (aa => b map (bb => f(aa, bb))))
  */
class Chapter04Test extends FunSuite {

  test("exercise.4.2") {
    assert(Option.variance(Seq(1, 2, 3, 4)) == Chapter04.Some(1.25))
  }

  test("exercise.4.3") {
    assert(Option.parseInsQuote("10", "10") == Chapter04.Some(100))
  }

  test("exercise.4.4") {
    assert(Chapter04.Some(List(1, 2, 3)) == Option.sequence(
      List[Option[Int]](Chapter04.Some(1), Chapter04.Some(2), Chapter04.Some(3)))
    )
  }

  test("exercise.4.5") {
     val result = Chapter04.Option.traverse(List(1, 2, 3))(a => Chapter04.Option.Try(a.toString))
    assert(result == Chapter04.Some(List("1", "2", "3")))
  }
}

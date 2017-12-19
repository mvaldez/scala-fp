package fpis

import scala.util.Try

/**
  *
  */
object Chapter04 {

  sealed trait Either[+E, +A]
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case None => None
        case (Some(a)) => Some(f(a))
      }

    def flatMap[B](f: A => Option[B]) : Option[B] = {
      this map f getOrElse None
    }

    def getOrElse[B>:A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B>:A](ob: => Option[B]): Option[B] = {
      this map (Some(_)) getOrElse ob
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(a => if (f(a)) Some(a) else None)
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // companion
  object Option {

    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs) flatMap(m => mean(xs.map(a => Math.pow(a - m, 2))))
    }

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      // a flatMap (aa => b map (bb => f(aa, bb))) todo: ??
      case (_, None) => None
      case(None, _) => None
      case(Some(x), Some(y)) => Some(f(x, y))
    }

    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
      age * numberOfSpeedingTickets
    }

    def parseInsQuote(age: String, num: String): Option[Double] = {
      val optAge: Option[Int] = Try(age.toInt)
      val optNum: Option[Int] = Try(num.toInt)
      map2(optAge, optNum) (insuranceRateQuote)
    }

    def Try[A](a: =>A): Option[A] = {
      try Some(a)
      catch { case e: Exception => None }
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(x => sequence(t).map(y => x :: y))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      sequence(a.map(aa => f(aa)))
    }
  }
}

package calculator

import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal((b() * b()) - (4 * a() * c()))

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      if (a() == 0) Set()
      else delta() match {
        case d if d > 0 => Set((-b() + sqrt(d)) / (a() * 2), (-b() - sqrt(d)) / (a() * 2))
        case d if d == 0 => Set(-b() / (a() * 2))
        case default => Set()
      }
    }

}

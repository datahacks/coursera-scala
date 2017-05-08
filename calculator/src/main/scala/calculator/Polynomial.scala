package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    // Δ = b² - 4ac
    Var(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / 2a
    
    Var(delta() match {
      case d if (d >= 0) => Set((-b() + scala.math.sqrt(delta())) / (2*a()),(-b() - scala.math.sqrt(delta())) / (2*a()))
      case d if (d == 0) => Set(-b() / (2*a()) )
      case d => Set()
    }
        
    )
  }
}

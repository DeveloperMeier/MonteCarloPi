import scala.math.sqrt
import scala.util.Random

object MonteCarloRec {
  def main(args: Array[String]): Unit = println(
    generation(
      monteCarlo(100, isInCircle),
      monteCarlo(100, isInCircle),
      100000
    )
  )

  def monteCarlo(n: Int, test: () => Boolean) = Stream.continually(if (test()) 1.00 else 0.00 )
    .take(n)
    .sum / n

  def isInCircle(): Boolean = {
    val (x, y) = (Random.nextFloat, Random.nextFloat)
    sqrt(x*x + y*y) <= 1
  }

  def generation(p: Double, n: Double, precision: Int, gen: Int = 1): Double = {
    if (Math.abs(p - n) < 0.000001) p else {
      val c = monteCarlo(precision, isInCircle) * 4
      println(s"Generation #${gen}: Seeded with ${p}, ${n}")
      println("_________________________________")
      println(s"Guess: ${c}")
      println(s"Variance: ${Math.abs(p - n)}")
      println("")
      generation((p+n)/2, (n + c)/ 2, precision, gen + 1)
    }
  }
}

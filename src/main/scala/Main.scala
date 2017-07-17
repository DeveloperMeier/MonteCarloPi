import scala.math.sqrt
import scala.util.Random

object Main {
  val precisionIntent = 10000 // How many simulations to run for a single generation: Higher == Longer per Generation
  val generationCount = 100 // How many generations to run for each simulation: Higher == Longer per Generation
  val totalGenerations = 1000 // How many generations to average: Higher == More Iterations

  def main(args: Array[String]):Unit = {
    val generations = for {
      _ <- Range(0, totalGenerations)
      generation <- generationValue(precisionIntent, generationCount)
    } yield generation
    println(s"Final Guess After ${totalGenerations} Sequences: ${generations.sum / generations.size}")
  }

  def generationValue(simulationPrecision: Int, generationCount: Int): IndexedSeq[Double] = {
    val gen = for {
      _ <- Range(0, generationCount)
      it = monteCarlo(simulationPrecision, circleTest) * 4
    } yield it
    println(s"Generation Guess: ${gen.sum / gen.size}")
    gen
  }

  def circleTest() = {
    val (x, y) = (Random.nextFloat, Random.nextFloat)
    sqrt(x*x + y*y) <= 1
  }

  def monteCarlo(trials: Int, test:() => Boolean) = Stream.continually(if (test()) 1.00 else 0.00)
    .take(trials)
    .sum / trials
}

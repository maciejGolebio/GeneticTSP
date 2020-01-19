import scala.util.Random

object Graph extends App {
  def createCompleteWeightGraph(n: Int): Array[Array[Int]] = {
    val g = Array.ofDim[Int](n, n)
    for (i <- 0 until n; j <- i until n) {
      if (i == j) g(i)(i) = -1
      else {
        g(i)(j) = Random.between(1, 20).abs
        g(j)(i) = g(i)(j)
      }
    }
    g
  }

  def createCompleteWeightGraphPointsBased(n: Int): Array[Array[Int]] = {
    val g = Array.ofDim[Int](n, n)
    val points = Array.ofDim[(Int, Int)](n)
    val dist = (p1: (Int, Int), p2: (Int, Int)) => Math.sqrt(
      (p1._1 - p2._1) * (p1._1 - p2._1) +
        (p1._2 - p2._2) * (p1._2 - p2._2))
    for (i <- points.indices)
      points(i) = (Random.between(1, 200), Random.between(1, 200))

    for (i <- 0 until n; j <- i until n) {
      if (i == j) g(i)(i) = -1
      else {
        g(i)(j) = dist(points(i), points(j)).toInt + 1
        g(j)(i) = g(i)(j)
      }
    }
    g
  }

  def randomCycle(g: Array[Array[Int]]): Array[Int] = {
    val child = Array.ofDim[Int](g.length)
    for (i <- child.indices)
      child(i) = i
    Random.shuffle(child).toArray
  }

  def dist(g: Array[Array[Int]], child: Array[Int]): Int = {
    var sum = 0
    for (i <- 1 until child.length)
      sum += g(child(i))(child(i - 1))
    sum
  }

  val g = createCompleteWeightGraphPointsBased(3)
  for (i <- g.indices) {
    for (j <- g.indices) {
      print(g(i)(j) + " ")
    }
    println()
  }
}

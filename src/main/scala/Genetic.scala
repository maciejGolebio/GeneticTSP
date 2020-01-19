import scala.collection.mutable
import scala.util.Random

object Genetic extends App {

  //todo 2N to N
  private def edgeTable(p1: Array[Int], p2: Array[Int]): Array[collection.mutable.Set[(Int, Boolean)]] = {
    val edgesTable = Array.ofDim[mutable.Set[(Int, Boolean)]](p1.length)
    for (i <- edgesTable.indices)
      edgesTable(i) = mutable.Set()
    for (i <- p1.indices) {
      if (i > p1.length - 2) {
        edgesTable(p1(i)).add((p1(0), false))
        edgesTable(p1(0)).add((p1(i), false))
      } else {
        edgesTable(p1(i)).add((p1(i + 1), false))
        edgesTable(p1(i + 1)).add((p1(i), false))
      }
    }
    for (i <- p2.indices) {
      if (i > p1.length - 2) {
        if (edgesTable(p2(i)) contains(p2(0), false)) {
          edgesTable(p2(i)).remove((p2(0), false))
          edgesTable(p2(0)).remove((p2(i), false))
          edgesTable(p2(0)).add((p2(i), true))
          edgesTable(p2(i)).add((p2(0), true))
        } else {
          edgesTable(p2(0)).add((p2(i), false))
          edgesTable(p2(i)).add((p2(0), false))
        }
      } else {
        if (edgesTable(p2(i)) contains(p2(i + 1), false)) {
          edgesTable(p2(i)).remove((p2(i + 1), false))
          edgesTable(p2(i + 1)).remove((p2(i), false))
          edgesTable(p2(i + 1)).add((p2(i), true))
          edgesTable(p2(i)).add((p2(i + 1), true))
        } else {
          edgesTable(p2(i + 1)).add((p2(i), false))
          edgesTable(p2(i)).add((p2(i + 1), false))
        }
      }
    }
    /*  for (i <- edgesTable.indices) {
        print(i + ":  ")
        edgesTable(i).map(x => if (x._2) {
          x._1.toString + "+ "
        } else x._1.toString + " ").foreach(print)
        println()
      }*/
    edgesTable
  }

  /*
    Edge Crossover
    child has parents common connection
    @return child of p1 and p2
   */
  def crossover(p1: Array[Int], p2: Array[Int]): Array[Int] = {
    val c = Array.ofDim[Int](p1.length)
    var edgesTable = edgeTable(p1, p2)
    var k = Random.between(0, c.length)
    // for (i <- c.indices) {
    c(0) = k
    for (i <- 0 until c.length - 1) {
      edgesTable = edgesTable.map(_.filter(x => x._1 != k))
      if (edgesTable(k).isEmpty) {
        val v: Vector[Int] = edgesTable.map(_.map(_._1)).flatten.toSet.toVector
        k = v(Random.between(0, v.size))
      } else {
        val x = edgesTable(k).filter(_._2 == true).map(_._1)
        if (x.nonEmpty) {
          k = x.toVector(Random.between(0, x.size))
        } else {
          k = edgesTable(k).map(x => (edgesTable(x._1), x._1)).minBy(_._1.size)._2
          val l = edgesTable(k).map(x => (edgesTable(x._1), x._1)).filter(_._1.size == k)
          if (l.size > 1)
            k = l.toVector(Random.between(0, l.size))._2
        }
      }
      c(i + 1) = k
    }
    //c.foreach(println)
    c
  }

  def mutation(p1: Array[Int]): Array[Int] = {
    val mut = p1.clone()
    val swap = (x1: Int, x2: Int) => {
      mut(x1) = p1(x2)
      mut(x2) = p1(x1)
      mut
    }
    swap(Random.between(0, p1.length), Random.between(0, p1.length))
  }

  def selection(population: List[Array[Int]], graph: Array[Array[Int]]): List[(Array[Int], Double)] = {
    import Graph.dist
    val p = population.map(x => (x, 0.001 / dist(graph, x)))
    val sum = p.map(_._2).sum
    p.map(x => (x._1, (x._2 / sum) * 360))
  }


}

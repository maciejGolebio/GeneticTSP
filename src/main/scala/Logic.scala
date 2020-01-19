import scala.language.postfixOps
object Logic extends App {
  val n: Int = 20
  val epocs: Int = 30

  val graph = Graph.createCompleteWeightGraphPointsBased(n)
  var population: collection.mutable.Set[Array[Int]] = collection.mutable.Set.empty

  for (_ <- 0 to n) {
    population.add(Graph.randomCycle(graph))
  }

  for (_ <- 0 to epocs) {
    population = Genetic.selection(population, graph)
  }

  var min = Int.MaxValue
  var tmp = 0
  val p = population toArray
  var best: Array[Int] = p(0)
  for (i <- p.indices) {
    tmp = Graph.dist(graph, p(i))
    if (tmp < min) {
      min = tmp;
      best = p(i)
    }
  }
  print(min)
}

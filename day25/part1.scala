import io.Source
import scala.util.Random

object Part1 {

    def main(args: Array[String]): Unit = 
        val graph = parseInput(args(0))
        println(findCut(graph, desiredCutSize=3))

    
    def findCut(graph: Map[String, Seq[String]], desiredCutSize: Int): Int =
        karger(graph, desiredCutSize).getOrElse(findCut(graph, desiredCutSize)) // Karger has a high probability to find the cut. We iterate until it does

    def karger(graph: Map[String, Seq[String]], desiredCutSize: Int): Option[Int] =
        def helper(sizes: Map[String, Int], ref: Map[String, String], graph: Map[String, Seq[String]]): (Map[String, Int], Map[String, Seq[String]]) = 
            if graph.keySet.size == 2 then 
                (sizes, graph) 
            else
                val nodes = graph.keySet.toVector
                val from = ref(nodes(Random.nextInt(nodes.size)))
                val tos = graph(from).toVector
                val to = ref(tos(Random.nextInt(tos.size)))
                val mergedFromNeighbors = (graph(from) ++ graph(to)).map(ref).filterNot(Set(from, to))

                val newSizes = sizes.updated(from, sizes(from) + sizes(to)) - to
                val newGraph = graph.updated(from, mergedFromNeighbors) - to
                val newRef = ref.map((original, ref) => if ref == to then original -> from else original -> ref)
                helper(newSizes, newRef, newGraph)

        val (sizes, contractedGraph) = helper(graph.keySet.map(_ -> 1).toMap, graph.keySet.map(n => n->n).toMap, graph)
        Option.when(contractedGraph.values.forall(_.size == desiredCutSize))(sizes.values.reduce(_ * _))

    def parseInput(s: String): Map[String, Seq[String]] =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        import scala.collection.mutable.HashMap as HMap
        val graph = HMap[String, Seq[String]]()
        lines.foreach(line =>
            line.split(": ") match
                case Array(from, tos) => tos.split(" ").foreach(to =>
                    graph.put(from, to +: graph.getOrElse(from, Seq()))
                    graph.put(to, from +: graph.getOrElse(to, Seq())) 
                )
                case _ => throw AssertionError()
        )
        graph.toMap
}
//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "immix"

import io.Source
import collection.mutable.PriorityQueue
import collection.mutable.Map as HMap

object Part2 {

    def main(args: Array[String]): Unit = 
        val (instructions, graph) = parseInput(args(0))
        val startingNodes = graph.findStartingNodes(s => s.last == 'A')
        val pq: PriorityQueue[AbsoluteNodeState] = PriorityQueue[AbsoluteNodeState]()(Ordering.by(-_.distance))
        pq.addAll(startingNodes)
        val cache: HMap[RelativeNodeState, RelativeNodeState] = HMap()
        println(findLength(pq, 'Z', graph, instructions, cache))
        
        

    def findLength(currentNodes: PriorityQueue[AbsoluteNodeState], goalNodeIdLetter: Char, graph: Graph, instructions: Instructions, cache: HMap[RelativeNodeState, RelativeNodeState], started: Boolean=false): Long =
        // The basic idea is the following:
        // If all nodes are at the same distance, then return the distance.
        // Otherwise, find the least advanced node and move it to its next target node.
        // We use a Priority Queue to efficiently insert the new node and retrieve the least advanced node
        val target = currentNodes.head.distance
        if started && currentNodes.toSeq.forall(_.distance == target) then
            target
        else
            val smallestNode = currentNodes.dequeue()
            val nodeState = RelativeNodeState(smallestNode.id, smallestNode.distance % instructions.size)
            val updatedRelativeNode = cache.get(nodeState).getOrElse(findNextGoalRelativeNode(smallestNode, goalNodeIdLetter, graph, instructions))
            cache.put(nodeState, updatedRelativeNode)
            val updatedNodeAbsolute = updatedRelativeNode.toAbsolute(smallestNode.distance)
            currentNodes.enqueue(updatedNodeAbsolute)
            findLength(currentNodes, goalNodeIdLetter, graph, instructions, cache, started = true)
            

    def findNextGoalRelativeNode(initialNodeState: AbsoluteNodeState, goalNodeIdLetter: Char, graph: Graph, instructions: Instructions, started: Boolean=false): RelativeNodeState =
        def helper(currentNodeState: AbsoluteNodeState, started: Boolean): RelativeNodeState =
            if started && currentNodeState.id.last == goalNodeIdLetter then 
                currentNodeState.toRelative(initialNodeState.distance) // return relative representation for caching correctly the states
            else 
                helper(AbsoluteNodeState(graph.move(currentNodeState.id, instructions(currentNodeState.distance)), currentNodeState.distance + 1), true)
        helper(initialNodeState, started)


    def parseInput(s: String): (Instructions, Graph) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq
        val instruction = lines.head.trim() 
        val nodePattern = "([0-9A-Z]+) = [(]([0-9A-Z]+), ([0-9A-Z]+)[)]".r // eg. AAA = (BBB, CCC)
        val nodes =  
            for pm <- nodePattern.findAllMatchIn(lines.tail.mkString("\n"))
            yield Node(pm.group(1), pm.group(2), pm.group(3))

        (Instructions(instruction), Graph(nodes))


    // Alias type for String
    type NodeId = String


    // Make the difference very explicitely between absolute and relative states of the node. 
    // Absolute meaning the total distance we currently are, and Relative means where in the instruction set we are.
    case class RelativeNodeState(id: NodeId, distance: Long):
        def toAbsolute(d: Long): AbsoluteNodeState = AbsoluteNodeState(id, distance + d)

    case class AbsoluteNodeState(id: NodeId, distance: Long):
        def toRelative(d: Long): RelativeNodeState = RelativeNodeState(id, distance - d)     

    case class Node(id: NodeId, leftId: NodeId, rightId: NodeId):
        def move(instruction: Char) = if instruction == 'L' then leftId else rightId

    // Abstraction of how to interract with nodes
    case class Graph(private val nodes: Iterator[Node]):
        private val mapNodes = nodes.map(n => n.id -> n).toMap

        def move(nodeId: NodeId, instruction: Char): NodeId = mapNodes(nodeId).move(instruction)
        def findStartingNodes(startingNodeCondition: NodeId => Boolean): Seq[AbsoluteNodeState] = mapNodes.map(_._1).filter(startingNodeCondition).map(AbsoluteNodeState(_, 0L)).toSeq

    // It's just a simple wrapper of the instruction string, making it safe to interact with using the modulo operator
    case class Instructions(s: String):
        val vector = s.toVector
        val size = vector.size
        def apply(i: Long): Char = vector((i % size).toInt)
        

}
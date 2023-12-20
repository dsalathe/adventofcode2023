import io.Source
import scala.collection.immutable.Queue

object Part2 {

    def main(args: Array[String]): Unit =
        val (reversedGraph, modules) = parseInput(args(0))
        val rxNeighbors = reversedGraph("rx").toSet
        val targets = rxNeighbors.flatMap(node => reversedGraph(node)) // Here we unfortunately assume rx's neighbors are conjunctions
        val targetValues = pushButton(rxNeighbors, targets, modules)
        println(targetValues.map(tv => BigInt(tv._2)).reduce(lcm))

    def lcm(a: BigInt, b: BigInt) = a * b / a.gcd(b)


    def pushButton(rxNeighbors: Set[String], targets: Set[String], modules: Map[String, Module], count: Int=1, targetsValues: Map[String, Int]=Map.empty): Map[String, Int] =
        def helper(modules: Map[String, Module], q: Queue[(String, String, Pulse)], targetsValues: Map[String, Int]): (Map[String, Module], Map[String, Int]) =
            if q.isEmpty || targetsValues.size == targets.size then
                (modules, targetsValues) 
            else
                val ((sentId, destId, pulse), dq) = q.dequeue
                if targets.contains(sentId) && rxNeighbors.contains(destId) && pulse == Pulse.HIGH then
                    helper(modules, dq, if targetsValues.contains(sentId) then targetsValues else targetsValues + (sentId -> count))
                else if !modules.contains(destId) then
                    helper(modules, dq, targetsValues)
                else modules(destId) match
                    case f @ FliFlop(id, state, destinations) => pulse match
                        case Pulse.LOW => 
                            val newFlipFlop = f.copy(state = state.switch())
                            val sentPulse = if state == State.OFF then Pulse.HIGH else Pulse.LOW
                            val newQ = dq.enqueueAll(destinations.map((id, _, sentPulse)))
                            val newModules = modules.updated(id, newFlipFlop)
                            helper(newModules, newQ, targetsValues)
                        case Pulse.HIGH => helper(modules, dq, targetsValues)
                    case Broadcaster(id, destinations) => 
                        helper(modules, dq.enqueueAll(destinations.map((id, _, pulse))), targetsValues)
                    case c @ Conjunction(id, pulses, destinations) => 
                        val newPulses = pulses.updated(sentId, pulse)
                        val sentPulse = if newPulses.forall{case sentId -> pulse => pulse == Pulse.HIGH} then Pulse.LOW else Pulse.HIGH
                        val newModules = modules.updated(id, c.copy(pulses = newPulses))
                        val newQ = dq.enqueueAll(destinations.map((id, _, sentPulse)))
                        helper(newModules, newQ, targetsValues)
        
        val (newModules, newTargetsValues) = helper(modules, 
                Queue().enqueueAll(modules("broadcaster").asInstanceOf[Broadcaster].destinations.map(("broadcaster", _, Pulse.LOW))),
                targetsValues)
        if newTargetsValues.size == targets.size then 
            newTargetsValues
        else
            pushButton(rxNeighbors, targets, newModules, count + 1, newTargetsValues)
                
            

    
    enum State:
        case ON, OFF
        def switch(): State = if this == ON then OFF else ON
    enum Pulse:
        case LOW, HIGH
    
    sealed trait Module
    case class FliFlop(id: String, state: State, destinations: Seq[String]) extends Module
    case class Broadcaster(id: String, destinations: Seq[String]) extends Module
    case class Conjunction(id: String, pulses: Map[String, Pulse], destinations: Seq[String]) extends Module

    def parseInput(s: String): (Map[String, Seq[String]], Map[String, Module]) =
        val lines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val unconnectedMap = lines.map (line => 
            line.split(" -> ") match
                case Array(module, rawDestinations) => 
                    val destinations = rawDestinations.split(", ").toSeq
                    module match
                        case "broadcaster" => "broadcaster" -> Broadcaster("broadcaster", destinations)
                        case id if id.startsWith("%") => id.tail -> FliFlop(id.tail, State.OFF, destinations)
                        case id if id.startsWith("&") => id.tail -> Conjunction(id.tail, Map.empty[String, Pulse], destinations)
                        case _ => throw AssertionError()
                case _ => throw AssertionError()
        ).toMap
        val inbounds: Seq[(String, String)] = unconnectedMap.toSeq.flatMap {case id -> module => module match
            case FliFlop(id, _, destinations) =>  destinations.map(d => (id, d))
            case Conjunction(id, pulses, destinations) => destinations.map(d => (id, d))
            case Broadcaster(id, destinations) => destinations.map(d => (id, d))
        }
        val reversedGraph = inbounds.groupBy{case (from, to) => to}.view.mapValues(values => values.map{case (from, to) => from}).toMap
        (reversedGraph, inbounds.foldLeft(unconnectedMap) {case (m, inboundId -> destId) => if !m.contains(destId) then m else m(destId) match
            case c @ Conjunction(id, pulses, destinations) => m.updated(destId, c.copy(pulses = pulses + (inboundId -> Pulse.LOW)))
            case _ => m            
        })
}
import io.Source
import scala.collection.immutable.Queue

object Part1 {

    def main(args: Array[String]): Unit = 
        val modules = parseInput(args(0))
        val (low, high) = pushButton(modules, 1000)
        println(low * high)


    def pushButton(modules: Map[String, Module], nTimes: Int, low: Long=0, high: Long=0): (Long, Long) =
        def helper(modules: Map[String, Module], q: Queue[(String, String, Pulse)], low: Long, high: Long): (Map[String, Module], Long, Long) =
            if q.isEmpty then
                (modules, low, high) 
            else
                val ((sentId, destId, pulse), dq) = q.dequeue
                if !modules.contains(destId) then
                    helper(modules, dq, if pulse == Pulse.LOW then low + 1 else low, if pulse == Pulse.HIGH then high + 1 else high)
                else modules(destId) match
                    case f @ FliFlop(id, state, destinations) => pulse match
                        case Pulse.LOW => 
                            val newFlipFlop = f.copy(state = state.switch())
                            val sentPulse = if state == State.OFF then Pulse.HIGH else Pulse.LOW
                            val newQ = dq.enqueueAll(destinations.map((id, _, sentPulse)))
                            val newModules = modules.updated(id, newFlipFlop)
                            helper(newModules, newQ, low + 1, high)
                        case Pulse.HIGH => helper(modules, dq, low, high + 1)
                    case Broadcaster(id, destinations) => 
                        helper(modules, dq.enqueueAll(destinations.map((id, _, pulse))), 
                            if pulse == Pulse.LOW then low + 1 else low, if pulse == Pulse.HIGH then high + 1 else high)
                    case c @ Conjunction(id, pulses, destinations) => 
                        val newPulses = pulses.updated(sentId, pulse)
                        val sentPulse = if newPulses.forall{case sentId -> pulse => pulse == Pulse.HIGH} then Pulse.LOW else Pulse.HIGH
                        val newModules = modules.updated(id, c.copy(pulses = newPulses))
                        val newQ = dq.enqueueAll(destinations.map((id, _, sentPulse)))
                        helper(newModules, newQ, 
                            if pulse == Pulse.LOW then low + 1 else low, if pulse == Pulse.HIGH then high + 1 else high)
        
        if nTimes == 0 then 
            (low, high)
        else
            val (newModules, newLow, newHigh) = helper(modules, 
                Queue().enqueueAll(modules("broadcaster").asInstanceOf[Broadcaster].destinations.map(("broadcaster", _, Pulse.LOW)) ),
                1L + low, high)
            pushButton(newModules, nTimes-1, newLow, newHigh)
                
            

    enum State:
        case ON, OFF
        def switch(): State = if this == ON then OFF else ON
    enum Pulse:
        case LOW, HIGH
    
    sealed trait Module
    case class FliFlop(id: String, state: State, destinations: Seq[String]) extends Module
    case class Broadcaster(id: String, destinations: Seq[String]) extends Module
    case class Conjunction(id: String, pulses: Map[String, Pulse], destinations: Seq[String]) extends Module

    def parseInput(s: String): Map[String, Module] =
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
        inbounds.foldLeft(unconnectedMap) {case (m, inboundId -> destId) => if !m.contains(destId) then m else m(destId) match
            case c @ Conjunction(id, pulses, destinations) => m.updated(destId, c.copy(pulses = pulses + (inboundId -> Pulse.LOW)))
            case _ => m            
        }
}
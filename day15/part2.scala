import io.Source
import scala.collection.mutable.ArrayBuffer

object Part2 {
    val HASH_SIZE = 256

    def main(args: Array[String]): Unit = 
        val instructions = parseInput(args(0))
        val boxes = fillBoxes(instructions)
        println(countPower(boxes))

    def countPower(boxes: ArrayBuffer[ArrayBuffer[(String, Int)]]): Int =
        boxes.zipWithIndex.map{case (b, i) => if b.isEmpty then 0 else (i+1) * b.zipWithIndex.map{case ((_, l), j) => (j+1) * l}.sum  }.sum


    def fillBoxes(instructions: Seq[Instruction]): ArrayBuffer[ArrayBuffer[(String, Int)]] =
        val boxes = ArrayBuffer.fill(HASH_SIZE)(ArrayBuffer[(String, Int)]())
        instructions foreach {instruction =>
            val hashed = hash(instruction.id)
            val box = boxes(hashed)
            val idx = box.zipWithIndex.find{ case ((id, value), i) => id == instruction.id}.map(_._2)
            if instruction.operation == '=' then
                if idx.isDefined then
                    box(idx.get) = (instruction.id, instruction.value)
                else
                    boxes(hashed) = box.appended((instruction.id, instruction.value))
            else
                if idx.isDefined then
                    box.remove(idx.get)
        }
        boxes

    def hash(s: String): Int =
        s.foldLeft(0)((v, c) => ((c.toInt + v) * 17) % HASH_SIZE)

    case class Instruction(id: String, operation: Char, value: Int)

    def parseInput(s: String): Seq[Instruction] =
        Source.fromFile(s).getLines().filter(!_.isBlank()).toSeq.head
            .split(",").toSeq
            .map(s => Instruction(s.filter(_.isLetter).toString(), if s.contains("=") then '=' else '-', s.filter(_.isDigit).toIntOption.getOrElse(0)))
        
}
import io.Source

object Part2 {

    def main(args: Array[String]): Unit = 
        val workflows = parseInput(args(0))
        println(computeScore(computePart(workflows)))

    enum Category:
        case /*MERRY*/ X,M,A,S
    
    case class Workflow(id: String, conditions: Seq[Condition], default: String)
    case class Range(low: Int, high: Int)
    type PartRange = Map[Category, Range]
    case class Condition(category: Category, cmp: Char, value: Int, nextId: String):
        def eval(partRange: PartRange): Seq[(PartRange, Option[String])] = 
            val newRange = 
                if cmp == '<' then 
                    Seq((Range(partRange(category).low, value-1), Some(nextId)), (Range(value, partRange(category).high), Option.empty[String]))
                else
                    Seq((Range(partRange(category).low, value), Option.empty[String]), (Range(value+1, partRange(category).high), Some(nextId)))
            newRange.filter{case (range, nextId) => range.low < range.high}.map{case (range, nextId) => (partRange.updated(category, range), nextId)}


    def computeScore(ranges: Seq[PartRange]): Long =
        ranges.map(r =>  r.values.map{case Range(low, high) => high - low + 1L}.reduce(_ * _)).sum

    def computePart(workflows: Map[String, Workflow]): Seq[PartRange] =
        def receive(partRange: PartRange, id: String): Seq[PartRange] =
            val workflow = workflows(id)
            val nexts: Seq[(PartRange, Option[String])] = workflow.conditions.foldLeft(Seq((partRange, Option.empty[String])))((result, condition) =>
                result.flatMap {case (partRange, nextId) => nextId match
                    case Some(id) => Seq((partRange, Some(id)))
                    case None => condition.eval(partRange)
                }
            )
            nexts.flatMap { case (partRange, oNextId) =>
                val nextId = oNextId.getOrElse(workflow.default)
                if nextId == "A" then 
                    Some(partRange)
                else if nextId == "R" then
                    None
                else 
                    receive(partRange, nextId)
            }

        val partRange = Category.values.map(cat => cat -> Range(1, 4000)).toMap
        receive(partRange, "in")

    def parseInput(s: String): Map[String, Workflow] =
        val allLines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val (workflowLines, _) = allLines.partition(p => p.head != '{')
        val workflows = workflowLines.map { line =>
            val id = line.takeWhile(_ != '{')
            val remaining = line.dropWhile(_ != '{').tail.dropRight(1).split(",")
            val rawConditions = remaining.dropRight(1)
            val default = remaining.last
            val conditions = rawConditions.map { rawCondition =>
                val category = rawCondition.head
                val cmp = rawCondition(1)
                val valueNextId = rawCondition.drop(2).split(":")
                val value = valueNextId.head.toInt
                val nextId = valueNextId(1)
                Condition(Category.valueOf(category.toString().toUpperCase()), cmp, value, nextId)
            }.toSeq
            id -> Workflow(id, conditions, default)
        }.toMap
        workflows
}
import io.Source

object Part1 {

    def main(args: Array[String]): Unit = 
        val (workflows, parts) = parseInput(args(0))
        println(parts.map(p => computePart(workflows, p)).sum)

    enum Category:
        case /*MERRY*/ X,M,A,S

    case class Workflow(id: String, conditions: Seq[Condition], default: String)
    type Part = Map[Category, Int]
    case class Condition(category: Category, cmp: Char, value: Int, nextId: String):
        private val compare: (Int, Int) => Boolean = if cmp == '<' then (m: Int, n: Int) => m < n else (m: Int, n: Int) => m > n
        def eval(part: Part): Option[String] = 
            val partValue = part(category)
            Option.when(compare(partValue, value))(nextId)
            

    def computePart(workflows: Map[String, Workflow], part: Part): Int =
        def receive(id: String): Int =
            val workflow = workflows(id)
            val nextId = workflow.conditions.foldLeft(Option.empty[String])((result, condition) => result.orElse(condition.eval(part))).getOrElse(workflow.default)
            if nextId == "A" then part.values.sum else if nextId == "R" then 0 else receive(nextId)

        receive("in")

    def parseInput(s: String): (Map[String, Workflow], Seq[Part]) =
        val allLines = Source.fromFile(s).getLines().filter(!_.isBlank())
        val (workflowLines, partsLines) = allLines.partition(p => p.head != '{')
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
        val parts = partsLines.map { line =>
            val rawParts = line.tail.dropRight(1).split(",").map(_.drop(2).toInt)
            (Category.values zip rawParts).toMap
        }.toSeq
        (workflows, parts)
}
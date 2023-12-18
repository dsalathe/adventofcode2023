//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

@main def main(filename: String): Unit = 
    val lines = scala.io.Source.fromFile(filename).getLines().filter(!_.isBlank())
    val res = lines.map(line => findFirstDigit(line) * 10 + findFirstDigit(line.reverse))
        .sum
    println(res)

def findFirstDigit(s: String): Int =
    s.find(_.isDigit).map(_.asDigit).get // expects an error if not found

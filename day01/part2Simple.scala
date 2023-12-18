//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

@main def main2(filename: String): Unit = 
    val lines = scala.io.Source.fromFile(filename).getLines().filter(!_.isBlank()).map(sanizizeNumbers(_))
    val res = lines.map(line => line(0).asDigit * 10 + line.last.asDigit)
        .sum
    println(res)

def sanizizeNumbers(s: String): String =
    s.replace("one", "one1one")
    .replace("two", "two2two")
    .replace("three", "three3three")
    .replace("four", "four4four")
    .replace("five", "five5five")
    .replace("six", "six6six")
    .replace("seven", "seven7seven")
    .replace("eight", "eight8eight")
    .replace("nine", "nine9nine")
    .filter(_.isDigit)


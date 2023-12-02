//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

import io.Source

object Part1 {

  case class RGB(red: Int, green: Int, blue: Int)

  val RGB_PARAMS: RGB = RGB(12, 13, 14)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile(args(0)).getLines().filter(!_.isBlank())
    println(
      lines.map(toRGBs)
        .map(rgbs => rgbs.forall(rgb => lowerOrEqThan(rgb, RGB_PARAMS)))
        .zipWithIndex
        .map((isLower, index) => if isLower then index + 1 else 0)
        .sum)
  }

  def toRGBs(s: String): Seq[RGB] =
    s.split(":")(1).trim().split("; ").map(toRGB).toSeq

  def toRGB(s: String): RGB = {
    val colors = s.split(", ").toSeq
    val red = extractColor(colors, "red")
    val green = extractColor(colors, "green")
    val blue = extractColor(colors, "blue")
    RGB(red, green, blue)
  }

  def extractColor(colors: Seq[String], color: String): Int =
    colors.find(_.contains(color)).map(l => l.filter(_.isDigit).toInt).getOrElse(0)

  def lowerOrEqThan(rgb1: RGB, rgb2: RGB): Boolean =
    rgb1.red <= rgb2.red && rgb1.green <= rgb2.green && rgb1.blue <= rgb2.blue
}
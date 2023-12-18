//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"

import io.Source
import scala.math.max

object Part2 {

  case class RGB(red: Int, green: Int, blue: Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile(args(0)).getLines().filter(!_.isBlank())
    println(
      lines.map(toRGBs)
        .map(rgbs => rgbs.foldLeft(RGB(0, 0, 0))(maxRGB))
        .map(rgb => rgb.red * rgb.green * rgb.blue)
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

  def maxRGB(rgb1: RGB, rgb2: RGB): RGB =
    RGB(max(rgb1.red, rgb2.red), max(rgb1.green, rgb2.green), max(rgb1.blue, rgb2.blue))
}
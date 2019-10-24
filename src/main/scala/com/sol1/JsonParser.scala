package com.sol1

//import cats.effect.{Sync, IO}
//import cats.implicits.{toFoldableOps => _, _}
//import io.circe.generic.auto._
//import org.http4s.{EntityDecoder, HttpRoutes}
//import org.http4s.circe.{jsonEncoder, jsonOf}
import net.liftweb.json.DefaultFormats
import net.liftweb.json._

class JsonParser {
  implicit val formats = DefaultFormats

  case class InpData(points: Int)
  val source = scala.io.Source.fromFile("inp.txt")

  test

  def test = {
    val lines: String = try source.mkString finally source.close()
    val json = parse(lines)
    val elements = (json \\ "points").children
    for (acct <- elements) {
      val m = acct.extract[InpData]
      println(" Array: " + m.points)
    }
  }

}

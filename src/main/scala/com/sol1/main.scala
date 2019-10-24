package com.sol1

import net.liftweb.json.DefaultFormats
import net.liftweb.json._
import scala.math._
//import cats._
//import cats.implicits._

object main extends App {
  implicit val formats = DefaultFormats

  val source = scala.io.Source.fromFile("inp.txt")

  //println(readJson)
  //println(toLine())
  //  for {
  //    lst <- readJson
  //    _ <- toLine(lst)
  //  }
  //val x = readJson
  //val y = circuitList(x)
  //val z = toLine(y)
//  println(z)
//  calculateAngle(z)

  val lines = for{
    lst <- readJson
    circLst <- circuitList(lst)
    lines <- toLine(circLst)
  } yield (lines)

  print(lines)




  //  for {
  //    lst <- readJson
  //    circLst <- circuitList(lst)
  //  }

  def readJson: Either[String, List[(Double, Double)]] = {
    val lines: String = try source.mkString finally source.close()
    //    val json = parse(lines)
    //    val elements = (json).children
    //    val m = elements map(x => x.extract[List[List[Double]]])
    //println(m flatMap (x => x flatMap (y => List((y(0), y(1))))))
    Right(parse(lines).
      children.
      map(x => x.extract[List[List[Double]]]).
      flatMap(y =>
        y flatMap (z =>
          List((z(0), z(1))))
      ))
    //m flatMap (x => x flatMap (y => List((y(0), y(1)))))
  }

  def circuitList(list: List[(Double, Double)]):Either[String, List[(Double, Double)]] = {
    Right(list :+ (list.head))
  }

  def toLine(list: List[(Double, Double)]): Either[String, List[List[(Double, Double)]]] = {
    Right(list.sliding(2).toList)
  }

  //  double get_ang (const pt & a, const pt & b) {
  //    double ang = abs (atan2 (a.y, a.x) - atan2 (b.y, b.x));
  //    return min (ang, 2*PI-ang);
  //  }
  def calculateAngle(list: List[List[(Double, Double)]]) = {
    val cortege = list.map(x =>
      (x(0),
        x(1),
        sqrt(pow((x(0)._1 - x(1)._1), 2) + pow((x(0)._2 - x(1)._2), 2))
      ))

    println(cortege)
  }

  //  for {
  //    lst <- readJson
  //    rad <-
  //  } yield rad
}

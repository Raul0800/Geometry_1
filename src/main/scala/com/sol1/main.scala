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

  mainLoop()

  def readData: Either[String, List[(Double, Double)]] = {
    val lines: String = try source.mkString finally source.close()
    //    val json = parse(lines)
    //    val elements = (json).children
    //    val m = elements map(x => x.extract[List[List[Double]]])
    //println(m flatMap (x => x flatMap (y => List((y(0), y(1))))))
    println(parse(lines).
      children.
      map(x => x.extract[List[List[Double]]]).
      flatMap(y =>
        y flatMap (z =>
          List((z(0), z(1))))
      ))
    Right(parse(lines).
      children.
      map(x => x.extract[List[List[Double]]]).
      flatMap(y =>
        y flatMap (z =>
          List((z(0), z(1))))
      ))
    //m flatMap (x => x flatMap (y => List((y(0), y(1)))))
  }

  def circuitList(list: List[(Double, Double)]): Either[String, List[(Double, Double)]] = {
    println(list :+ (list.head))
    Right(list :+ (list.head))
  }

  def toLine(list: List[(Double, Double)]): Either[String, List[List[(Double, Double)]]] = {
    println(list.sliding(2).toList)
    Right(list.sliding(2).toList)
  }

  def calculateLenght(list: List[List[(Double, Double)]]): Either[String,
    List[((Double, Double), (Double, Double), Double)]] = {
    val cortege = list.map(x =>
      (x(0),
        x(1),
        sqrt(pow((x(0)._1 - x(1)._1), 2) + pow((x(0)._2 - x(1)._2), 2))
      ))
    println(cortege)
    Right(cortege)
  }

  def calculateAngle(list: List[((Double, Double), (Double, Double), Double)]):
  Either[String, (List[((Double, Double), (Double, Double), Double)], List[Double])] = {
    val angels = (list :+ list.head).sliding(2).toList.map(x =>
      (x(0), x(1)) match {
        case (a, b) => {
          val c = sqrt(pow(a._1._1 - b._2._1, 2) + pow(a._1._2 - b._2._2, 2))
          acos((pow(a._3, 2) + pow(b._3, 2) - pow(c, 2))
            / (2 * a._3 * b._3))
        }
      })
    println(list /* :+ list.head*/ , angels)
    Right(list /* :+ list.head*/ , angels)
  }

  def calculateHigth(pair: (List[((Double, Double), (Double, Double), Double)], List[Double])):
  Either[String, (List[((Double, Double), (Double, Double), Double)], List[Double], List[Double])] = {
    Right(pair._1, pair._2, pair._1.map(x => {
      val iter1 = pair._1.indexOf(x)
      val iter2 = if (iter1 == 0) pair._2.size - 1 else if (iter1 == pair._2.size - 1) 0 else iter1 + 1

      (x._3, pair._2(iter1), pair._2(iter2)) match {
        case (l, alph, bet) => {
          println(l, alph, bet)
          l * (sin(alph / 2) * sin(bet / 2)) / sin(alph / 2 + bet / 2)
        }
      }
    })
    )
  }

  def mainLoop():
  Either[String, (List[((Double, Double), (Double, Double), Double)], List[Double], List[Double])] = {
    val tuple3 = for {
      lst <- readData
      circLst <- circuitList(lst)
      lines <- toLine(circLst)
      cortege <- calculateLenght(lines)
      pair <- calculateAngle(cortege)
      tuple3 <- calculateHigth(pair)
    } yield (tuple3)

    println(tuple3)

    val rTuple3 = tuple3.right.get

    //
    //some logic
    //
    if (rTuple3._3.size > 2)
      mainLoop()
    else
      tuple3
  }
}

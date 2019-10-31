package com.sol1

import net.liftweb.json.DefaultFormats
import net.liftweb.json._

import scala.annotation.tailrec
import scala.math._

object main extends App {
  implicit val formats = DefaultFormats

  val source = scala.io.Source.fromFile("inp.txt")

  val results = prepareData match {
    case Right(value) => (mainLoop(value, List.empty)
    //case Left(value) => (null, value)
  }

  //  println("Answer or error: " + results)
  val fLst = checkCrossing(results._2, results._1._3, results._1._2)
  writeAnswer(results._1._2, fLst)

  def writeAnswer(radius: Double, lines: List[(List[(Double, Double)], Int)]): Unit = {
    println("Radius: " + radius)
    print("Indices: ")
    lines.map(x => x._2).take(3).foreach(x => print(x + " "))
    println("\nLines: " + lines.map(x => x._1))
  }


  def checkCrossing(lines: List[List[(Double, Double)]], center: (Double, Double), radius: Double): List[(List[(Double, Double)], Int)] = {
    val eps = 1.0E-08;
    lines.map(x => (x, lines.indexOf(x))).filter(pair =>
      (pair._1) match {
        case (x) => {
          (x(0), x(1)) match {
            case (p1, p2) => {
              val mP = ((p1._1 + p2._1) / 2, (p1._2 + p2._2) / 2)
              if (sqrt(pow(mP._1 - center._1, 2) + pow(mP._2 - center._2, 2)) - radius < eps) true
              else false
            }
          }
        }
      })
  }

  def readData: Either[String, List[(Double, Double)]] = {
    val lines: String = try source.mkString finally source.close()
    //    println("readData: " + parse(lines).
    //      children.
    //      map(x => x.extract[List[List[Double]]]).
    //      flatMap(y =>
    //        y flatMap (z =>
    //          List((z(0), z(1))))
    //      ))
    Right(parse(lines).
      children.
      map(x => x.extract[List[List[Double]]]).
      flatMap(y =>
        y flatMap (z =>
          List((z(0), z(1)))))
    )
  }

  def circuitList(list: List[(Double, Double)]): Either[String, List[(Double, Double)]] = {
    //println("circuitList: " + (list :+ (list.head)))
    Right(list :+ (list.head))
  }

  def toLine(list: List[(Double, Double)]): Either[String, List[List[(Double, Double)]]] = {
    //println("toLine: " + list.sliding(2).toList)
    Right(list.sliding(2).toList)
  }

  def calculateLenght(list: List[List[(Double, Double)]]): Either[String,
    List[((Double, Double), (Double, Double), Double)]] = {
    val cortege = list.map(x =>
      (x(0),
        x(1),
        sqrt(pow((x(0)._1 - x(1)._1), 2) + pow((x(0)._2 - x(1)._2), 2))
      ))
    //println("calculateLenght: " + cortege)
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
    //println("calculateAngle: " + (list /* :+ list.head*/ , angels))
    Right(list /* :+ list.head*/ , angels)
  }

  def calculateHigth(pair: (List[((Double, Double), (Double, Double), Double)], List[Double])):
  Either[String, (List[((Double, Double), (Double, Double), Double)], List[Double], List[Double])] = {
    Right(pair._1, pair._2, pair._1.map(x => {
      val iter1 = pair._1.indexOf(x)
      val iter2 = if (iter1 == 0) pair._2.size - 1 else iter1 - 1 //if (iter1 == pair._2.size - 1) 0 else iter1 + 1

      (x._3, pair._2(iter1), pair._2(iter2)) match {
        case (l, alph, bet) => {
          //println("calculateHigth: " + (l, alph, bet) + ", X: " + x + ", iter1: " + iter1 + ", iter2: " + iter2)
          l * (sin(alph / 2) * sin(bet / 2)) / sin(alph / 2 + bet / 2)
        }
      }
    })
    )
  }

  def prepareData(): Either[String, List[(Double, Double)]] = {
    readData
  }

  @tailrec
  def mainLoop(lst: List[(Double, Double)], lines: List[List[(Double, Double)]]): ((Int, Double, (Double, Double)), List[List[(Double, Double)]]) = {
    val tuple3 = for {
      circLst <- circuitList(lst)
      lines <- toLine(circLst)
      cortege <- calculateLenght(lines)
      pair <- calculateAngle(cortege)
      tuple3 <- calculateHigth(pair)
    } yield (tuple3, lines)

    //println(tuple3)

    val rTuple3 = tuple3.right.get._1

    //ищем наименьший
    val minH = (rTuple3._3.indexOf(rTuple3._3.min), rTuple3._3.min, {
      val x2x1 = rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._1._1 - rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._2._1
      val y2y1 = rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._1._2 - rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._2._2
      val ab = pow(x2x1 * x2x1 + y2y1 * y2y1, 0.5)
      val v1x = -x2x1 / ab
      val v1y = -y2y1 / ab
      val v3x = (if (v1y > 0) -v1y else v1y) * rTuple3._3.min
      val v3y = (if (v1x > 0) -v1x else v1x) * rTuple3._3.min
      ((rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._1._1 + rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._2._1) / 2 + v3x,
        (rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._1._2 + rTuple3._1(rTuple3._3.indexOf(rTuple3._3.min))._2._2) / 2 + v3y)
    })
    //println("mainLoop: " + minH)
    rTuple3._3.size match {
      case (s) if s > 3 => {
        val indOfPointsToDel = minH._1 match {
          case (x) if x == rTuple3._3.size - 1 => (0, minH._1 - 1)
          case (x) if x == 0 => (rTuple3._3.size - 1, 1)
          case _ => (minH._1 - 1, minH._1 + 1)
        }
        val newLst = for {
          newX <- rebuildingPoints(rTuple3._1(indOfPointsToDel._1), rTuple3._1(indOfPointsToDel._2))
          newLst <- refreshList(lst, newX, minH._1, minH._1 + 1)
        } yield (newLst)

        newLst match {
          case Left(x) => (minH, if (lines.isEmpty) tuple3.right.get._2 else lines)
          case Right(x) => mainLoop(x, if (lines.isEmpty) tuple3.right.get._2 else lines)
        }
      }
      case _ => (minH, if (lines.isEmpty) tuple3.right.get._2 else lines)
    }
  }

  def rebuildingPoints(l1: ((Double, Double), (Double, Double), Double),
                       l2: ((Double, Double), (Double, Double), Double)): Either[String, (Double, Double)] = {
    val a1 = (l1._2._2 - l1._1._2) / (l1._2._1 - l1._1._1)
    val b1 = (l1._1._1 * l1._2._2 - l1._1._2 * l1._2._1) / (l1._2._1 - l1._1._1)

    val a2 = (l2._2._2 - l2._1._2) / (l2._2._1 - l2._1._1)
    val b2 = (l2._1._1 * l2._2._2 - l2._1._2 * l2._2._1) / (l2._2._1 - l2._1._1)

    ((b1 - b2) / (a1 - a2), a1 * (b1 - b2) / (a1 - a2) - b1) match {
      case (x) if (x._1.isNaN && x._2.isNaN) || (x._1.isInfinite && x._2.isInfinite) => Left("Parallel sides")
      case (x) => Right(x)
    }
  }

  def refreshList(lst: List[(Double, Double)], newP: (Double, Double), indToDel1: Int, indToDel2: Int):
  Either[String, List[(Double, Double)]] = {
    Right((lst.slice(0, indToDel1) ++
      List(newP) ++ lst.slice(indToDel1 + 1, lst.size))
      .filter(x => lst.indexOf(x) != indToDel2))
  }
}

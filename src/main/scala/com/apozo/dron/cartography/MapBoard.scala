package com.apozo.dron.cartography

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer

object MapBoard {
  val URB_CON = "Urb"
}

case class MapBoard(_wide: Int, _heigh: Int) {

  import MapBoard.URB_CON

  private def addUrbToMap: List[(Urbanization, Int, Int)] = {
    @tailrec
    def _add(index: Int, axisX: Int, axisY: Int, mapList: ListBuffer[(Urbanization, Int, Int)]): List[(Urbanization, Int, Int)] = {
      val middleRange: Int = (_wide + 1) / 2
      val maxRange = middleRange - 1
      (index, axisX, axisY) match {
        case (i, _, _) if i == (_wide * _heigh) + 1 => mapList.toList
        case (i, x, y) if x <= _wide => {
          mapList += ((createUrbanization(i, getRange(maxRange, middleRange, x, y)), x, y))
          _add(i + 1, x + 1, y, mapList)
        }
        case (i, x, y) if x == _wide + 1 && y > 1 => _add(i, 1, y - 1, mapList)
      }
    }

    _add(1, 1, _heigh, ListBuffer())
  }


  def buildMap: List[(Urbanization, Int, Int)] = {
    addUrbToMap
  }

  def print(mapBoardToPrint: List[(Urbanization, Int, Int)]) = {

    def paddinString(toPadd: String): String = {
      "%1$10s".format(toPadd)
    }

    val toPrint = mapBoardToPrint.foldLeft(new StringBuilder)((sb, x) => {
      if (x._2 == _wide) {
        sb.append(paddinString(x._1.getId + s" <${x._1.range}> " + "\n"))
      } else if (x._2 < _wide)
        sb.append(paddinString(x._1.getId) + s"<${x._1.range}> | ")

      sb
    })

    println(toPrint)
  }

  private def getRange(maxRange: Int, middleRange: Int, x: Int, y: Int): Int = {

    var newRange = maxRange
    if ((y != _heigh && y != 1) && (x == 1 && x == _wide)) {
      newRange -= 1
    } else if (y == middleRange && x == middleRange) {
      newRange = 0
    } else if ((y != _heigh && y != 1) && (x > 1 && x < _wide)) {
      newRange -= 1
    }

    newRange
  }

  private def createUrbanization(id: Int, range: Int = 1) = Urbanization(URB_CON + id, range)

}

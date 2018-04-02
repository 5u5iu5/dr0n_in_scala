package com.apozo.dron

import com.apozo.dron.cartography._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class DronService(mapBoard: List[(Urbanization, Int, Int)]) {

  def getListUrbanizations(x: Int, y: Int, range: Int): List[Urbanization] = {
    var resultList: ListBuffer[Urbanization] = new ListBuffer[Urbanization]
    resultList ++= getTotalSurface(range, getUrbId(x, y))
    resultList.toList
  }

  private def getTotalSurface(range: Int, sourceUrb: Urbanization): List[Urbanization] = {

    def ALL = List[Directions](UPL, UP, UPR, LEFT, RIGHT, DOWNL, DOWN, DOWNR)

    var accuUrbList = new ListBuffer[Urbanization]
    var nextRangeIndex = 1

    def getAdyacents(directions: List[Directions], idUrb: String): List[Urbanization] = {
      var surfaceList = new ListBuffer[Urbanization]
      directions.foreach(surfaceList += getAdyacent(idUrb, _))
      surfaceList.toList.distinct
    }

    def getAdyacentsByList(listUrbs: List[Urbanization]): List[Urbanization] = {
      var surfaceList = new ListBuffer[Urbanization]
      listUrbs.foreach(u => surfaceList ++= getAdyacents(ALL, u.getId))
      surfaceList.toList.distinct
    }


    def removeDuplicates[Urbanization](elements: List[Urbanization]): List[Urbanization] = elements match {
      case Nil => elements
      case head :: tail => head :: removeDuplicates(tail filterNot (_ == head))
    }

    def removeNonExist(elements: List[Urbanization]): List[Urbanization] =
      elements.filterNot(_.getId == "NoNExist")

    @tailrec
    def _getTotalSurface(levelList: List[Urbanization]): List[Urbanization] = levelList match {
      case Nil =>
        nextRangeIndex += 1
        accuUrbList ++= getAdyacents(ALL, sourceUrb.getId)
        _getTotalSurface(removeNonExist(accuUrbList.toList))
      case _ :: _ if nextRangeIndex > range => accuUrbList.toList
      case l =>
        nextRangeIndex += 1
        accuUrbList ++= getAdyacentsByList(l)
        _getTotalSurface(removeNonExist(accuUrbList.toList))
    }

    removeDuplicates(removeNonExist(sourceUrb +: _getTotalSurface(Nil)))
  }


  def getUrbId(x: Int, y: Int): Urbanization =
    Some(mapBoard.find((t3) => t3._2 == x && t3._3 == y)).get match {
      case Some(u) => u._1
      case None => Urbanization("NoNExist")
    }

  def getAdyacent(idUrb: String, direcction: Directions): Urbanization =
    mapBoard.find(_._1.getId == idUrb).map(urb =>
      (urb, direcction) match {
        case (u, UP) => getUrbId(u._2, u._3 + 1)
        case (u, LEFT) => getUrbId(u._2 - 1, u._3)
        case (u, RIGHT) => getUrbId(u._2 + 1, u._3)
        case (u, DOWN) => getUrbId(u._2, u._3 - 1)
        case (u, UPL) => getUrbId(u._2 - 1, u._3 + 1)
        case (u, UPR) => getUrbId(u._2 + 1, u._3 + 1)
        case (u, DOWNL) => getUrbId(u._2 - 1, u._3 - 1)
        case (u, DOWNR) => getUrbId(u._2 + 1, u._3 - 1)
      }).get

}



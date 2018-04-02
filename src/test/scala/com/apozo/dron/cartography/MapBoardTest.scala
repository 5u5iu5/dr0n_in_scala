package com.apozo.dron.cartography

import org.scalatest.FunSuite

class MapBoardTest extends FunSuite {

  test("Should create square map to 9 x 9") {
    val mapBoard: MapBoard = MapBoard(9,9)
    val map = mapBoard.buildMap
    mapBoard.print(map)
    assert(map.size == 81)
    assert(map(4)._1.id === "Urb5")
    assert(map(18)._1.id === "Urb19")
    assert(map(18)._1.getRange === 4)
  }

  test("Should create square map to 5 x 5") {
    val mapBoard: MapBoard = MapBoard(5,5)
    val map = mapBoard.buildMap
    mapBoard.print(map)
    assert(map.size == 25)
    assert(map(4)._1.id === "Urb5")
    assert(map(18)._1.id === "Urb19")
    assert(map(18)._1.getRange === 1)
  }

}


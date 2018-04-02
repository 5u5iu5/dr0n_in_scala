package com.apozo.dron

import com.apozo.dron.cartography._
import org.scalatest.{FeatureSpec, FunSuite, GivenWhenThen, Ignore}

class DronServiceTest extends FeatureSpec with GivenWhenThen {

  info("DRON test by Alvaro Del Pozo")

  feature("[getUrbanization] to test") {
    scenario("Simple auxiliary methods to test with simple map") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a urbanization bt coordinates (3,3)")
      val urb = dron.getUrbId(3, 3)
      Then("should return me the urbanization 13")
      assert(urb.getId == MapBoard.URB_CON + "13" && urb.getRange == 0)
    }

    //    scenario("Simple auxiliary methods to test with more complex map") {
    //      Given("a more complex square map 9*9")
    //      val dron = DronService(MapBoard(9, 9).buildMap)
    //      assert(dron != null)
    //      When("get a urbanization bt coordinates (5,5)")
    //      val urb = dron.getUrbId(5, 5)
    //      Then("should return me the urbanization 41")
    //      assert(urb.id == MapBoard.URB_CON + "41" && urb.getRange == 1)
    //    }
  }

  feature("[getAdyacent]") {
    scenario("Simple auxiliary method to test adyacent with UP direction") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a adyacent of Urb13 with UP direction")
      val urb = dron.getAdyacent(MapBoard.URB_CON + "13", UP)
      Then("should return me the urbanization 8")
      assert(urb.getId == MapBoard.URB_CON + "8" && urb.getRange == 1)
    }

    scenario("Simple auxiliary method to test adyacent with LEFT direction") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a adyacent of Urb13 with LEFT direction")
      val urb = dron.getAdyacent(MapBoard.URB_CON + "13", LEFT)
      Then("should return me the urbanization 12")
      assert(urb.getId == MapBoard.URB_CON + "12" && urb.getRange == 1)
    }

    scenario("Simple auxiliary method to test adyacent with RIGHT direction") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a adyacent of Urb13 with RIGHT direction")
      val urb = dron.getAdyacent(MapBoard.URB_CON + "13", RIGHT)
      Then("should return me the urbanization 14")
      assert(urb.getId == MapBoard.URB_CON + "14" && urb.getRange == 1)
    }

    scenario("Simple auxiliary method to test adyacent with DOWN direction") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a adyacent of Urb13 with DOWN direction")
      val urb = dron.getAdyacent(MapBoard.URB_CON + "13", DOWN)
      Then("should return me the urbanization 18")
      assert(urb.getId == MapBoard.URB_CON + "18" && urb.getRange == 1)
    }
  }

  feature("[getListUrbanization] to test") {
    scenario("Simple request to dron with simple map with range = 1") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a all urbanizations by coordinates (3,3) and range 1")
      val urbs = dron.getListUrbanizations(3, 3, 1)
      Then("should return me a list of urbanizations")
      assert(urbs.size == 9)
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 7))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 8))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 9))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 12))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 13))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 14))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 17))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 18))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 19))
    }

    scenario("Complex request to dron with simple map with range = 1") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a all urbanizations by coordinates (2,2) and range 1")
      val urbs = dron.getListUrbanizations(2, 2, 1)
      Then("should return me a list of urbanizations")
      assert(urbs.size == 9)
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 11))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 12))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 13))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 16))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 17))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 18))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 21))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 22))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 23))
    }

    scenario("Simple request to dron with simple map with range = 2") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a all urbanizations by coordinates (3,3) and range 2")
      val urbs = dron.getListUrbanizations(3, 3, 2)
      Then("should return me a list of urbanizations")
      assert(urbs.size == 25)
      for (i <- 1 to 25) assert(urbs.find(_.getId == MapBoard.URB_CON + i).get != null)
    }

    scenario("Complex request to dron with simple map with range = 2 out of map") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a all urbanizations by coordinates (2,2) and range 2")
      val urbs = dron.getListUrbanizations(2, 2, 2)
      Then("should return me a list of urbanizations")
      assert(urbs.size == 16)
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 6))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 7))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 8))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 9))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 11))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 12))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 13))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 14))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 16))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 17))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 18))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 19))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 21))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 22))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 23))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 24))
    }

    scenario("Complex request to dron with simple map with range = 2 out of map in a corner as origin") {
      Given("a simple square map 5*5")
      val dron = DronService(MapBoard(5, 5).buildMap)
      assert(dron != null)
      When("get a all urbanizations by coordinates (5,5) and range 2")
      val urbs = dron.getListUrbanizations(5, 5, 2)
      Then("should return me a list of urbanizations")
      assert(urbs.size == 9)
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 3))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 4))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 5))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 8))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 9))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 10))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 13))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 14))
      assert(urbs.exists(_.getId == MapBoard.URB_CON + 15))
    }

  }

}


package com.apozo.dron.cartography

@SerialVersionUID(123L)
case class Urbanization(id: String, range: Int = 1) extends Serializable {
  def getId = id
  def getRange = range
}

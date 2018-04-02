package com.apozo.dron

object Testing extends App {

  @annotation.tailrec
  def hasLoop(path: Seq[(Int, Int)]): Boolean =
    if (path.size < 2) false
    else if (path.tail.contains(path.head)) true
    else hasLoop(path.tail)

  println(hasLoop(Array((0, 0), (0, 1))))
  println(hasLoop(Array((0, 0), (1, 0), (1, -1), (0, 0), (0, 1))))

}

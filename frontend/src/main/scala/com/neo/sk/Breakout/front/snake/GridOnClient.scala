package com.neo.sk.Breakout.front.snake

import com.neo.sk.Breakout.Breakout.{Grid, Point}

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 10:13 PM
  */
class GridOnClient(override var boundary: Point, override var model: Int) extends Grid {

  override def debug(msg: String): Unit = println(msg)

  override def info(msg: String): Unit = println(msg)

}

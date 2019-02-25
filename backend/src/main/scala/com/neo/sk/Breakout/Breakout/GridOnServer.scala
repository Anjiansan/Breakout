package com.neo.sk.Breakout.Breakout

import com.neo.sk.Breakout.Breakout._
import org.slf4j.LoggerFactory

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 9:55 PM
  */
class GridOnServer(override var boundary: Point, override var model: Int) extends Grid {


  private[this] val log = LoggerFactory.getLogger(this.getClass)

  override def debug(msg: String): Unit = log.debug(msg)

  override def info(msg: String): Unit = log.info(msg)

  private[this] var waitingJoin = Map.empty[Long, String]

  def addBreakout(id: Long, name: String) = {
    waitingJoin += (id -> name)
    genWaitingSnake()
  }

  private[this] def genWaitingSnake() = {
    waitingJoin.filterNot(kv => breakouts.contains(kv._1)).foreach { case (id, name) =>
      breakouts += id -> initScene(id, name)
    }
    waitingJoin = Map.empty[Long, String]
  }

  override def update(isFront: Boolean = false): Unit = {
    super.update()
//    genWaitingSnake()
  }

}

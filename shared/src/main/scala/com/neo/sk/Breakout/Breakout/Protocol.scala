package com.neo.sk.Breakout.Breakout

/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:40 PM
  */
object Protocol {

  sealed trait GameMessage

  case class GridDataSync(
                          frameCount: Long,
                          breakouts: List[SkDt]
  ) extends GameMessage

  case class TextMsg(
    msg: String
  ) extends GameMessage

  case class Id(id: Long) extends GameMessage

  case class Play(id: Long) extends GameMessage

  case class BreakoutAction(id: Long, keyCode: Int, frame: Long) extends GameMessage

  case class BreakoutLeft(id: Long, name: String) extends GameMessage

  case class NetDelayTest(createTime: Long) extends GameMessage

  val frameRate = 90

}

package com.neo.sk.Breakout

import com.neo.sk.Breakout.snake.Protocol

/**
  * User: Taoz
  * Date: 6/28/2018
  * Time: 7:26 PM
  */
object TmpTest {


  import io.circe._
  import io.circe.parser._
  import io.circe.generic.auto._
  import io.circe.syntax._

  sealed trait TmpMsg

  case class Action(name: String, value: Float) extends TmpMsg

  case class MultiAction(name: String, a1: Action, a2: Action) extends TmpMsg


  def main(args: Array[String]): Unit = {

    val LITTLE_ENDIAN = true


    def bytes2Long(bytes: Array[Byte]): Long = {
      /*    var value = 0l
          var c = 0
          while (c < 8) {
            val shift = (if (LITTLE_ENDIAN) c else 7 - c) << 3
            value |= (0xff.toLong << shift) & (bytes(c).toLong << shift)
            c += 1
          }
          value*/

      val data = if (LITTLE_ENDIAN) bytes.reverse else bytes
      var value = 0l
      var c = 0
      while (c < 8) {
        val shift = (7 - c) << 3
        value |= (0xff.toLong << shift) & (data(c).toLong << shift)
        c += 1
      }
      value
    }

    def long2Bytes(l: Long): Array[Byte] = {
      val bytes = new Array[Byte](8)
      var temp = l
      var c = 0
      while (c < 8) {
        bytes(7 - c) = (temp & 0xff).toByte
        temp = temp >> 8
        c += 1
      }
      if (LITTLE_ENDIAN) bytes.reverse else bytes
    }


    val l0 = Long.MaxValue
    val bytes = long2Bytes(l0)
    val l1 = bytes2Long(bytes)

    println(s"l0: $l0")
    println(s"bytes: ${bytes.mkString(",")}")
    println(s"l1: $l1")


  }

  def main1(args: Array[String]): Unit = {

    println("hello, world.")

    //val msg: Msg = Action("a1", 1.1f)
    val msg: TmpMsg = MultiAction("t1", Action("a1", 1.1f), Action("a2", 2.1f))
    val jsonStr = msg.asJson.noSpaces
    println("jsonStr:" + jsonStr)

    val m1 = decode[TmpMsg](jsonStr)
    println("m1:", m1)


  }

}






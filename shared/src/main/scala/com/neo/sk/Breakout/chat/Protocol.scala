package com.neo.sk.Breakout.chat

import com.neo.sk.Breakout.utils.MiddleBuffer

/**
  * User: Taoz
  * Date: 7/12/2018
  * Time: 9:13 AM
  */
object Protocol {



  sealed trait Msg
  case class TextMsg(id: Int, data: String, value: Float, d: Double) extends Msg
  case class MultiTextMsg(id: Int, b: Option[Boolean], ls: List[TextMsg], l: Long = -999l) extends Msg


  case class TestMessage(id: Int, data: String, ls: Array[Float]){
    def encode(container: MiddleBuffer): Unit = {
      container.putInt(id)
      container.putString(data)
      //container.putFloatArray(ls)
    }
  }

  object TestMessage {
    def decode(data: MiddleBuffer): TestMessage = {
      val id = data.getInt()
      val d = data.getString()
//      val ls = data.getFloatArray()
      TestMessage(id, d, Array())
    }

    def encode(target: TestMessage, container: MiddleBuffer): Unit = {
      container.putInt(target.id)
      container.putString(target.data)
      //container.putFloatArray(target.ls)
    }
  }


  case class TestMessage1(){
    var id = 1
    var data = ""
    var ls = Array(1.2f)

    def this(id: Int, data: String, ls: Array[Float]) {
      this()
      this.id = id
      this.data = data
      this.ls = ls
    }

    def toBytes(container: MiddleBuffer): MiddleBuffer = {
      container.putInt(id)
      container.putString(data)
      //container.putFloatArray(ls)
      container
    }

    def this(data: MiddleBuffer) {
      this()
      id = data.getInt()
      this.data = data.getString()
      //ls = data.getFloatArray()
      ls = Array()
    }
  }

  object TestMessage1 {


  }


}




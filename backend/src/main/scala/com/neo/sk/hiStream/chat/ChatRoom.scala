package com.neo.sk.hiStream.chat

import akka.NotUsed
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.neo.sk.hiStream.chat.Protocol.{Msg, MultiTextMsg, TextMsg}
import com.neo.sk.hiStream.chat.RoomMaster.{JoinRoom, LeftRoom}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

/**
  * User: Taoz
  * Date: 7/10/2018
  * Time: 10:51 AM
  */
trait ChatRoom {

  def join(id: Int, name: String): Flow[Msg, Msg, Any]

}


object ChatRoom {

  import RoomMaster._

  private case class Key(id: Long, keyCode: Int)

  private case class NetTest(id: Long, createTime: Long)


  def playInSink(
    id: Int,
    name: String,
    room: ActorRef): Sink[Msg, NotUsed] = {
    Sink.actorRef[Msg](room, LeftRoom(id, name))
  }

  def create(system: ActorSystem)(implicit executor: ExecutionContext): ChatRoom = {

    val room = system.actorOf(RoomMaster.props(), "roomMaster")

    new ChatRoom {
      override def join(id: Int, name: String): Flow[Msg, Msg, Any] = {
        val in =
          Flow[Msg]
            .map { s => s }
            .to(playInSink(id, name, room))
        val out =
          Source.actorRef[Msg](3, OverflowStrategy.dropHead)
            .mapMaterializedValue(outActor => room ! JoinRoom(id, name, outActor))
        Flow.fromSinkAndSource(in, out)
      }
    }
  }
}

object RoomMaster {

  def props() = Props(new RoomMaster())

  final case class JoinRoom(id: Int, name: String, peer: ActorRef)

  final case class LeftRoom(id: Int, name: String)


}

class RoomMaster extends Actor {

  import concurrent.duration._

  private var peer: Option[ActorRef] = None
  private val log = LoggerFactory.getLogger(this.getClass)

  import context.dispatcher

  val keepAlive =
    context.system.scheduler.schedule(0 second, 8 second, self, "\u0001")


  override def receive: Receive = {
    case "\u0001" =>
      val m = TextMsg(123, s"heartbeat", 0.123f, 0.00000001)
      dispatch(m)
    case JoinRoom(id, name, userRef) =>
      log.info(s"$id joined room by [$name]")
      peer = Some(userRef)
      val m = TextMsg(0, s"welcome [$name]", 0.1f, 0.009)
      dispatch(m)
    case msg: Msg => msg match {
      case m@TextMsg(id, data, value, dd) =>
        log.info(s"got: $m")
        val rep =
          if (data.startsWith("x3")) {
            val ls = (1 to 3).map { i =>
              TextMsg(id, s"i got your msg[$data]", i*0.1f, i*0.0001)
            }
            MultiTextMsg(id, None, ls.toList)
          } else {
            TextMsg(id, s"i got your msg[$data]", value + 0.1f, 0.0000001)
          }
        dispatch(rep)
      case m@MultiTextMsg(id, d, ls, l) =>
        log.info(s"got: $m")
        var c = 0.0f
        ls.foreach { r =>
          val rep = TextMsg(id, s"multiMsg part msg[${r.data}]", c, 0.0000001)
          c += 0.1f
          dispatch(rep)
        }
    }

    case LeftRoom(_, name) =>
      log.info(s"$name left.")
      peer = None
    case x =>
      log.info(s"got unknown msg: $x")
      val rep = TextMsg(9999, s"i got your msg[${x.toString}]", 0.999f, 0.000000003)
      dispatch(rep)
  }


  private def dispatch(msg: Msg): Unit = {
    log.debug(s"send msg: $msg")
    peer.foreach(_ ! msg)
  }
}

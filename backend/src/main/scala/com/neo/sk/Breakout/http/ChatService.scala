package com.neo.sk.Breakout.http

import java.util.concurrent.atomic.AtomicInteger

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives.{getFromResource, handleWebSocketMessages, parameter, path, pathEndOrSingleSlash, pathPrefix, _}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorAttributes, Materializer, Supervision}
import akka.util.{ByteString, Timeout}
import com.neo.sk.Breakout.chat.ChatRoom
import com.neo.sk.Breakout.chat.Protocol.{Msg, TestMessage, TextMsg}
import com.neo.sk.utils.MiddleBufferInJvm
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContextExecutor

/**
  * User: Taoz
  * Date: 7/10/2018
  * Time: 12:25 PM
  */
trait ChatService {
  import com.neo.sk.utils.byteObject.ByteObject._

  implicit val system: ActorSystem

  implicit def executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  implicit val timeout: Timeout

  lazy val chatRoom: ChatRoom = ChatRoom.create(system)

  private val idGenerator = new AtomicInteger(1000000)

  private[this] val log = LoggerFactory.getLogger(this.getClass)


  val chatRoute: Route = {
    (pathPrefix("chat") & get) {
      pathEndOrSingleSlash {
        getFromResource("html/chat.html")
      } ~
      path("join") {
        parameter('name) { name =>
          println(s"Got a join connection: $name")
          handleWebSocketMessages(webSocketChatFlow(name))
        }
      }
    }
  }

  val sendBuffer = new MiddleBufferInJvm(4096)


  def webSocketChatFlow(nickname: String): Flow[Message, Message, Any] =
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          log.info(s"msg from webSocket: $msg")
          TextMsg(-1, msg, 100.1f, 1.000000000000003)
        case BinaryMessage.Strict(bMsg) =>

          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[Msg](buffer) match {
              case Right(v) => v
              case Left(e) =>
                println(s"decode error: ${e.message}")
                TextMsg(-1, "decode error", 9.1f, 1.00000000002)
            }

          msg
      }
      .via(chatRoom.join(idGenerator.getAndIncrement(), nickname)) // ... and route them through the chatFlow ...
      .map { msg =>
      BinaryMessage.Strict(ByteString(
        //encoded process
        msg.fillMiddleBuffer(sendBuffer).result()
      )) // ... pack outgoing messages into WS JSON messages ...
    }.withAttributes(ActorAttributes.supervisionStrategy(decider)) // ... then log any processing errors on stdin


  var c = 10


  private def str2byteBuffer(str: String) = {
    val id = (System.currentTimeMillis() / 10000).toInt
    println(s"id: $id")
    val ls = new Range(0, id % 5, 1).toArray.map(_ + 0.1f)
    val msg = TestMessage(id, str, ls)
    sendBuffer.clear()
    msg.encode(sendBuffer)
    val r = sendBuffer.result()
    println(s"send msg: $msg")
    println(s"send bytes: ${r.mkString(",")}")
    r
  }


  private def str2bytes(str: String): Array[Byte] = {

    val data = str.getBytes("utf-8")
    val len = data.length
    val array = new Array[Byte](len + 1)
    array(0) = len.toByte
    for (i <- 0 until len) {
      array(i + 1) = data(i)
    }
    println(s"str2bytes: str=[$str] to [${array.mkString(",")}]")
    array
  }


  private def tmp: Flow[Any, Nothing, NotUsed] = {
    val in = Sink.ignore
    val out = Source.empty
    Flow.fromSinkAndSource(in, out)
  }

  private val decider: Supervision.Decider = {
    e: Throwable =>
      e.printStackTrace()
      println(s"WS stream failed with $e")
      Supervision.Resume
  }


}

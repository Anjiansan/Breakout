package com.neo.sk.Breakout.Breakout

import java.awt.event.KeyEvent

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.neo.sk.Breakout.Breakout.Protocol.Play
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:29 PM
  */


trait PlayGround {


  def joinGame(id: Long, name: String): Flow[String, Protocol.GameMessage, Any]

  def syncData()

}


object PlayGround {

  val bounds = Point(Boundary.w, Boundary.h)

  val log = LoggerFactory.getLogger(this.getClass)


  def create(system: ActorSystem)(implicit executor: ExecutionContext): PlayGround = {

    val ground = system.actorOf(Props(new Actor {
      var subscribers = Map.empty[Long, ActorRef]

      var userMap = Map.empty[Long, String]

      val grid = new GridOnServer(bounds)

      var tickCount = 0l

      override def receive: Receive = {
        case r@Join(id, name, subscriber) =>
          log.info(s"got $r")
          userMap += (id -> name)
          context.watch(subscriber)
          subscribers += (id -> subscriber)
          grid.addBreakout(id, name)
          dispatchTo(id, Protocol.Id(id))
          dispatch(grid.getGridData)

        case r@Left(id, name) =>
          log.info(s"got $r")
          subscribers.get(id).foreach(context.unwatch)
          subscribers -= id
          grid.removeBreakout(id)
          dispatch(Protocol.BreakoutLeft(id, name))

        case r@Key(id, keyCode, frame) =>
          log.debug(s"got $r")
          val skDt = grid.breakouts.filter(_._1 == id).head._2
          if (keyCode == KeyEvent.VK_SPACE) {
            skDt.ball.fired = true
            skDt.ball.speedX = 2
            skDt.ball.speedY = 15
            dispatch(grid.getGridData)
            dispatchTo(id, Play(id))
          } else if(skDt.ball.fired) {
            grid.addActionWithFrame(id, keyCode, frame)
            dispatch(Protocol.BreakoutAction(id, keyCode, frame))
          }

        case r@Terminated(actor) =>
          log.warn(s"got $r")
          subscribers.find(_._2.equals(actor)).foreach { case (id, _) =>
            log.debug(s"got Terminated id = $id")
            subscribers -= id
            grid.removeBreakout(id).foreach(s => dispatch(Protocol.BreakoutLeft(id, s.name)))
          }

        case Sync =>
          tickCount += 1
          grid.update()
          if (tickCount % 20 == 5) {
            val gridData = grid.getGridData
            dispatch(gridData)
          }

        case NetTest(id, createTime) =>
          log.info(s"Net Test: createTime=$createTime")
          dispatchTo(id, Protocol.NetDelayTest(createTime))

        case x =>
          log.warn(s"got unknown msg: $x")
      }

      def dispatchTo(id: Long, gameOutPut: Protocol.GameMessage): Unit = {
        subscribers.get(id).foreach { ref => ref ! gameOutPut }
      }

      def dispatch(gameOutPut: Protocol.GameMessage) = {
        subscribers.foreach { case (_, ref) => ref ! gameOutPut }
      }


    }
    ), "ground")

    import concurrent.duration._
    system.scheduler.schedule(3 seconds, Protocol.frameRate millis, ground, Sync) // sync tick


    def playInSink(id: Long, name: String) = Sink.actorRef[UserAction](ground, Left(id, name))


    new PlayGround {
      override def joinGame(id: Long, name: String): Flow[String, Protocol.GameMessage, Any] = {
        val in =
          Flow[String]
            .map { s =>
              if (s.startsWith("T")) {
                val timestamp = s.substring(1).toLong
                NetTest(id, timestamp)
              } else {
                Key(id, s.split("#").head.toInt, s.split("#").last.toLong)
              }
            }
            .to(playInSink(id, name))

        val out =
          Source.actorRef[Protocol.GameMessage](3, OverflowStrategy.dropHead)
            .mapMaterializedValue(outActor => ground ! Join(id, name, outActor))

        Flow.fromSinkAndSource(in, out)
      }

      override def syncData(): Unit = ground ! Sync
    }

  }


  private sealed trait UserAction

  private case class Join(id: Long, name: String, subscriber: ActorRef) extends UserAction

  private case class Left(id: Long, name: String) extends UserAction

  private case class Key(id: Long, keyCode: Int, frame: Long) extends UserAction

  private case class NetTest(id: Long, createTime: Long) extends UserAction

  private case object Sync extends UserAction


}
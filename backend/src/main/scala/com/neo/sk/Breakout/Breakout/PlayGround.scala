package com.neo.sk.Breakout.Breakout

import java.awt.event.KeyEvent
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.neo.sk.Breakout.Breakout.Protocol.{Play, Winner}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:29 PM
  */


trait PlayGround {


  def joinGame(id: Long, name: String, t: Int): Flow[String, Protocol.GameMessage, Any]

  def syncData()

}


object PlayGround {

  val log = LoggerFactory.getLogger(this.getClass)

  val users = scala.collection.mutable.ListBuffer.empty[Long]

  private val idGenerator = new AtomicInteger(1000)


  def create(system: ActorSystem)(implicit executor: ExecutionContext): PlayGround = {

    val ground = system.actorOf(Props(new Actor {
      var subscribers = scala.collection.mutable.Map.empty[Long, List[(Long, ActorRef)]]

      var userMap = Map.empty[Long, (String, Long)]

      val grid = scala.collection.mutable.Map.empty[Long, GridOnServer]

      val dRooms = scala.collection.mutable.ListBuffer.empty[Long]

      var tickCount = 0l

      override def receive: Receive = {
        case r@Join(id, name, subscriber, t) =>
          log.info(s"got $r")
          if(t == 1 || dRooms.isEmpty) {
            val roomId = idGenerator.getAndIncrement().toLong
            userMap += (id -> (name, roomId))
            context.watch(subscriber)
            subscribers += (roomId -> List((id, subscriber)))
            if(t == 1) {
              grid += (roomId -> new GridOnServer(Point(Boundary.w, Boundary.h), 1))
            }
            else {
              dRooms += roomId
              grid += (roomId -> new GridOnServer(Point(Boundary1.w, Boundary1.h), 2))
            }
            grid(roomId).addBreakout(id, name)
            dispatchTo(roomId, id, Protocol.Id(id))
            dispatch(roomId, grid(roomId).getGridData)
          }
          else {
            val roomId = dRooms.head
            dRooms -= roomId
            userMap += (id -> (name, roomId))
            context.watch(subscriber)
            subscribers.update(roomId, subscribers(roomId) :+ (id, subscriber))
            grid(roomId).addBreakout(id, name)
            dispatchTo(roomId, id, Protocol.Id(id))
            grid(roomId).breakouts.foreach {
              b =>
                val skDt = b._2
                skDt.ball.fired = true
                skDt.ball.speedX = 2
                skDt.ball.speedY = 15
            }
            dispatch(roomId, grid(roomId).getGridData)
            grid(roomId).breakouts.foreach(b => dispatchTo(roomId, b._1, Play(b._1)))
          }

        case r@Left(id, name) =>
          log.info(s"got $r")
          val roomId = userMap(id)._2
          subscribers(roomId).foreach(i => if(i._1 == id) context.unwatch(i._2))
          subscribers.update(roomId, subscribers(roomId).filter(_._1 != id))
          grid(roomId).removeBreakout(id)
          dispatch(roomId, Protocol.BreakoutLeft(id, name))

        case r@Key(id, keyCode, frame) =>
          log.debug(s"got $r")
          val roomId = userMap(id)._2
          val skDt = grid(roomId).breakouts.filter(_._1 == id).head._2
          if (keyCode == KeyEvent.VK_SPACE && !dRooms.contains(roomId) && grid(roomId).breakouts.size == 1) {
            skDt.ball.fired = true
            skDt.ball.speedX = 2
            skDt.ball.speedY = 15
            dispatch(roomId, grid(roomId).getGridData)
            dispatchTo(roomId, id, Play(id))
          } else if(skDt.ball.fired) {
            grid(roomId).addActionWithFrame(id, keyCode, frame)
            dispatch(roomId, Protocol.BreakoutAction(id, keyCode, frame))
          }

        case r@Terminated(actor) =>
          log.warn(s"got $r")
//          val roomId = subscribers.filter(_.)
//          subscribers.find(_._2.equals(actor)).foreach { case (id, _) =>
//            log.debug(s"got Terminated id = $id")
//            subscribers -= id
//            grid(roomId).removeBreakout(id).foreach(s => dispatch(roomId, Protocol.BreakoutLeft(id, s.name)))
//          }

        case Sync =>
          tickCount += 1
          grid.foreach {
            g =>
              g._2.update()
              if (tickCount % 10 == 5) {
                val gridData = g._2.getGridData
                dispatch(g._1, gridData)
              }
              if(g._2.model == 2 &&  g._2.deadCount > 1) {
                dispatch(g._1, Winner(g._2.maxScore, userMap(g._2.maxScore)._1))
              }
          }

        case NetTest(id, createTime) =>
          log.info(s"Net Test: createTime=$createTime")
          val roomId = userMap(id)._2
          dispatchTo(roomId, id, Protocol.NetDelayTest(createTime))

        case x =>
          log.warn(s"got unknown msg: $x")
      }

      def dispatchTo(rId: Long, id: Long, gameOutPut: Protocol.GameMessage): Unit = {
        subscribers(rId).foreach { ref => if(id == ref._1) ref._2 ! gameOutPut }
      }

      def dispatch(rId: Long, gameOutPut: Protocol.GameMessage) = {
        subscribers(rId).foreach { case (_, ref) => ref ! gameOutPut }
      }


    }
    ), "ground")

    import concurrent.duration._
    system.scheduler.schedule(3 seconds, Protocol.frameRate millis, ground, Sync) // sync tick


    def playInSink(id: Long, name: String) = Sink.actorRef[UserAction](ground, Left(id, name))


    new PlayGround {
      override def joinGame(id: Long, name: String, t: Int): Flow[String, Protocol.GameMessage, Any] = {
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
            .mapMaterializedValue(outActor => ground ! Join(id, name, outActor, t))

        Flow.fromSinkAndSource(in, out)
      }

      override def syncData(): Unit = ground ! Sync
    }

  }


  private sealed trait UserAction

  private case class Join(id: Long, name: String, subscriber: ActorRef, t: Int) extends UserAction

  private case class Left(id: Long, name: String) extends UserAction

  private case class Key(id: Long, keyCode: Int, frame: Long) extends UserAction

  private case class NetTest(id: Long, createTime: Long) extends UserAction

  private case object Sync extends UserAction


}
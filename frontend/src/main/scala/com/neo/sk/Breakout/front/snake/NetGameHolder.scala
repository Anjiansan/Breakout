package com.neo.sk.Breakout.front.snake

import java.awt.event.KeyEvent

import com.neo.sk.Breakout.Breakout.Protocol.GridDataSync
import com.neo.sk.Breakout.Breakout._
import org.scalajs.dom
import org.scalajs.dom.ext.{Color, KeyCode}
import org.scalajs.dom.html.{Document => _, _}
import org.scalajs.dom.raw._
import scalatags.JsDom.short._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 12:45 PM
  */
@JSExportTopLevel("snake.NetGameHolder")
object NetGameHolder {


  val bounds = Point(Boundary.w, Boundary.h)
  val canvasUnit = 1
  val canvasBoundary = bounds * canvasUnit
  val textLineHeight = 14

  var currentRank = List.empty[Score]
  var myId = -1l

  val grid = new GridOnClient(bounds)

  var firstCome = true
  var wsSetup = false
  var justSynced = false
  var isPlay = false

  var lastBX = 0D
  var lastBY = 0D

  val watchKeys = Set(
    KeyCode.Space,
    KeyCode.Left,
    KeyCode.Right,
    KeyCode.F2
  )

  object MyColors {
    val myHeader = "#FF0000"
    val myBody = "#FFFFFF"
    val otherHeader = Color.Blue.toString()
    val otherBody = "#696969"
  }

  private[this] lazy val nameField = dom.document.getElementById("name").asInstanceOf[HTMLInputElement]
  private[this] lazy val joinButton = dom.document.getElementById("join").asInstanceOf[HTMLButtonElement]
  private[this] lazy val canvas = dom.document.getElementById("GameView").asInstanceOf[Canvas]
  private[this] lazy val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private[this] val offCanvas = dom.document.getElementById("backCanvas").asInstanceOf[Canvas]
  private[this] val offCtx = offCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  val background = img(*.src := s"/breakout/static/img/background.jpg").render
  val ball = img(*.src := s"/breakout/static/img/ball.png").render
  val blockL = img(*.src := s"/breakout/static/img/block001.png").render
  val blockH = img(*.src := s"/breakout/static/img/block002.png").render
  val paddle = img(*.src := s"/breakout/static/img/paddle.png").render

  var nextFrame = 0//requestAnimationFrame
  private var logicFrameTime = System.currentTimeMillis()

  @JSExport
  def run(): Unit = {
    isPlay = false
    canvas.width = canvasBoundary.x.toInt
    canvas.height = canvasBoundary.y.toInt
    drawGameOn()

    joinButton.onclick = { (event: MouseEvent) =>
      joinGame(nameField.value)
      logicFrameTime = System.currentTimeMillis()
      nextFrame = dom.window.requestAnimationFrame(gameRender())
      event.preventDefault()
    }
    nameField.focus()
    nameField.onkeypress = { (event: KeyboardEvent) =>
      if (event.keyCode == 13) {
        joinButton.click()
        event.preventDefault()
      }
    }

    dom.window.setInterval(() => gameLoop(), Protocol.frameRate)
  }

  def gameRender():Double => Unit = {
    d =>
      val cur = System.currentTimeMillis()
      val offsetTime = cur - logicFrameTime
//      grid.checkRush(myId)
      draw(offsetTime)
      nextFrame = dom.window.requestAnimationFrame(gameRender())
  }

  def drawGameOn(): Unit = {
//    ctx.fillStyle = Color.Black.toString()
//    ctx.fillRect(0, 0, canvas.width, canvas.height)
    ctx.fillStyle = Color.Black.toString()
    ctx.font = "36px Helvetica"
    ctx.fillText("Welcome.", 150, 180)
//    offCtx.drawImage(background,0,0,Boundary.w.toInt,bounds.y.toInt)
  }

  def drawWin(): Unit = {
    ctx.fillStyle = Color.Black.toString()
    ctx.fillRect(0, 0, canvas.width, canvas.height)
    ctx.fillStyle = Color.Red.toString()
    ctx.font = "36px Helvetica"
    ctx.fillText("Game Over.", 150, 180)
//    offCtx.drawImage(background,0,0,Boundary.w.toInt,bounds.y.toInt)
  }

  def drawGameOff(): Unit = {
    if (!firstCome) {
      ctx.fillStyle = Color.Black.toString()
      ctx.fillRect(0, 0, bounds.x * canvasUnit, bounds.y * canvasUnit)
      ctx.fillStyle = "rgb(250, 250, 250)"
      ctx.font = "36px Helvetica"
      ctx.fillText("Ops, connection lost.", 150, 180)
    }
  }


  def gameLoop(): Unit = {
    if (wsSetup && isPlay) {
      logicFrameTime = System.currentTimeMillis()
      update()
//      if (!justSynced) {
//        update()
//      } else {
//        justSynced = false
//      }
    }
  }

  def update(): Unit = {
    grid.update(true)
  }

  def draw(offsetTime: Double): Unit = {
    if (wsSetup) {
      if(grid.winner == myId) {
        drawWin()
        isPlay = false
      }
      else {
        val data = grid.getGridData
        drawGrid(myId, data, offsetTime)
      }
    } else {
      drawGameOff()
    }
  }

  def drawGrid(uid: Long, data: GridDataSync, offsetTime: Double): Unit = {

    ctx.clearRect(0, 0, bounds.x * canvasUnit, bounds.y * canvasUnit)
    ctx.drawImage(background, 0, 0, Boundary.w.toInt,bounds.y.toInt)

    data.breakouts.find(_.id == myId) match {
      case Some(breakout) =>
        if(isPlay) {
          ctx.drawImage(paddle, breakout.paddle.x + breakout.paddle.speed * offsetTime / Protocol.frameRate, breakout.paddle.y, PaddleSize.w, PaddleSize.h)
          if(!grid.checkRush(myId, offsetTime)) {
            ctx.drawImage(ball, breakout.ball.x - breakout.ball.speedX * offsetTime / Protocol.frameRate, breakout.ball.y - breakout.ball.speedY * offsetTime / Protocol.frameRate, BallSize.w, BallSize.h)
            println(offsetTime + " " + (breakout.ball.x - breakout.ball.speedX * offsetTime / Protocol.frameRate) + " " + (breakout.ball.y - breakout.ball.speedY * offsetTime / Protocol.frameRate))
            lastBX = breakout.ball.x - breakout.ball.speedX * offsetTime / Protocol.frameRate
            lastBY = breakout.ball.y - breakout.ball.speedY * offsetTime / Protocol.frameRate
          }
          else {
            ctx.drawImage(ball, lastBX, lastBY, BallSize.w, BallSize.h)
          }
        }
        else {
          ctx.drawImage(paddle, breakout.paddle.x, breakout.paddle.y, PaddleSize.w, PaddleSize.h)
          ctx.drawImage(ball, breakout.ball.x, breakout.ball.y, BallSize.w, BallSize.h)
        }


        breakout.blocks.foreach {
          b =>
            if(b.alive)
              ctx.drawImage(if(b.life == 2) blockH else blockL, b.x, b.y, BlockSize.w, BlockSize.h)
        }
        ctx.fillStyle = Color.White.toString()
        ctx.font = "24px Microsoft YaHei"
        ctx.fillText(s"分数: ${breakout.score.allScore}", 10, 30)
        ctx.fillText(s"关卡: ${breakout.score.lv}", bounds.x - 100, 30)

      case None =>
        drawGameOff()
    }

  }

  def joinGame(name: String): Unit = {
    joinButton.disabled = true
    val gameStream = new WebSocket(getWebSocketUri(dom.document, name))
    gameStream.onopen = { (event0: Event) =>
      drawGameOn()
      wsSetup = true
      canvas.focus()
      canvas.onkeydown = {
        (e: dom.KeyboardEvent) => {
          if (watchKeys.contains(e.keyCode)) {
            if (e.keyCode == KeyCode.F2) {
              gameStream.send("T" + System.currentTimeMillis())
            } else {
              gameStream.send(s"${e.keyCode.toString}#${grid.frameCount + 1}")
            }
            e.preventDefault()
          }
        }
      }
      event0
    }

    gameStream.onerror = { (event: Event) =>
      drawGameOff()
      joinButton.disabled = false
      wsSetup = false
      nameField.focus()

    }


    import io.circe.generic.auto._
    import io.circe.parser._

    gameStream.onmessage = { (event: MessageEvent) =>
      val wsMsg = decode[Protocol.GameMessage](event.data.toString).right.get
      wsMsg match {
        case Protocol.Id(id) => myId = id
        case Protocol.TextMsg(message) => writeToArea(s"MESSAGE: $message")
        case Protocol.BreakoutLeft(id, user) => writeToArea(s"$user left!")
        case Protocol.Play(id) =>
          if(id == myId)
          {
            isPlay = true
            logicFrameTime = System.currentTimeMillis()
          }

        case a@Protocol.BreakoutAction(id, keyCode, frame) =>
          grid.addActionWithFrame(id, keyCode, frame)

        case data: Protocol.GridDataSync =>
          if(data.breakouts.nonEmpty && grid.breakouts.nonEmpty)
            println("=========" + grid.frameCount + " " + grid.breakouts.head._2.ball.y + " " + data.frameCount + " " + data.breakouts.head.ball.y)
          grid.actionMap = grid.actionMap.filterKeys(_ > data.frameCount)
          grid.frameCount = data.frameCount
          grid.breakouts.empty
          grid.breakouts ++= data.breakouts.map(s => s.id -> s)
          justSynced = true
          logicFrameTime = System.currentTimeMillis()
        //drawGrid(msgData.uid, data)
        case Protocol.NetDelayTest(createTime) =>
          val receiveTime = System.currentTimeMillis()
          val m = s"Net Delay Test: createTime=$createTime, receiveTime=$receiveTime, twoWayDelay=${receiveTime - createTime}"
          writeToArea(m)
      }
    }


    gameStream.onclose = { (event: Event) =>
      drawGameOff()
      joinButton.disabled = false
      wsSetup = false
      nameField.focus()
    }

    def writeToArea(text: String): Unit = {
//      playground.insertBefore(p(text), playground.firstChild)
      println(text)
    }
  }

  def getWebSocketUri(document: Document, nameOfChatParticipant: String): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}/breakout/netSnake/join?name=$nameOfChatParticipant"
  }


}

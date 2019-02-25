package com.neo.sk.Breakout.Breakout

import java.awt.event.KeyEvent

import scala.util.Random


/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 5:34 PM
  */
trait Grid {

  var boundary: Point

  var model:Int

  def debug(msg: String): Unit

  def info(msg: String): Unit

  val random = new Random(System.nanoTime())

  val historyRankLength = 5

  var frameCount = 0l
  var breakouts = scala.collection.mutable.Map.empty[Long, SkDt]
  var actionMap = Map.empty[Long, Map[Long, Int]]

  var winner = -1l
  var maxScore = 0l
  var deadCount = 0

  def removeBreakout(id: Long): Option[SkDt] = {
    val r = breakouts.get(id)
    if (r.isDefined) {
      breakouts -= id
    }
    r
  }


  def addAction(id: Long, keyCode: Int) = {
    addActionWithFrame(id, keyCode, frameCount + 1)
  }

  def addActionWithFrame(id: Long, keyCode: Int, frame: Long) = {
    val map = actionMap.getOrElse(frame, Map.empty)
    val tmp = map + (id -> keyCode)
    actionMap += (frame -> tmp)
  }


  def update(isFront: Boolean = false) = {
    //println(s"-------- grid update frameCount= $frameCount ---------")
    breakouts.foreach(_._2.paddle.speed = 0)
    updateBreakouts(isFront)
    actionMap -= frameCount
    frameCount += 1
  }

  def initScene(id: Long, name: String, lv: Int = Level.DEFAULT) = {
    val paddle = if(model ==1) Paddle(449, 450) else Paddle(199, 550)
    val ball = if(model ==1) Ball(491, 432) else Ball(241, 532)
    val data = initBlock(lv)
    SkDt(id, name, paddle, ball, data._1, Score(data._2))
  }

  def initBlock(lv: Int = Level.DEFAULT) = {
    import scala.collection.mutable.ListBuffer
    // 创建砖块坐标二维数组，并生成不同关卡
    val c_w = boundary.x              // canvas宽度
    val c_h = boundary.y             // canvas高度
    val xNum_max = c_w/50                    // x轴砖块最大数量
    val yNum_max = 6                       // y轴砖块最大数量
    var x_start = 0D                        // x轴起始坐标，根据砖块数量浮动
    var y_start = 60D                          // y轴起始坐标，默认从60起

    val arr = ListBuffer.empty[(Double, Double, Int)]

    lv match {
      case 1 => // 正三角形
        var xNum = if(model == 1) 16 else 8                               // x轴砖块第一层数量
        var yNum = if(model == 1) 9 else 5                                 // y轴砖块层数
        // 循环y轴
        for(i <- 0 until yNum){
          // 修改每层x轴砖块数量
          if (i == 0) {
            xNum = 1
          } else if (i == 1) {
            xNum = 2
          } else {
            xNum += 2
          }
          x_start = (xNum_max - xNum)/2 * 50             // 修改每层x轴砖块起始坐标
          // 循环x轴
          for(k <- 0 until xNum){
            arr.append((x_start + k*50, y_start + i*20, if(i < 3) 2 else 1))
          }
        }
      case 2 =>  // 倒三角形
        var xNum = 16                              // x轴砖块第一层数量
        var yNum = 9                                // y轴砖块层数
        // 循环y轴
        for(i <- 0 until yNum){
          // 修改每层x轴砖块数量
          if (i == yNum - 1) {
            xNum = 1
          } else if (i == 0) {
            xNum = xNum
          } else {
            xNum -= 2
          }
          x_start = (xNum_max - xNum)/2 * 50             // 修改每层x轴砖块起始坐标
          // 循环x轴
          for(k <- 0 until xNum){
            arr.append((x_start + k*50, y_start + i*20, if(i < 3) 2 else 1))
          }
        }
      case 3 => // 工字形
        var xNum = 16                              // x轴砖块第一层数量
        var yNum = 9                                // y轴砖块层数
        // 循环y轴
        for(i <- 0 until yNum){
          // 修改每层x轴砖块数量
          if (i == 0) {
            xNum = xNum
          } else if (i > 4) {
            xNum += 2
          } else {
            xNum -= 2
          }
          x_start = (xNum_max - xNum)/2 * 50             // 修改每层x轴砖块起始坐标
          // 循环x轴
          for(k <- 0 until xNum){
            arr.append((x_start + k*50, y_start + i*20, if(i < 3) 2 else 1))
          }
        }
    }

    val blocks = ListBuffer.empty[Block]
    var allValue = 0
    arr.foreach {
      item =>
        if(item._3 == 1) {
          blocks.append(Block(item._1, item._2))
          allValue += 1
        }
        else {
          blocks.append(Block(item._1, item._2, 2))
          allValue += 2
        }
    }
    (blocks.toList, allValue)
  }


  private[this] def updateBreakouts(isFront: Boolean) = {
    def updateABreakout(breakout: SkDt, actMap: Map[Long, Int], isFront: Boolean) = {
      val p = breakout.paddle
      val b = breakout.ball
      val keyCode = actMap.get(breakout.id)
      keyCode match {
        case Some(KeyEvent.VK_LEFT) => p.moveLeft(model)
        case Some(KeyEvent.VK_RIGHT) => p.moveRight(model)
        case _ =>
      }
      // 小球碰撞挡板检测
      if (p.collide(b)) {
        // 当小球运动方向趋向挡板中心时，Y轴速度取反，反之则不变
        if (Math.abs(b.y + BallSize.h/2 - p.y + PaddleSize.h/2) > Math.abs(b.y + BallSize.h/2 + b.speedY - p.y + PaddleSize.h/2)) {
          b.speedY *= -1
        } else {
          b.speedY *= 1
        }
        // 设置X轴速度
        b.speedX = p.collideRange(b).toInt
      }
      // 小球碰撞砖块检测
      breakout.blocks.foreach {
        item =>
          if (item.collide(b)) { // 小球、砖块已碰撞
            // 当小球运动方向趋向砖块中心时，速度取反，反之则不变
            if ((b.y < item.y && b.speedY < 0) || (b.y > item.y && b.speedY > 0)) {
              if (!item.collideBlockHorn(b)) {
                b.speedY *= -1
              } else { // 当小球撞击砖块四角时，Y轴速度不变
                b.speedY *= 1
              }
            } else {
              b.speedY *= 1
            }
            // 当小球撞击砖块四角时，X轴速度取反
            if (item.collideBlockHorn(b)) {
              b.speedX *= -1
            }
            // 计算分数
            breakout.score.computeScore(breakout.blocks)
          }
      }
      // 挡板移动时边界检测
      if (p.x <= 0) { // 到左边界时
        p.isLeftMove = false
      } else {
        p.isLeftMove = true
      }
      if (p.x >= 1000 - PaddleSize.w) { // 到右边界时
        p.isRightMove = false
      } else {
        p.isRightMove = true
      }
      // 移动小球
      b.move(breakout.score, model)
      breakout.copy(blocks = breakout.blocks.filter(_.alive))
    }

    val acts = actionMap.getOrElse(frameCount, Map.empty[Long, Int])
    val temp = breakouts
    temp.foreach {
      t =>
        breakouts = breakouts.updated(t._1, updateABreakout(t._2, acts, isFront))
    }

    val act = actionMap.getOrElse(frameCount + 1, Map.empty[Long, Int])
    act.filter(a => a._2 == KeyEvent.VK_LEFT || a._2 == KeyEvent.VK_RIGHT).foreach {
      a =>
        if(breakouts.exists(b => b._1 == a._1 && b._2.paddle.x > 0 && b._2.paddle.x < 1000 - PaddleSize.w)) {
          breakouts(a._1).paddle.speed = if (a._2 == KeyEvent.VK_LEFT) -20 else 20
        }
    }

    maxScore = breakouts.map(b => (b._1, b._2.score.allScore)).toList.maxBy(_._2)._1
    if(!isFront) {
      val tempBs = breakouts
      tempBs.foreach {
        b =>
          if (b._2.score.lv == Level.OVER) {
            breakouts -= b._1
            breakouts += b._1 -> initScene(b._2.id, b._2.name)
            if(model == 2)
              deadCount += 1
          }
          if (b._2.blocks.isEmpty) {
            if (b._2.score.lv == Level.LEVELTHREE)
              winner = b._1
            breakouts -= b._1
            breakouts += b._1 -> initScene(b._2.id, b._2.name, if (b._2.score.lv != Level.LEVELTHREE) b._2.score.lv + 1 else Level.DEFAULT)
            if(model == 2) {
              deadCount = 3
            }
          }
      }
    }
  }

  def checkRush(id: Long, offsetTime: Double): Boolean = {
    if(breakouts.exists(_._1 == id)) {
      val breakout = breakouts.find(_._1 == id).get._2
      val p = breakout.paddle.copy()
      val b = breakout.ball.copy()
      b.x = b.x - b.speedX * offsetTime / Protocol.frameRate
      b.y = b.y - b.speedY * offsetTime / Protocol.frameRate
      // 小球碰撞挡板检测
      if (p.collide(b)) {
        return true
      }
      // 小球碰撞砖块检测
      breakout.blocks.foreach {
        item =>
          if (item.collide(b, true)) { // 小球、砖块已碰撞
            return true
          }
      }
      // 挡板移动时边界检测
      if (p.x <= 0) { // 到左边界时
        return true
      }
      if (p.x >= 1000 - PaddleSize.w) { // 到右边界时
        return true
      }
    }
    return false
  }

  def getGridData: Protocol.GridDataSync = {
    Protocol.GridDataSync(
      frameCount,
      breakouts.values.toList
    )
  }


}

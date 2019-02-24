package com.neo.sk.Breakout



/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:48 PM
  */
package object Breakout {

  sealed trait Spot

  // 定义挡板对象
  case class Paddle(
                     var x: Double,                                   // x轴坐标
                     var y: Double,                                   // y轴坐标
                     var speed: Int = 0,                             // x轴移动速度
                     ballSpeedMax: Int = 16,                       // 小球反弹速度最大值
                     var isLeftMove: Boolean = true,                  // 能否左移
                     var isRightMove: Boolean = true                  // 能否右移
                   ) {
    def moveLeft () = {
      x += -20
      if (this.x <= 0) {
        x = 1
      }
    }

    def moveRight () = {
      x += 20
      if(this.x >= 1000 - PaddleSize.w) {
        x = 999 - PaddleSize.w
      }
    }

    // 小球、挡板碰撞检测
    def collide(ball: Ball): Boolean = {
      val b = ball
      val p = this
      if (Math.abs((b.x + BallSize.w/2) - (p.x + PaddleSize.w/2)) < (BallSize.w + PaddleSize.w)/2 &&
        Math.abs((b.y + BallSize.h/2) - (p.y + PaddleSize.h/2)) < (BallSize.h + PaddleSize.h)/2) {
        return true
      }
      return false
    }

    // 计算小球、挡板碰撞后x轴速度值
    def collideRange(ball: Ball): Double = {
      val b = ball
      val p = this
      var rangeX = 0D
      rangeX = (p.x + PaddleSize.w/2) - (b.x + BallSize.w/2)
      if (rangeX < 0) { // 小球撞击挡板左侧
        return rangeX / (BallSize.w/2 + PaddleSize.w/2) * p.ballSpeedMax
      } else if(rangeX ==0) {
        return 0.5 * p.ballSpeedMax
      } else { // 小球撞击挡板右侧
        return rangeX / (BallSize.w/2 + PaddleSize.w/2) * p.ballSpeedMax
      }
    }
  }
  object PaddleSize{
    val w: Int = 102                               // 图片宽度
    val h: Int = 22                                // 图片高度
  }

  // 小球对象
  case class Ball(
                   var x: Double,                                   // x轴坐标
                   var y: Double,                                   // y轴坐标
                   var speedX: Int = 0,                             // x轴速度
                   var speedY: Int = 0,                             // y轴速度
                   var fired: Boolean = false                       // 是否运动，默认静止不动
                 ) {
    def move(s: Score) = {
      if (this.fired) {
        // 碰撞边界检测
        if (this.x <= 0 || this.x >= 1000 - BallSize.w) {
          this.speedX *= -1
        }
        if (this.y <= 0) {
          this.speedY *= -1
        }
        if (this.y >= 500 - BallSize.h) {
          // 游戏结束
          s.lv = Level.OVER
        }
        // 移动
        this.x -= this.speedX
        this.y -= this.speedY
      }
    }
  }
  object BallSize{
    val w: Int = 18                                // 图片宽度
    val h: Int = 18                                // 图片高度
  }

  // 砖块
  case class Block(
                    x: Double,                                   // x轴坐标
                    y: Double,                                   // y轴坐标
                    var life: Int = 1,                           // 生命值
                    var alive: Boolean = true                    // 是否存活
                  ) {
    // 消除砖块
    def kill() = {
      this.life -= 1
      if (this.life == 0) {
        this.alive = false
      }
    }

    // 小球、砖块碰撞检测
    def collide(ball: Ball, onlyCheck: Boolean = false): Boolean = {
      var b = ball
      if (Math.abs((b.x + BallSize.w/2) - (this.x + BlockSize.w/2)) < (BallSize.w + BlockSize.w)/2 &&
        Math.abs((b.y + BallSize.h/2) - (this.y + BlockSize.h/2)) < (BallSize.h + BlockSize.h)/2) {
        if(!onlyCheck)
          this.kill()
        return true
      } else {
        return false
      }
    }
    // 计算小球、砖块碰撞后x轴速度方向
    def collideBlockHorn(ball: Ball): Boolean = {
      val b = ball    // 小球
      val bk = this   // 砖块
      val rangeX = Math.abs((b.x + BallSize.w/2) - (bk.x + BlockSize.w/2))
      val rangeY = Math.abs((b.y + BallSize.h/2) - (bk.y + BlockSize.h/2))
      if (rangeX > BlockSize.w/2 && rangeX < (BlockSize.w/2 + BallSize.w/2) && rangeY < (BlockSize.h/2 + BallSize.h/2)) { // X轴方向与砖块四角相交
        if (b.x < bk.x && b.speedX > 0 || b.x > bk.x && b.speedX < 0) { // 小球在砖块左侧时
          return false
        } else { // 小球在砖块右侧
          return true
        }
      }
      return false
    }
  }
  object BlockSize{
    val w: Int = 50                               // 图片宽度
    val h: Int = 20                               // 图片高度
  }

  // 计分板
  case class Score(
                    allValue: Int,
                    var allScore: Int = 0,                               // 总分
                    var lv: Int = Level.DEFAULT                              // 当前关卡
                  ) {
    // 计算总分
    def computeScore(blocks: List[Block]) = {
      val num = allValue - blocks.map(_.life).sum
      this.allScore = 100 * num
    }
  }

//  case class Body(id: Long, life: Int) extends Spot
//  case class Header(id: Long, life: Int) extends Spot
//  case class Apple(score: Int, life: Int) extends Spot
//
//  case class Score(id: Long, n: String, k: Int, l: Int, t: Option[Long] = None)
//  case class Bd(id: Long, life: Int, x: Int, y: Int)
//  case class Ap(score: Int, life: Int, x: Int, y: Int)
//
//
//
  case class Point(x: Double, y: Double) {
    def +(other: Point) = Point(x + other.x, y + other.y)

    def -(other: Point) = Point(x - other.x, y - other.y)

    def *(n: Int) = Point(x * n, y * n)

    def %(other: Point) = Point(x % other.x, y % other.y)
  }
//
//
//  class Snake(x: Int, y: Int, len: Int = 5, d: Point = Point(1, 0)) {
//    var length = len
//    var direction = d
//    var header = Point(x, y)
//  }
//
  case class SkDt(
                  id: Long,
                  name: String,
                  paddle: Paddle,
                  ball: Ball,
                  blocks: List[Block],
                  score: Score
  )


  object Boundary{
    val w = 1000
    val h = 500
  }

  object Level {
    val LEVELONE = 1
    val LEVELTWO = 2
    val LEVELTHREE = 3
    val DEFAULT = 1
    val OVER = 0
  }





}

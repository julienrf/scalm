package mario

import org.scalajs.dom.{document, window}
import scalm.Html._
import scalm._
import scala.math._
import Html._

object Main extends App {

  def main(args: Array[String]): Unit =
    Scalm.start(this, document.querySelector("#mario"))

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Mario(x: Double, y: Double, vx: Double, vy: Double, dir: Direction)

  type Model = Mario

  def init: (Model, Cmd[Msg]) = (Mario(0, 0, 0, 0, Right), Cmd.Empty)

  sealed trait Msg
  case object PassageOfTime extends Msg
  case object ArrowLeftPressed extends Msg
  case object ArrowRightPressed extends Msg
  case object ArrowLeftReleased extends Msg
  case object ArrowRightReleased extends Msg
  case object ArrowUpPressed extends Msg
  case object Void extends Msg

  val gravity = 0.25
  val applyGravity: Mario => Mario = (mario) =>
    mario.copy(vy = if (mario.y > 0) mario.vy - gravity else 0)

  val applyMotion: Mario => Mario = (mario: Model) =>
    mario.copy(x = mario.x + mario.vx, y = max(0.0, mario.y + 3 * mario.vy))

  val walkLeft: Model => Model = _.copy(vx = -1.5, dir = Left)

  val walkRight: Model => Model = _.copy(vx = 1.5, dir = Right)

  val jump: Model => Model = _.copy(vy = 6.0)

  val applyPhysics: Model => Model = applyGravity compose applyMotion

  def update(msg: Msg, model: Model): (Model, Cmd[Msg]) =
    msg match {
      case ArrowUpPressed if model.y == 0.0 =>
        val newModel = (jump andThen applyPhysics)(model)
        (newModel, Effects.Cmd.playSound("resources/jump-c-07.mp3", Void))

      case ArrowLeftPressed =>
        val newModel = (walkLeft andThen applyPhysics)(model)
        (newModel, Cmd.Empty)

      case ArrowRightPressed =>
        val newModel = (walkRight andThen applyPhysics)(model)
        (newModel, Cmd.Empty)

      case ArrowLeftReleased if model.dir == Left =>
        val newModel = model.copy(vx = 0.0)
        (newModel, Cmd.Empty)

      case ArrowRightReleased if model.dir == Right =>
        val newModel = model.copy(vx = 0.0)
        (newModel, Cmd.Empty)

      case PassageOfTime => (applyPhysics(model), Cmd.Empty)
      case _             => (model, Cmd.Empty)
    }

  def subscriptions(model: Model): Sub[Msg] = {
    val keyLeftPressSub = Effects.keyPressSub(37, ArrowLeftPressed)
    val keyRightPressSub = Effects.keyPressSub(39, ArrowRightPressed)
    val keyLeftReleaseSub = Effects.keyReleaseSub(37, ArrowLeftReleased)
    val keyRightReleaseSub = Effects.keyReleaseSub(39, ArrowRightReleased)
    val keyUpSub = Effects.keyPressSub(38, ArrowUpPressed)
    val fpsSub = Effects.requestAnimationFrameSub.map(_ => PassageOfTime)

    Sub
      .Combine(fpsSub, keyUpSub)
      .combine(keyLeftPressSub)
      .combine(keyRightPressSub)
      .combine(keyLeftReleaseSub)
      .combine(keyRightReleaseSub)
  }

  def view(model: Model): Html[Msg] = {

    val posX = (window.innerWidth - 85) / 2 + model.x
    val posY = (window.innerHeight - 85) - model.y

    val verb = (model.y > 0, model.vx != 0) match {
      case (true, _) => "jump"
      case (_, true) => "walk"
      case _         => "stand"
    }

    val dir = model.dir.toString.toLowerCase
    val transform =
      s"transform: matrix(1, 0, 0, 1, $posX, $posY)"
    val css =
      s"padding: 0px; margin: 0px; display: block; width: 35px; height: 35px; position: absolute; opacity: 1; $transform; background-color: transparent;"

    div()(
      img(style(css), src(s"resources/mario/$verb/$dir.gif"))
    )
  }
}

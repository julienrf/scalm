package mario

import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent, document}
import org.scalajs.dom.raw.{Event, HTMLAudioElement, TouchEvent}
import scalm.{Cmd, Sub, Task}
import scalm.Sub.ofTotalObservable

object Effects {

  val requestAnimationFrameSub: Sub[Double] = ofTotalObservable[Double](
    "requestAnimation", { observer =>
      var handle = 0
      def loop(time: Double): Unit = {
        observer.onNext(time)
        handle = dom.window.requestAnimationFrame(loop)
      }
      handle = dom.window.requestAnimationFrame(loop)
      () =>
        dom.window.cancelAnimationFrame(handle)
    }
  )

  def keyPressSub(keyCode: Int): Sub[KeyboardEvent] =
    Sub.fromEvent[KeyboardEvent, KeyboardEvent]("keydown", dom.window) {
      event =>
        if (event.keyCode == keyCode) Some(event) else None
    }

  def keyReleaseSub(keyCode: Int): Sub[KeyboardEvent] =
    Sub.fromEvent[KeyboardEvent, KeyboardEvent]("keyup", dom.window) { event =>
      if (event.keyCode == keyCode) Some(event) else None
    }

  val touchStartSub: Sub[(Double, Double)] =
    Sub.fromEvent[TouchEvent, (Double, Double)]("touchstart", dom.window) {
      event =>
        val first = event.touches.item(0)
        Some(first.clientX, first.clientY)
    }

  val touchEndSub: Sub[(Double, Double)] =
    Sub.fromEvent[TouchEvent, (Double, Double)]("touchend", dom.window) {
      event =>
        val first = event.changedTouches.item(0)
        Some(first.clientX, first.clientY)
    }

  object Cmd {
    def playSound(url: String): Cmd[Nothing] =
      Task
        .RunObservable[Nothing, Nothing] { _ =>
          {
            val audio =
              document.createElement("audio").asInstanceOf[HTMLAudioElement]
            audio.src = url
            audio.onloadeddata = (_: Event) => audio.play()
            () =>
              ()
          }
        }
        .attempt(_.merge)
  }

}

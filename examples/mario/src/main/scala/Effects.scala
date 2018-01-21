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

  def keyPressSub[M](keyCode: Int, msg: M): Sub[M] = ofTotalObservable[M](
    s"keyDown$keyCode", { observer =>
      dom.window.addEventListener("keydown", (keyEvent: KeyboardEvent) => {
        if (keyEvent.keyCode == keyCode) observer.onNext(msg)
      })
      () =>
        ()
    }
  )

  def keyReleaseSub[M](keyCode: Int, msg: M): Sub[M] = ofTotalObservable[M](
    s"keyUp$keyCode", { observer =>
      dom.window.addEventListener("keyup", (keyEvent: KeyboardEvent) => {
        if (keyEvent.keyCode == keyCode) observer.onNext(msg)
      })
      () =>
        ()
    }
  )

  object Cmd {
    def playSound[Msg](url: String, msgBack: Msg): Cmd[Msg] =
      Task
        .RunObservable[Unit, Msg] { _ =>
          {
            val audio =
              document.createElement("audio").asInstanceOf[HTMLAudioElement]
            audio.src = url
            audio.onloadeddata = (_: Event) => audio.play()
            () =>
              ()
          }
        }
        .attempt(_ => msgBack)
  }

}

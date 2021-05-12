package snabbdom

import org.scalajs.dom.{Element, Text}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Default
import scala.scalajs.js.|

import scala.annotation.nowarn

@JSImport("snabbdom", Default)
@js.native
object snabbdom extends js.Object {
  @nowarn
  def init(modules: js.Array[js.Object]): js.Function2[VNode | Element, VNode, VNode] = js.native
}

@JSImport("snabbdom/h", Default)
@js.native
object h extends js.Function3[String, js.UndefOr[js.Any], js.UndefOr[js.Any], VNode] {
  def apply(selector: String, b: js.UndefOr[js.Any] = js.undefined, c: js.UndefOr[js.Any] = js.undefined): VNode = js.native
}

@js.native
trait VNode extends js.Object {
  val selector: js.UndefOr[String]
  val data: js.UndefOr[VNodeData]
  val children: js.UndefOr[js.Array[VNode | String]]
  val text: js.UndefOr[String]
  val elm: js.UndefOr[Element | Text]
  val key: js.UndefOr[String | Double]
}

@js.native
trait VNodeData extends js.Object

// --- Convenient syntax

trait SnabbdomSyntax extends Any {
  final def e(selector: String, opts: js.UndefOr[js.Object] = js.undefined): VNode =
    _root_.snabbdom.h(selector, opts)

  final def h(selector: String, opts: js.Object)(children: VNodeParam*): VNode =
    _root_.snabbdom.h(
      selector,
      opts,
      js.Array(children.flatMap(_.asVnodes): _*)
    )

  final def h(selector: String)(children: VNodeParam*): VNode =
    h(selector, js.Dynamic.literal())(children: _*)
}

sealed trait VNodeParam {
  def asVnodes: Seq[String | VNode]
}

object VNodeParam {
  implicit class Text(s: String) extends VNodeParam {
    def asVnodes = Seq(s)
  }
  implicit class Node(vnode: VNode) extends VNodeParam {
    def asVnodes = Seq(vnode)
  }
  implicit class Nodes(ps: Seq[VNode]) extends VNodeParam {
    def asVnodes = ps.flatMap(_.asVnodes)
  }
}

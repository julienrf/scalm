package mario

import scalm.Html.tag
import scalm.{Attr, Html, Prop}

object Html {

  def img[M](attrs: Attr[M]*): Html[M] = tag("img")(attrs: _*)()
  def src(uri: String): Prop = Prop("src", uri)
}

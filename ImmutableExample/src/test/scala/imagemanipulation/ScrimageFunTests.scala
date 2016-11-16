package imagemanipulation

import functionalpresentation.{Home, Restaurant, School}
import org.scalatest.FlatSpec

import pprint.Config
import scala.util.Try

/**
  * Created by bfrasure on 11/15/16.
  */
class ScrimageFunTests extends FlatSpec {

  "scrimage" should "write lines of text on an image" in {

    val content = List(
      "this is just a few",
      "random lines of text",
      "that I want to draw on an image",
      "lets keep makin em!",
      "more and more and more text!"
    )
    ScrimageFun.makeImgFromText(content)
  }

}

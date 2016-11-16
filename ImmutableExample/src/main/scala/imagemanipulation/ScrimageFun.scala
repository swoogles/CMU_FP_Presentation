package imagemanipulation

import com.sksamuel.scrimage.ScaleMethod.FastScale
import com.sksamuel.scrimage.filter.TwirlFilter
import com.sksamuel.scrimage.filter.EdgeFilter
import com.sksamuel.scrimage.filter.SnowFilter
import com.sksamuel.scrimage.filter.OilFilter
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.canvas.Canvas
import java.awt.{Color => JColor, Font => JFont, Image => JImage}

import com.sksamuel.scrimage.filter.{RippleFilter, RippleType}
import com.sksamuel.scrimage.canvas.Canvas._
import com.sksamuel.scrimage.canvas.Font
import com.sksamuel.scrimage.canvas.drawable.Text
import com.sksamuel.scrimage.Tag
import com.sksamuel.scrimage.Directory

// Needed for SnowFilter for some weird reason.
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by bfrasure on 11/15/16.
  */
object ScrimageFun {
  import ammonite.ops._

  val originalImgDir = ammonite.ops.pwd / up / 'OriginalImages
  val listed = ls! originalImgDir
  val manipulatedImgDir = cwd / up / 'ManipulatedImages

  def copyFreshImages_!(srcDir: Path, targetDir: Path): LsSeq = {
    val oldImages = ls! targetDir
    oldImages.map{ rm! _ }
    mkdir! targetDir
    for ( file <- (ls! srcDir) ) { cp.into(file, targetDir)  }
    ls! targetDir
  }

  val imgFont = new JFont("Sans-seriff", 1, 36)

  val imgTextNew =
    Text("Happy Holidays!", 200, 800-20, { g2 =>
      g2.setBackground(JColor.WHITE)
      g2.setFont(imgFont)
    })

  def makeTextDrawable(content: List[String]) = {
    content.zipWithIndex.map { case (lineContent, lineIdx) =>
      Text(lineContent, 200, 100 + (lineIdx * 30), { g2 =>
        g2.setBackground(JColor.WHITE)
        g2.setFont(imgFont)
      })
    }
  }


  val borderFunc: (Image=>Image) = (imgInner) => {
    val totalBorderSize = 150
    val borderStripeSize = 10
    Range(0, totalBorderSize / borderStripeSize).foldLeft(imgInner) { (curImg, idx) =>
      val borderStripeColor =
        if ( idx % 2 == 0) Color(200, 50, 50)
        else Color.Black

      curImg.pad(borderStripeSize, borderStripeColor)
    }
  }

  val rippleAmplitude = 30f
  val rippleWaveLength = 50f



  def makeArtGallery() = {
    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img = Image.fromFile(imgPath.toIO)
           .filter(OilFilter(5))
           .pad(100, Color(200, 50, 50))
           .pad(100, Color.Black)
           .pad(100, Color(200, 50, 50))

    ) { img.output(imgPath.toIO)(JpegWriter()) }
  }

  def downScaleImages() = {

    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img = Image.fromFile(imgPath.toIO)
           .fit(1000, 800, Color.Black)
    ) { img.output(imgPath.toIO)(JpegWriter())}
  }

  def crazyFrame() = {

    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img =
         borderFunc(Image.fromFile(imgPath.toIO)
           .fit(1000, 800, Color.Black))
    ) { img.output(imgPath.toIO)(JpegWriter())}
  }


  def makeGreetingCards() = {
    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img = Image.fromFile(imgPath.toIO)
           .filter(SnowFilter())
           .fit(800, 600, Color.Black)
           .pad(100, Color.Black)
           .draw(imgTextNew)
    ) { img.output(imgPath.toIO)(JpegWriter())}
  }

  def makeImgFromText(content: List[String]) = {
    println("Going to draw: ")
    println(content.mkString("\n"))
    val drawableText = makeTextDrawable(content)
    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img: Canvas = Image(800, 600)
           .fit(800, 600, Color.Black)
           .pad(100, Color.Black)
    ) {
      val imgWith1Line = img.draw(drawableText(0))
      val imgWith2Line = imgWith1Line.draw(drawableText(1))
      val imgWith3Line = imgWith2Line.draw(drawableText(2))

//      val imgWithText: Canvas = drawableText.fold(img){
//        case (curImg: Canvas, nextText: Text) => curImg.draw(nextText)
//      }

      imgWith3Line.output(imgPath.toIO)(JpegWriter())}
  }
}

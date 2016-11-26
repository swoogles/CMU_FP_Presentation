package billding.fp.immutable

import billding.fp.{Home, Restaurant, School}
import org.scalatest.FlatSpec

import scala.util.Try
import pprint.Config

class ImmutableCarTestsNonpresentation extends FlatSpec {
  implicit val pprintConfig = Config()

  val SAM = Person("Sam", Home)
  val JOE = Person("Joe", Home)
  val CAR = Car(100, Home)
  val SCENE = Scene(JOE, SAM, CAR)

  "Driving" should "not be possible if you aren't with the car" in {
    val scene = Scene(JOE, SAM, CAR.copy(location=Restaurant))
    val sceneResult = Scenarios.processScenes(
      scene,
      Travel(joe=Home, sam=Restaurant)
    )
    assert(sceneResult.isFailure)
  }

  "Driving" should "fail if both people want to travel to different places." in {
    val sceneResult = Scenarios.processScenes(
      SCENE,
      Travel(joe=School, sam=Restaurant)
    )
    assert(sceneResult.isFailure)

  }

//  "Driving" should "fail if joe comes to sam, and then sam's movement is applied" in {
//    println("test of interest")
//        val startScene = Scene(JOE.copy(location = School), SAM, CAR.copy(location=School))
//    val sceneResult = Scenarios.processScenesTyped(
//      startScene,
//      List(
//        SceneUpdate(joe=Home, sam=Restaurant)
//      )
//    )
//    println("done with test body")
//    assert(sceneResult.isFailure)
//
//  }

  "Driving" should "cease when you run out of gas" in {
    val sceneResult = Scenarios.processScenesCumulative(
      SCENE,
      Travel(joe=Home, sam=Restaurant),
      Travel(joe=Home, sam=School),
      Travel(joe=Home, sam=Home),
      Travel(joe=Home, sam=School),
      Travel(joe=Home, sam=Home),
      Travel(joe=School, sam=Home)
    )

    pprint.pprintln(sceneResult)
    assert(sceneResult.last.isFailure, true)
  }


  "Driving" should "chain" in {

    val sceneResult = SCENE
      .update(Travel(joe=Home, sam=School))
      .update(Travel(joe=Home, sam=Home))
      .update(Travel(joe=Home, sam=School))
      .update(Travel(joe=Home, sam=Home))
      .update(Travel(joe=School, sam=Home))

    println("Chained result:")
    pprint.pprintln(sceneResult)
//    assert(sceneResult.last.isFailure, true)
  }

//  "Driving" should "handle failures and apply valid movements as possible" in {
//    val startScene = Scene(SAM, JOE, CAR)
//    val sceneResult = Scenarios.processScenesFaultTolerantTyped(
//      startScene,
//      SceneUpdate(joe=School, sam=Restaurant), // Fail - Diverging locations
//      SceneUpdate(joe=Home, sam=School),
//      SceneUpdate(joe=Home, sam=Home),
//      SceneUpdate(joe=School, sam=Restaurant), // Fail - Diverging locations
//      SceneUpdate(joe=Home, sam=School),
//      SceneUpdate(joe=Home, sam=Home),
//      SceneUpdate(joe=Home, sam=School),
//      SceneUpdate(joe=Home, sam=Home),
//      SceneUpdate(joe=School, sam=Home)
//    )
//
//    pprint.pprintln(sceneResult)
//  }

  // TODO put in separate test since it doesn't involve scene updating.
//  "A car" should "should always have fuel after fuel check." in {
//    assert(CAR.fuel == 100)
//    TravelFunctions.drive(SAM, CAR, Restaurant) // Does nothing, since the result is ignored
//    assert(CAR.fuel == 100)
//  }

    "Driving" should "cease when you run out of gas, but preserve last state" in {
      val sceneResult = Scenarios.processScenesKeepLastGoodStateScene(
        SCENE,
        Travel(joe=Home, sam=Restaurant),
        Travel(joe=Home, sam=School),
        Travel(joe=Home, sam=Home),
        Travel(joe=Home, sam=School),
        Travel(joe=Home, sam=Home),
        Travel(joe=School, sam=Home)
      )

      pprint.pprintln(sceneResult)
      sceneResult match {
        case Right(nonBrokenScene) => fail()
        case Left(failedSceneWithState) => println("Failed scene: " + failedSceneWithState)
      }
    }

}

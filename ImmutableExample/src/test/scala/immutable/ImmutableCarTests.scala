package billding.fp.immutable

import billding.fp.{Home, Restaurant, School}
// import functionalpresentation.{Home, Restaurant}
import org.scalatest.FlatSpec
import pprint.Config

import scala.util.Try

class ImmutableCarTests extends FlatSpec {
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

  "Travel" should "be impossible if both people want to travel to different places." in {
    val sceneResult = Scenarios.processScenes(
      SCENE,
      Travel(joe=School, sam=Restaurant)
    )
    assert(sceneResult.isFailure)

  }

  "The car" should "cease driving when you run out of gas" in {
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

  "Scenes" should "support a mix of Refueling, Waiting, and Travelling" in {
    val sceneResult = Scenarios.processScenesCumulative(
      SCENE,
      Travel(joe=Home, sam=Restaurant),
      Travel(joe=Home, sam=School),
      Travel(joe=Home, sam=Home),
      Refuel,
      Travel(joe=Home, sam=School),
      Travel(joe=Home, sam=Home),
      Travel(joe=School, sam=Home)
    )

    pprint.pprintln(sceneResult)
    assert(sceneResult.last.isSuccess)
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
  }

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

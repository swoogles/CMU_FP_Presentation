package immutable

import org.scalatest.FlatSpec

import scala.util.Try
import pprint.Config

class ImmutableMotorcycleTests extends FlatSpec {
  implicit val pprintConfig = Config()

  val SAM = Person("Sam", Home)
  val JOE = Person("Joe", Home)
  val CAR = Motorcycle(100, Home)

  "Driving" should "not be possible if you aren't with the car" in {
    val sceneResult = Scenarios.processMovements(
      SAM, JOE, CAR.copy(location=Restaurant),
      SceneUpdate(joe=Home, sam=Restaurant)
    )
    assert(sceneResult.isFailure, true)
    sceneResult
  }

  "Driving" should "fail if both people want to travel to different places." in {
    val sceneResult = Scenarios.processMovements(
      SAM, JOE, CAR,
      SceneUpdate(joe=School, sam=Restaurant)
    )
    assert(sceneResult.isFailure, true)

  }

  "Driving" should "cease when you run out of gas" in {
    val sceneResult = Scenarios.processScenesCumulative(
      SAM, JOE, CAR,
      SceneUpdate(joe=Home, sam=Restaurant),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=School, sam=Home)
    )

    pprint.pprintln(sceneResult)
    assert(sceneResult.last.isFailure, true)
  }

  "Driving" should "cease when you run out of gas, but preserve last state" in {
    val sceneResult = Scenarios.processScenesKeepLastGoodState(
      SAM, JOE, CAR,
      SceneUpdate(joe=Home, sam=Restaurant),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=School, sam=Home)
    )

    pprint.pprintln(sceneResult)
    sceneResult match {
      case Right(nonBrokenScene) => fail()
      case Left(failedSceneWithState) => println("Failed scene: " + failedSceneWithState)
    }
  }

  "Driving" should "utilize the builder pattern" in {
    val sceneResult = Scenarios.processMovements(
      SAM, JOE, CAR,
      SceneUpdate(joe=Home, sam=Restaurant),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=School, sam=Home)
    )

    assert(sceneResult.isFailure)
  }

//  "Driving" should "should be accomplished with scenes" in {
//    val startScene = Scene(SAM, JOE, CAR)
//    val sceneResult = Scenarios.processScenesFaultTolerantTyped(
//      startScene,
//      SceneUpdate(joe=Home, sam=Restaurant),
//      SceneUpdate(joe=Home, sam=School),
//      SceneUpdate(joe=Home, sam=Home),
//      SceneUpdate(joe=Home, sam=School),
//      SceneUpdate(joe=Home, sam=Home),
//      SceneUpdate(joe=School, sam=Home)
//    )
//
//    pprint.pprintln(sceneResult)
//  }

  "Driving" should "handle failures and apply valid movements as possible" in {
    val startScene = Scene(SAM, JOE, CAR)
    val sceneResult = Scenarios.processScenesFaultTolerantTyped(
      startScene,
      SceneUpdate(joe=School, sam=Restaurant), // Fail - Diverging locations
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=School, sam=Restaurant), // Fail - Diverging locations
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=Home, sam=School),
      SceneUpdate(joe=Home, sam=Home),
      SceneUpdate(joe=School, sam=Home)
    )

    pprint.pprintln(sceneResult)
  }

  "A car" should "should always have fuel after fuel check." in {
    assert(CAR.fuel == 100)
    TravelFunctions.drive(SAM, CAR, Restaurant) // Does nothing, since the result is ignored
    assert(CAR.fuel == 100)
  }

}
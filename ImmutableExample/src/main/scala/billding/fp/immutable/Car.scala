package billding.fp.immutable

import billding.fp.Location

import scala.util.{Failure, Success, Try}

case class Car(fuel: Int, location: Location)
case class Person(name: String, location: Location)

case class Scene(joe: Person, sam: Person, car: Car, t: Int = 0) {
  def update(intentions: SceneUpdate) : Scene =
    Scenarios.updateScene(this, intentions) match {
      case Success(scene) => scene
      case Failure(ex) => this
    }
  def updateTry(intentions: SceneUpdate) : Try[Scene] =
    Scenarios.updateScene(this, intentions)
}

case class SceneWithFailedMoves(scene: Scene, failedMoves: List[(Scene, SceneUpdate, Throwable)])

sealed trait SceneUpdate {
  val time: Int
}
case object Refuel extends SceneUpdate {
  val time = 1
}
case class Wait(time: Int) extends SceneUpdate
case class Travel(joe: Location, sam: Location, time: Int = 5) extends SceneUpdate

trait TravelBehavior {
  def drive(person: Person, car: Car, destination: Location): Try[(Person, Car)]
  def fill(person: Person, car: Car): Try[Car]
}

trait ScenarioActions {
  def updateScene(scene: Scene, intentions: SceneUpdate) : Try[Scene]
  def processScenes(scene: Scene, intentions: List[SceneUpdate]) : Try[Scene]
  def processScenesCumulative(scene: Scene, intentions: List[SceneUpdate]) : List[Try[Scene]]
  def processScenesKeepLastGoodState(scene: Scene, intentions: List[SceneUpdate]) : Either[(Throwable, Scene), Scene]
}

object TravelFunctions extends TravelBehavior {
  val tripCost = 20

  private def move(car: Car, destination: Location): Try[Car] =
    car match {
      case matchedCar: Car if (matchedCar.location == destination) => Success(matchedCar)
      case matchedCar: Car if (matchedCar.fuel >= tripCost) => Success(matchedCar.copy(fuel= matchedCar.fuel - tripCost, location=destination))
      case emptyCar => Failure(new Exception("Not enough fuel for the trip!"))
    }

  def drive(person: Person, car: Car, destination: Location): Try[(Person, Car)] =
    person.location match {
      case `destination` => Success((person, car)) // Try to ignore the backticks. Matches the value of destination rather than matching anything and sticking it in a val called destination
      case car.location =>
        TravelFunctions.move(car, destination) flatMap { movedCar =>
          Success((person.copy(location = destination), movedCar))
        }
      case invalidLocation => Failure(new Exception("Car and driver aren't in the same place!"))
    }

  def fill(person: Person, car: Car): Try[Car] =
    person.location match {
      case car.location => Success(car.copy(fuel = 100))
      case invalidLocation => Failure(new Exception("Car and driver aren't in the same place!"))
    }
}

object Scenarios extends ScenarioActions {

  def updateScene(scene: Scene, update: SceneUpdate) : Try[Scene] = {
    val finalTime = scene.t + update.time
    update match {
      case intentions: Travel =>
        for ((newJoe, joeCarResult) <- TravelFunctions.drive(scene.joe, scene.car, intentions.joe);
             (newSam, samCarResult) <- TravelFunctions.drive(scene.sam, scene.car, intentions.sam);
             coherentScene <-
               if (joeCarResult != scene.car && samCarResult != scene.car) {
                 if ( joeCarResult == samCarResult )
                   Success(Scene(newJoe, newSam, samCarResult, finalTime))
                 else
                   Failure(new Exception("Final car positions don't agree!"))
               } else if (joeCarResult != scene.car) {
                   Success(Scene(newJoe, newSam, joeCarResult, finalTime))
               } else {
                   Success(Scene(newJoe, newSam, samCarResult, finalTime))
               }
        ) yield {
          coherentScene
        }
      case Wait(_) => Success(scene)
      case Refuel =>
        val filledCar = scene.car.copy(fuel=100)
        Success(scene.copy(car = filledCar, t=finalTime))
    }
  }

  // TODO Find more use cases for this. I'm glad I managed to extract the duplicate code from processScenesType and
  // processScenesTypedCumulative, but it'd really start to shine if I can get 1 or 2 more useful variations.
  private val sceneUpdateCases = { (curScene: Try[Scene], update: SceneUpdate) =>
    (curScene, update) match {
      case (Success(curScene: Scene), curIntentions) => updateScene(curScene, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }

  def processScenes(scene: Scene, intentions: SceneUpdate*) : Try[Scene] =
    processScenes(scene, intentions.toList)

  def processScenes(scene: Scene, intentions: List[SceneUpdate]) : Try[Scene] =
    intentions.foldLeft(Try(scene))(sceneUpdateCases)

  def processScenesCumulative(scene: Scene, intentions: SceneUpdate*) : List[Try[Scene]] =
    processScenesCumulative(scene, intentions.toList)

  def processScenesCumulative(scene: Scene, intentions: List[SceneUpdate]) : List[Try[Scene]] =
    intentions.scanLeft(Try(scene))(sceneUpdateCases)

  def processScenesFaultTolerant(scene: Scene, intentions: SceneUpdate*) : SceneWithFailedMoves =
    processScenesFaultTolerant(scene, intentions.toList)

  def processScenesFaultTolerant(scene: Scene, intentions: List[SceneUpdate]) : SceneWithFailedMoves = {
    val sceneWithReversedFailedMoves = intentions.foldLeft(SceneWithFailedMoves(scene, List())) {
      case (curScene, curIntentions) =>
        val sceneUpdateAttempt: Try[Scene] = updateScene(curScene.scene, curIntentions)
        sceneUpdateAttempt match {
          case Success(newScene) => curScene.copy(scene = newScene)
          case Failure(ex) => curScene.copy(
            failedMoves = (curScene.scene, curIntentions, ex) :: curScene.failedMoves
          )
        }
    }
    sceneWithReversedFailedMoves.copy(failedMoves = sceneWithReversedFailedMoves.failedMoves.reverse)

  }

  def processScenesKeepLastGoodStateScene(scene: Scene, intentions: SceneUpdate*) : Either[(Throwable, Scene),Scene] =
    processScenesKeepLastGoodState(scene, intentions.toList)

  def processScenesKeepLastGoodState(scene: Scene, intentions: List[SceneUpdate]) : Either[(Throwable, Scene), Scene] = {
    val startState: Either[(Throwable, Scene),Scene] = Right(scene)
    intentions.foldLeft(startState) {
      case (Right(curScene), curIntentions) => {
        updateScene(curScene, curIntentions) match {
          case Success(sceneTuple) => Right(sceneTuple)
          case Failure(ex) => Left((ex, curScene))
        }
      }
      case (Left(lastGoodStateWithException), curIntentions) => Left(lastGoodStateWithException)
    }
  }

}

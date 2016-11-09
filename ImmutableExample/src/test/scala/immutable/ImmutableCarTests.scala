package immutable

import org.scalatest.FlatSpec

import scala.util.Try

class ImmutableCarTests extends FlatSpec {

  val SAM = Person("Sam", Home)
  val JOE = Person("Joe", Home)
  val CAR = Car(100, Home)

  "Driving" should "not be possible if you aren't with the car" in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR.copy(location=Restaurant),
      Intentions(joe=Home, sam=Restaurant)
    )
    assert(sceneResult.isFailure, true)
  }

  "Driving" should "fail if both people want to travel to different places." in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR,
      Intentions(joe=School, sam=Restaurant)
    )
    assert(sceneResult.isFailure, true)

  }

  // TODO test out single person intentions passed to each step as a group
  "Driving" should "cease when you run out of gas" in {
    val sceneResult = Scenarios.processScenesCumulative(
      SAM, JOE, CAR,
      Intentions(joe=Home, sam=Restaurant),
      Intentions(joe=Home, sam=School),
      Intentions(joe=Home, sam=Home),
      Intentions(joe=Home, sam=School),
      Intentions(joe=Home, sam=Home),
      Intentions(joe=School, sam=Home)
    )
    sceneResult foreach println
    assert(sceneResult.last.isFailure, true)
  }

  "Driving" should "allow both people to ride in the same car to the same place" in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR,
      Intentions(Restaurant, Restaurant)
    )
    assert(sceneResult.isSuccess)
  }

  "Occupied Car" should "move car and driver together in a chainable, but obnoxious, way" in {
    val occupiedCar = OccupiedCar(SAM, CAR)
    val chainedResult =
      occupiedCar
        .drive(Restaurant).get
        .drive(School).get
        .drive(Home)

    val driveFunc = (x: OccupiedCar) => x.drive _

    assert(chainedResult.isSuccess)
    println("chainedResult: " + chainedResult)
  }

  "Occupied Car" should "move car and driver together in a chainable, but unsafe, way" in {
    val occupiedCar = OccupiedCar(SAM, CAR)
    val chainedResult =
      occupiedCar
        .driveNoTry(Restaurant)
        .driveNoTry(School)
        .driveNoTry(Home)

//    assert(chainedResult.isSuccess)
    println("chainedResult: " + chainedResult)
  }

  "Driving" should "utilize the builder pattern" in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR,
      Intentions(joe=Home, sam=Restaurant),
      Intentions(joe=Home, sam=School),
      Intentions(joe=Home, sam=Home),
      Intentions(joe=Home, sam=School),
      Intentions(joe=Home, sam=Home),
      Intentions(joe=School, sam=Home)
    )
    val res: Try[(Person, Car)] = Person.drive(SAM, CAR, Restaurant)

    val multiDrivingResult: Try[(Person, Car)] = for (
      (newSam, newCar) <- Person.drive(SAM, CAR, Restaurant);
      (newSam2, newCar2) <- Person.drive(newSam, CAR, Restaurant);
      (newSam3, newCar3) <- Person.drive(newSam2, CAR, Restaurant)
    ) yield  { (newSam3, newCar3)}

//    res map { case (movedperson, drivenCar) =>
//        Person.drive(movedperson, drivenCar, Home)
//    }

    assert(multiDrivingResult.isFailure)
  }

  "A car" should "should always have fuel after fuel check." in {
    assert(CAR.fuel == 100)
    Person.drive(SAM, CAR, Restaurant)
    assert(CAR.fuel == 100)
  }

  def scenes = {
    if (CAR.fuel < 100)
      Person.fill(JOE, CAR)
  }

}

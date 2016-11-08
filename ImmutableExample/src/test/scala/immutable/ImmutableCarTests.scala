package immutable

import org.scalatest.FlatSpec

class ImmutableCarTests extends FlatSpec {

  val SAM = Person("Sam", Home)
  val JOE = Person("Joe", Home)
  val CAR = Car(100, Home)

  "Driving" should "not be possible if you aren't with the car" in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR.copy(location=Restaurant),
      Intentions(Home, Restaurant)
    )
    assert(sceneResult.isFailure, true)
  }

  "Driving" should "fail if both people want to travel to different places." in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR,
      Intentions(School, Restaurant)
    )
    assert(sceneResult.isFailure, true)

  }

  "Driving" should "cease when you run out of gas" in {
    val sceneResult = Scenarios.processScenes(
      SAM, JOE, CAR,
      Intentions(Home, Restaurant),
      Intentions(Home, School),
      Intentions(Home, Home),
      Intentions(Home, School),
      Intentions(Home, Home),
      Intentions(School, Home)
    )
    assert(sceneResult.isFailure, true)
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

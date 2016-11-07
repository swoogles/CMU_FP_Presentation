package com.bfrasure.fp.cmu.immutable

import org.scalatest.FlatSpec

class ImmutableCarTests extends FlatSpec {

  val SAM = Person("Sam", Home)
  val JOE = Person("Joe", Home)
  val CAR = Car(100, Pristine, Home)

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

  def scenes = {
    Scenarios.processScenes(
      SAM, JOE, CAR,
      Intentions(School, Home)
    )

    if (CAR.location == Home) { // Now we're safe. Sort've.
      println("Sam's driving")
      Person.drive(SAM, CAR, School)
    }

    Person.drive(JOE, CAR, Home)

    if (CAR.condition != Pristine)
      Person.clean(JOE, CAR)

    Person.drive(JOE, CAR, JanesHouse)
    Person.drive(JOE, CAR, Home)

    if (CAR.fuel < 100)
      Person.fill(JOE, CAR)
  }

}

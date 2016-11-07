sealed trait Condition
case object Pristine extends Condition
case object Decent extends Condition
case object Trashed extends Condition

sealed trait Location
case object School extends Location
case object Home extends Location
case object Restaurant extends Location
case object JanesHouse extends Location

case class Car(fuel: Int, condition: Condition, location: Location)
case class Person(name: String, location: Location)
object Person {
  def assertWithException(condition: Boolean, msg: String): Unit = {
    if (!condition)
      throw new Exception(msg)
  }

  def drive(person: Person, car: Car, destination: Location): (Person, Car) = {
    assertWithException(person.location == car.location, "Car and driver aren't in the same place!")
    val newCar = Car(car.fuel-20, car.condition, destination)
    val newPerson = Person(person.name, destination)
    if (car.fuel < 20) // This does NOT belong in the Person class.
      throw new Exception("Out of gas!")
    (newPerson, newCar)
  }

  // TODO: Make person parameter relevant in some way.
  //       Possibly just accept that it doesn't have a software need here.
  def clean(person: Person, car: Car): Car =
    car.copy(condition = Pristine)
  def fill(person: Person, car: Car): Car =
    car.copy(fuel = 100)
}

case class Intentions(joeDestination: Location, samDestination: Location)
case class IntentionsAlt(joe: Person, sam: Person) // Rather than giving destinations, you can request a precise end state.

object Scenarios {
  def assertWithException(condition: Boolean, msg: String): Unit = {
    if (!condition)
      throw new Exception(msg)
  }

  val SAM = Person("Sam", Home)
  val JOE = Person("Joe", Home)
  val CAR = Car(100, Pristine, Home)

  

  // Consider 2 version:
  // - This one
  // - Try[ (Person, Person, Car) ] // This one might be overkill for an entry talk.
  def updateScene(joe: Person, sam: Person, car: Car, intentions: Intentions): (Person, Person, Car) = {
    if (joe.location != intentions.joeDestination) {
      assertWithException(joe.location == car.location, "Joe wants to move, but he's not with the car!")

      if (sam.location == intentions.samDestination) { // Sam's staying put
        (joe.copy(location=intentions.joeDestination), sam, car.copy(location=intentions.joeDestination))
      } else {
        assertWithException(sam.location == car.location, "Sam wants to move, but he's not with the car!")
        assertWithException(intentions.joeDestination == intentions.samDestination, "The guys want to go to different places!")
        (joe.copy(location=intentions.joeDestination), sam.copy(location=intentions.samDestination), car.copy(location=intentions.joeDestination))
      }

    } else { // Joe's staying put
      if (sam.location == intentions.samDestination)
        (joe, sam, car) // Nobodies moving
      else {
        assertWithException(sam.location == car.location, "Sam wants to move, but he's not with the car!")
        (joe, sam.copy(location=intentions.samDestination), car.copy(location=intentions.joeDestination))
      }
    }

  }

  def processScenes(joe: Person, sam: Person, car: Car, intentions: Intentions*): (Person, Person, Car) = 
    processScenes(joe, sam, car, intentions.toList)


  def processScenes(joe: Person, sam: Person, car: Car, intentions: List[Intentions]): (Person, Person, Car) = {
    intentions.foldLeft((joe, sam, car)){ case ((curJoe, curSam, curCar), curIntentions) => updateScene(curJoe, curSam, curCar, curIntentions)}
  }


  def scenes = {
    processScenes(
      SAM, JOE, CAR,
      Intentions(School, Home)
    )
    Person.drive(JOE, CAR, School)

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

    if (CAR.fuel > 80) {
      Person.drive(SAM, CAR, Restaurant)
      Person.drive(SAM, CAR, School)
      Person.drive(SAM, CAR, Home)
      Person.drive(SAM, CAR, School)
      Person.drive(SAM, CAR, Home)
      Person.drive(JOE, CAR, School) // Error: Out of gas!
    }
  }

}

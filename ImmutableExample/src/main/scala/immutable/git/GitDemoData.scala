package immutable.git

import immutable.{BranchCommit, LeafCommit}

/**
  * Created by bfrasure on 11/16/16.
  */
object GitDemoData {

  val initialCommit = LeafCommit(
    List(
      (0, "package immutable"),
      (1, "")
    ),
    List()
  )

  val addMutableDataClasses = BranchCommit(
    List(
      (2, "case class Car(var fuel: Int, var location: Location)"),
      (3, "case class Person(var name: String, var location: Location)"),
      (4, "")
    ),
    List(),
    initialCommit
  )

  val addImmutableDataClasses = BranchCommit(
    List(
      (2, "case class Car(fuel: Int, location: Location)"),
      (3, "case class Person(name: String, location: Location)")
    ),
    List(2, 3),
    addMutableDataClasses
  )

  val addLocations = BranchCommit(
    List(
      (5, "sealed trait Location"),
      (6, "case object School extends Location"),
      (7, "case object Home extends Location"),
      (8, "case object Restaurant extends Location"),
      (9, "")
    ),
    List(),
    addImmutableDataClasses
  )

  val addImports = BranchCommit(
    List(
      (1, ""),
      (2, "import scala.util.{Failure, Success, Try}")
    ),
    List(),
    addLocations
  )

  val addCarStubs = BranchCommit(
    List(
      (12, "object TravelFunctions {"),
      (13, "  val tripCost = 20"),
      (14, ""),
      (15, "  def drive(car: Car, destination: Location): Try[Car] = ???"),
      (16, "  def fill(person: Person, car: Car): Try[Car] = ???"),
      (17, "}")
    ),
    List(),
    addImports
  )

  val addDriveImplementation = BranchCommit(
    List(
      (15, "  def drive(car: Car, destination: Location): Try[Car] ="),
      (16, "    car match {"),
      (17, "      case Car(_, location) if (location == destination) => Success(car)"),
      (18, "      case Car(fuel, _) if (fuel >= tripCost) => Success(Car(car.fuel - tripCost, destination))"),
      (19, "      case _ => Failure(new Exception(\"Ran out of gas!\"))"),
      (20, "    }"),
      (21, "")
    ),
    List(
      15
    ),
    addCarStubs
  )

  val addFillImplementation = BranchCommit(
    List(
      (22, "  def fill(person: Person, car: Car): Try[Car] ="),
      (23, "    person.location match {"),
      (24, "      case car.location => Success(car.copy(fuel = 100))"),
      (25, "      case _ => Failure(new Exception(\"Car and driver aren't in the same place!\"))"),
      (26, "    }"),
      (27, "")
    ),
    List(
      22
    ),
    addCarStubs
  )

  val allCommitsInOrder = List(
    initialCommit,
    addMutableDataClasses,
    addImmutableDataClasses,
    addLocations,
    addImports,
    addCarStubs,
    addDriveImplementation,
    addFillImplementation
  )
}

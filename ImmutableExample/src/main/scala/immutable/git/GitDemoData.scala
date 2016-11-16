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
      (3, "case class Person(name: String, location: Location)"),
      (4, "")
    ),
    List(2, 3, 4),
    addMutableDataClasses
  )

  val addLocations = BranchCommit(
    List(
      (2, "sealed trait Location"),
      (3, "case object School extends Location"),
      (4, "case object Home extends Location"),
      (5, "case object Restaurant extends Location"),
      (6, "")
    ),
    List(),
    addImmutableDataClasses
  )

  val addCarStubs = BranchCommit(
    List(
      (1, ""),
      (2, "import scala.util.{Failure, Success, Try}"),
      (12, "object TravelFunctions {"),
      (13, "  val tripCost = 20"),
      (14, ""),
      (15, "  def drive(car: Car, destination: Location): Try[Car] = ???"),
      (21, "  }"),
      (22, "}")
    ),
    List(),
    addImmutableDataClasses
  )

  val addCarFunctions = BranchCommit(
    List(
      (1, ""),
      (2, "import scala.util.{Failure, Success, Try}"),
      (12, "object CarFunctions {"),
      (13, "  val tripCost = 20"),
      (14, ""),
      (15, "  def drive(car: Car, destination: Location): Try[Car] = {"),
      (16, "    car match {"),
      (17, "      case Car(_, location) if (location == destination) => Success(car)"),
      (18, "      case Car(fuel, _) if (fuel >= tripCost) => Success(Car(car.fuel - tripCost, destination))"),
      (19, "      case _ => Failure(new Exception(\"Ran out of gas!\"))"),
      (20, "    }"),
      (21, "  }"),
      (22, "}")
    ),
    List(),
    addImmutableDataClasses
  )

  val allCommitsInOrder = List(
    initialCommit,
    addMutableDataClasses,
    addImmutableDataClasses,
    addLocations,
    addCarFunctions
  )
}

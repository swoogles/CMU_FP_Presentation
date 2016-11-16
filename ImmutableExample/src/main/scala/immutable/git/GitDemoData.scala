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
      (2, "case class Motorcycle(var fuel: Int, var location: Location)"),
      (3, "case class Person(var name: String, var location: Location)"),
      (4, "")
    ),
    List(),
    initialCommit
  )

  val addImmutableDataClasses = BranchCommit(
    List(
      (2, "case class Motorcycle(fuel: Int, location: Location)"),
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

  val addMotorcycleFunctions = BranchCommit(
    List(
      (1, ""),
      (2, "import scala.util.{Failure, Success, Try}"),
      (12, "object MotorcycleFunctions {"),
      (13, "  val tripCost = 20"),
      (14, ""),
      (15, "  def drive(motorcycle: Motorcycle, destination: Location): Try[Motorcycle] = {"),
      (16, "    motorcycle match {"),
      (17, "      case Motorcycle(_, location) if (location == destination) => Success(motorcycle)"),
      (18, "      case Motorcycle(fuel, _) if (fuel >= tripCost) => Success(Motorcycle(motorcycle.fuel - tripCost, destination))"),
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
    addMotorcycleFunctions
  )
}

package immutable

import org.scalatest.FlatSpec

class SimpleGitTests extends FlatSpec {
  val initialCommit = Commit(
    List(
      (0, "package immutable"),
      (1, "")
    ),
    List(),
    None
  )

  val addMutableDataClasses = Commit(
    List(
      (2, "case class Car(var fuel: Int, var location: Location)"),
      (3, "case class Person(var name: String, var location: Location)"),
      (4, "")
    ),
    List(),
    Some(initialCommit)
  )

  val addImmutableDataClasses = Commit(
    List(
      (2, "case class Car(fuel: Int, location: Location)"),
      (3, "case class Person(name: String, location: Location)"),
      (4, "")
    ),
    List(2, 3, 4),
    Some(addMutableDataClasses)
  )

  val addLocations = Commit(
    List(
      (2, "sealed trait Location"),
      (3, "case object School extends Location"),
      (4, "case object Home extends Location"),
      (5, "case object Restaurant extends Location"),
      (6, "")
    ),
    List(),
    Some(addImmutableDataClasses)
  )

  "basic git shit 3 step" should "work" in {
    println("combined content 4 step: ")
    addMutableDataClasses.content foreach println
  }

  "basic git shit 4 step" should "work" in {
    println("combined content 4 step: ")
    addImmutableDataClasses.content foreach println
  }
  "basic git shit 5 step" should "work" in {
    println("combined content 5 step: ")
    addLocations.content foreach println
  }
}

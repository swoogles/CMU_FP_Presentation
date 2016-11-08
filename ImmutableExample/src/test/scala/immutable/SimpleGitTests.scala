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

  val addDataClasses = Commit(
    List(
      (2, "case class Car(fuel: Int, location: Location)"),
      (3, "case class Person(name: String, location: Location)"),
      (4, "")
    ),
    List(),
    Some(initialCommit)
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
    Some(addDataClasses)
  )

  "basic git shit 4 step" should "work" in {
    println("combined content 4 step: ")
    addDataClasses.content foreach println
  }
  "basic git shit 5 step" should "work" in {
    println("combined content 5 step: ")
    addLocations.content foreach println
  }
}

package functionalpresentation

sealed trait Occuptation
case object Teacher extends Occuptation
case object Farmer extends Occuptation
case object Policeman extends Occuptation
case object Chemist extends Occuptation
case object Politician extends Occuptation
case object Soldier extends Occuptation
case object Postman extends Occuptation
case object Biologist extends Occuptation
case object Unemployed extends Occuptation

case class Person(name: String, age: Int, money: Double, job: Occuptation)

object SimpleTransformations {
  val population = List(
    Person("sam", 30, 4000.00, Teacher),
    Person("sally", 28, 6000.00, Teacher),
    Person("joe", 32, -2000.00, Farmer),
    Person("billy", 12, 0, Unemployed),
    Person("sue", 3, 0, Unemployed),
    Person("megan", 25, 2000.00, Biologist),
    Person("betty", 60, 8000.00, Postman),
    Person("fred", 45, -20000.00, Politician),
    Person("kevin", 19, -20000.00, Soldier),
    Person("megan", 25, 2000.00, Biologist),
    Person("jackie", 30, -5000.00, Chemist)
  )

  val debtors = population.filter(_.money < 0)
  val adults = population.filter(_.age >= 18)
  val nationalDebt = population.foldLeft(0.0) { (curDebt, person) => curDebt + person.money}
  val starvingScientists = population.filter(p => (p.job == Biologist || p.job == Chemist) && p.money < 0)
  val civilians = population.filter(p => (p.job != Politician || p.job != Soldier))

}


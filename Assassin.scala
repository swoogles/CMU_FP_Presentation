case class Person(name: String, alive: Boolean)

object Assassin_silent {
  def kill(target: Person) = ()
}

object Assassin_all_talk {
  def kill(target: Person) = "Sure, I'll kill him. Trust me."
}

object Assassin_numbers {
  def kill(target: Person) = 3
}


object Assassin_thorough {
  def kill(target: Person) = target.copy(alive=false)
}


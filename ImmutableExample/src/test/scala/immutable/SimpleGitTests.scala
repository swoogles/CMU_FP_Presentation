package immutable

import org.scalatest.FlatSpec

class SimpleGitTests extends FlatSpec {
  val initialCommit = Commit(
    "00001",
    List(
      (0, "package immutable"),
      (1, "class GitClient {"),
      (2, "}")
    ),
    List(),
    None
  )

  val urlAndLanguageCommit = Commit(
    "00002",
    List(
      //      (2, """val url = "https://github.com/swoogles/CMU_FP_Presentation" """),
      //      (3, """val language = "scala" """)
      (2, "val url = \"https://github.com/swoogles/CMU_FP_Presentation\""),
      (3, "val language = \"java\"")
    ),
    List(),
    Some(initialCommit)
  )

  val languageChangeCommit = Commit(
    "00003",
    List(
      (3, "val language = \"scala\"")
    ),
    List(
      3
    ),
    Some(urlAndLanguageCommit)
  )

  val addLoopCommit = Commit(
    "00003",
    List(
      (4, "for (i <- Range(0,5) {"),
      (5, "   i * 2"),
      (6, "}")
    ),
    List(),
    Some(languageChangeCommit)
  )

  "basic git shit 1 step" should "work" in {
    println("combined content 1 step: ")
    urlAndLanguageCommit.content foreach println
  }
  "basic git shit 2 step" should "work" in {
    println("combined content 2 step: ")
    languageChangeCommit.content foreach println
  }
  "basic git shit 3 step" should "work" in {
    println("combined content 3 step: ")
    addLoopCommit.content foreach println
  }
}

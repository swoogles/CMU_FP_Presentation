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
      (1, "val url = \"https://github.com/swoogles/CMU_FP_Presentation\""),
      (2, "val language = \"java\"")
    ),
    List(),
    None
  )

  val languageChangeCommit = Commit(
    "00003",
    List(
      (3, "val language = \"scala\"")
    ),
    List(
      3
    ),
    None
  )

  "basic git shit" should "work" in {
    val linkedCommit = urlAndLanguageCommit.copy(previousCommit = Some(initialCommit))
    val chainedCommit = languageChangeCommit.copy(previousCommit = Some(linkedCommit))
    println("combined content: ")
    chainedCommit.content foreach println

  }
}

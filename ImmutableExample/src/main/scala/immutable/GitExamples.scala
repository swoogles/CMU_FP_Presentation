package immutable

// TODO figure out how to mesh linesAdded and linesRemoved with their changing indices
case class Commit(hash: String, linesAdded: List[(Int, String)], linesRemoved: List[Int], previousCommit: Option[Commit]) {
  def content: List[String] = {
    if (previousCommit == None)
      linesAdded.map{ case (lineNum, line) => line}
    else {
      val contentSoFar = previousCommit.get.content
      val contentWithLinesRemoved: List[String] = linesRemoved.foldLeft(contentSoFar) {
        (innerContentSoFar, lineToRemove) => innerContentSoFar.drop(lineToRemove)
      }
      val contentWithLinesAdded: List[String] = linesAdded.foldLeft(contentWithLinesRemoved) { (innerContentSoFar, lineToAdd) => {
        val (newLineIdx, newLineContent) = lineToAdd
        val (contentBeginning, contentEnding) = innerContentSoFar.splitAt(newLineIdx)
        contentBeginning ::: newLineContent :: contentEnding
      }}
      contentWithLinesAdded
    }

  }
}
case class Branch(hash: String, history: List[Commit]) {
  assert(!history.isEmpty)
}

object GitConstruction {
  def applyCommit(currentCommit: Commit, changes: Commit) = {
  }

  def applyCommit(branch: Branch, changes: Commit) = {
  }

  def generateContent(branch: Branch) = {
    val (initialCommit :: restOfHistory) = branch.history

  }

}

class GitExamples {
  val initialCommit = Commit(
    "00001",
    List(
      (1, "package immutable"),
      (2, "class GitClient {"),
      (3, "}")
    ),
    List(),
    None
  )

  val urlAndLanguageCommit = Commit(
    "00002",
    List(
//      (3, """val url = "https://github.com/swoogles/CMU_FP_Presentation" """),
//      (4, """val language = "scala" """)
      (3, "val url = \"https://github.com/swoogles/CMU_FP_Presentation\""),
      (4, "val language = \"java\"")
    ),
    List(),
    None
  )

  val languageChangeCommit = Commit(
    "00003",
    List(
      (4, "val language = \"scala\"")
    ),
    List(
      4
    ),
    None
  )
}


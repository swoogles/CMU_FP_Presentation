package immutable

// TODO figure out how to mesh linesAdded and linesRemoved with their changing indices
case class Commit(hash: String, linesAdded: List[(Int, String)], linesRemoved: List[Int], previousCommit: Option[Commit]) {
  def content: List[String] = {
    if (previousCommit == None)
      linesAdded.map{ case (lineNum, line) => line}
    else {
      val contentSoFar = previousCommit.get.content
      val contentWithLinesRemoved: List[String] = linesRemoved.reverse.foldLeft(contentSoFar) {
        (innerContentSoFar, lineToRemove) => innerContentSoFar.take(lineToRemove) ++ innerContentSoFar.drop(lineToRemove+1)

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

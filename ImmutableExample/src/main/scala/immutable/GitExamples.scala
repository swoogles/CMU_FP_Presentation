package immutable

// TODO figure out how to mesh linesAdded and linesRemoved with their changing indices
// TODO Look into 2 classes here, rather than 1 with an optional header
case class Commit(linesAdded: List[(Int, String)], linesRemoved: List[Int], previousCommit: Option[Commit]) {
  def hash: Int = previousCommit match {
    case Some(prevCommit) => prevCommit.hash + 1
    case None => 0
  }

  def content: List[String] = {
    previousCommit match {
      case None => linesAdded.map { case (lineNum, line) => line }
      case Some(prevCommit) => {
        val contentSoFar = prevCommit.content
        val contentWithLinesRemoved: List[String] = linesRemoved.reverse.foldLeft(contentSoFar) {
          (innerContentSoFar, lineToRemove) => innerContentSoFar.take(lineToRemove) ++ innerContentSoFar.drop(lineToRemove + 1)

        }
        val contentWithLinesAdded: List[String] = linesAdded.foldLeft(contentWithLinesRemoved) { (innerContentSoFar, lineToAdd) => {
          val (newLineIdx, newLineContent) = lineToAdd
          val (contentBeginning, contentEnding) = innerContentSoFar.splitAt(newLineIdx)
          contentBeginning ::: newLineContent :: contentEnding
        }
        }
        contentWithLinesAdded
      }
    }

  }
}
object Commit {
  def updateContent(contentSoFar: List[String], nextCommit: Commit) = {
    val contentWithLinesRemoved: List[String] = nextCommit.linesRemoved.reverse.foldLeft(contentSoFar) {
      (innerContentSoFar, lineToRemove) => innerContentSoFar.take(lineToRemove) ++ innerContentSoFar.drop(lineToRemove + 1)

    }
    val contentWithLinesAdded: List[String] = nextCommit.linesAdded.foldLeft(contentWithLinesRemoved) { (innerContentSoFar, lineToAdd) => {
      val (newLineIdx, newLineContent) = lineToAdd
      val (contentBeginning, contentEnding) = innerContentSoFar.splitAt(newLineIdx)
      contentBeginning ::: newLineContent :: contentEnding
    }
    }
    contentWithLinesAdded
  }


  def processCommits(history: List[Commit]): List[List[String]] = {
    history.scanLeft(List("")) { (content, nextCommit) =>
      updateContent(content, nextCommit)
    }
    history.scanLeft(List("")) { updateContent }
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

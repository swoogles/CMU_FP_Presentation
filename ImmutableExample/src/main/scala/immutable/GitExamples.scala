package immutable

sealed trait GitObject {
  val linesAdded: List[(Int, String)]
  val linesRemoved: List[Int]
}

case class LeafCommit(linesAdded: List[(Int, String)], linesRemoved: List[Int]) extends GitObject
case class BranchCommit(linesAdded: List[(Int, String)], linesRemoved: List[Int], prevCommit: GitObject) extends GitObject

trait GitBehavior {
  def updateContent(contentSoFar: List[String], nextCommit: GitObject): List[String]
  def completeHistory(history: List[GitObject]): List[List[String]]
  def content(obj: GitObject): List[String]
}

object GitOperations extends GitBehavior {
  def updateContent(contentSoFar: List[String], nextCommit: GitObject): List[String] = {

    val contentWithLinesRemoved: List[String] = nextCommit.linesRemoved.reverse.foldLeft(contentSoFar) {
      (innerContentSoFar, lineToRemove) => innerContentSoFar.take(lineToRemove) ++ innerContentSoFar.drop(lineToRemove + 1)
    }

    val contentWithLinesAdded: List[String] = nextCommit.linesAdded.foldLeft(contentWithLinesRemoved) { (innerContentSoFar, lineToAdd) =>
      val (newLineIdx, newLineContent) = lineToAdd
      val (contentBeginning, contentEnding) = innerContentSoFar.splitAt(newLineIdx)
      contentBeginning ::: newLineContent :: contentEnding
    }
    contentWithLinesAdded
  }


  def completeHistory(history: List[GitObject]): List[List[String]] = {
    history.scanLeft(List("")) { (content, nextCommit) =>
      updateContent(content, nextCommit)
    }
    history.scanLeft(List("")) { updateContent }
  }

  def content(obj: GitObject): List[String] = {
    obj match {
      case initialCommit: LeafCommit => initialCommit.linesAdded.map { case (lineNum, line) => line }
      case branch: BranchCommit =>
        val contentSoFar = content(branch.prevCommit)
        updateContent(contentSoFar, branch)
    }
  }

}


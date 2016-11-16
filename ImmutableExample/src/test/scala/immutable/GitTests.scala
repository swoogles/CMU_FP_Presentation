package immutable

import org.scalatest.FlatSpec

class GitTests extends FlatSpec {

  "commits " should "assemble into a proper final codebase" in {
    for ((codeSnapShot, idx) <-
           GitOperations.completeHistory(immutable.git.GitDemoData.allCommitsInOrder).zipWithIndex) {
      println("====================================================================================================")
      println(s"Snapshot $idx: ")
      codeSnapShot foreach println
    }
  }

}

package imagemanipulation

import immutable.GitOperations
import org.scalatest.FlatSpec

class ScrimageFunTests extends FlatSpec {

  "scrimage" should "create images for every step" in {
    val fullHistory = GitOperations.completeHistory(immutable.git.GitDemoData.allCommitsInOrder)
    ScrimageFun.makeImgsFromHistory(fullHistory)
  }

}

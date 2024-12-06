import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class GuardGallivantTest extends AnyFlatSpec:
  "The total number of position visited by the guard in the example1 " should " be 41 " in:
    assert(GuardGallivant.getTotalVisitedPositionByGuardIn(Source.fromResource("guardGallivantExample1.txt").getLines().toList) == 41)
  "The total number of position visited by the guard in the exercise " should " be 5318 " in :
    assert(GuardGallivant.getTotalVisitedPositionByGuardIn(Source.fromResource("GuardGallivantExercise.txt").getLines().toList) == 5318)
  "The total number of obstructions in example1 " should " be 6 " in :
    assert(GuardGallivant.getTotalObstructionsIn(Source.fromResource("guardGallivantExample1.txt").getLines().toList) == 6)
  "The total number of obstructions in exercise " should " be 1831 " in :
    assert(GuardGallivant.getTotalObstructionsIn(Source.fromResource("GuardGallivantExercise.txt").getLines().toList) == 1831)

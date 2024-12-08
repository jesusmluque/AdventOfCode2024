import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ResonantCollinearityTest extends AnyFlatSpec:

  "The total numbers of antinode in the example1 file " should " be 14 " in:
    assert(ResonantCollinearity.countAntinodesWithoutReasoningIn(Source.fromResource("ResonantCollinearityExample1.txt").getLines().toList) == 14)
  "The total numbers of antinode in the exercise file " should " be 259 " in :
    assert(ResonantCollinearity.countAntinodesWithoutReasoningIn(Source.fromResource("ResonantCollinearityExercise.txt").getLines().toList) == 259)
  "The total numbers of antinode taking into account reasoning in the example1 file " should " be 34 " in :
    assert(ResonantCollinearity.countAntinodesWithReasoningIn(Source.fromResource("ResonantCollinearityExample1.txt").getLines().toList) == 34)
  "The total numbers of antinode taking into account reasoning in the exercise file " should " be 927 " in :
    assert(ResonantCollinearity.countAntinodesWithReasoningIn(Source.fromResource("ResonantCollinearityExercise.txt").getLines().toList) == 927)


import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ClawContraptionTest extends AnyFlatSpec:

  "The fewest tokens required to gain the maximun prices for the machines in the example 1 " should " be 480 " in:
    assert(ClawContraption.getRequiredTokensToWin(Source.fromResource("ClawContraptionExample.txt").getLines().toList, 0L) == 480L)
  "The fewest tokens required to gain the maximun prices for the machines in the exercise " should " be 480 " in :
    assert(ClawContraption.getRequiredTokensToWin(Source.fromResource("ClawContraptionExercise.txt").getLines().toList, 0L) == 32067L)
  "The fewest tokens required to gain the maximun prices for the machines in the example 1 when prices locations has an extra of 10000000000000L " should " be 875318608908 " in :
    assert(ClawContraption.getRequiredTokensToWin(Source.fromResource("ClawContraptionExample.txt").getLines().toList, 10000000000000L) == 875318608908L)
  "The fewest tokens required to gain the maximun prices for the machines in the exercise when prices locations has an extra of 10000000000000L " should " be 92871736253789 " in :
    assert(ClawContraption.getRequiredTokensToWin(Source.fromResource("ClawContraptionExercise.txt").getLines().toList, 10000000000000L) == 92871736253789L)

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class RestroomRedoubtTest extends AnyFlatSpec:

  "The safety factor (product of four quadrant robots count) for the restroom in Example 1 " should " be 12 " in:
    assert(RestroomRedoubt.getSafetyFactorFor(Source.fromResource("RestroomRedoubtExample.txt").getLines().toList, (7, 11)) == 12L)
  "The safety factor (product of four quadrant robots count) for the restroom in Exercise " should " be 223020000 " in :
    assert(RestroomRedoubt.getSafetyFactorFor(Source.fromResource("RestroomRedoubtExercise.txt").getLines().toList, (103, 101)) == 223020000L)
  "The tree christmas is in the " should " 7338 second " in:
    assert(RestroomRedoubt.findChristmasTree(Source.fromResource("RestroomRedoubtExercise.txt").getLines().toList, (103, 101)) == 7338L)
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source


class RedNosedReportsTest extends AnyFlatSpec:
  "The total reports that are safe in the example " should " be 2 " in:
    assert(RedNosedReports.countSafeReportsIn(Source.fromResource("redNosedReportsExample.txt").getLines().toList) == 2)

  "The total reports that are safe in the exercise " should " be 680 " in:
    assert(RedNosedReports.countSafeReportsIn(Source.fromResource("redNosedReportsExercise.txt").getLines().toList) == 680)

  "The total reports that are safe taking into account the Problem Dampener in the example " should " be 4 " in:
    assert(RedNosedReports.countSafeReportsWithProblemDamperIn(Source.fromResource("redNosedReportsExample.txt").getLines().toList) == 4)

  "The total reports that are safe taking into account the Problem Dampener in the exercise " should " be 710 " in:
    assert(RedNosedReports.countSafeReportsWithProblemDamperIn(Source.fromResource("redNosedReportsExercise.txt").getLines().toList) == 710)

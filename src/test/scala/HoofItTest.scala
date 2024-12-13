import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HoofItTest extends AnyFlatSpec:

  "The total score for all the trailheads found into the topographic map described into the Example 1 file " should " be 36 " in:
    assert(HoofIt.totalTrailheadsScoreIn(Source.fromResource("HoofItExample1.txt").getLines().toList) == 36L)
  "The total score for all the trailheads found into the topographic map described into the Exercise file " should " be 796 " in :
    assert(HoofIt.totalTrailheadsScoreIn(Source.fromResource("HoofItExercise.txt").getLines().toList) == 796L)
  "The total score for all the trailheads found into the topographic map described into the Example 1 file using the second type of score " should " be 81 " in :
    assert(HoofIt.totalTrailheadsScoreTwoIn(Source.fromResource("HoofItExample1.txt").getLines().toList) == 81L)
  "The total score for all the trailheads found into the topographic map described into the Exercise file using the second type of score " should " be 1942 " in :
    assert(HoofIt.totalTrailheadsScoreTwoIn(Source.fromResource("HoofItExercise.txt").getLines().toList) == 1942L)
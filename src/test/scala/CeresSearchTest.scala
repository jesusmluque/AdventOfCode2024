import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CeresSearchTest extends AnyFlatSpec:
  "The count of XMAS word into the example1 file " should " be 18 " in:
    assert(CeresSearch.countWordInto(Source.fromResource("CeresSearchExample1.txt").getLines().toList, "XMAS") == 18)
  "The count of XMAS word into the exercise file " should " be 2406 " in :
    assert(CeresSearch.countWordInto(Source.fromResource("CeresSearchExercise.txt").getLines().toList, "XMAS") == 2406)
  "The count of MAS word in X shape into the example1 file " should " be 9 " in :
    assert(CeresSearch.countXShapeWordInto(Source.fromResource("CeresSearchExample1.txt").getLines().toList, "MAS") == 9)
  "The count of MAS word in X shape into the exercise file " should " be 9 " in :
    assert(CeresSearch.countXShapeWordInto(Source.fromResource("CeresSearchExercise.txt").getLines().toList, "MAS") == 1807)
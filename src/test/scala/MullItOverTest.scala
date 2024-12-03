import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class MullItOverTest extends AnyFlatSpec:
  "The sum of all valid multiplications " should " be 161 in the example file " in:
    assert(MullItOver.totalSumAfterParse(Source.fromResource("mullItOverExample.txt").getLines().toList) == 161L)
  "The sum of all valid multiplications " should " be 322 in the example2 file " in :
    assert(MullItOver.totalSumAfterParse(Source.fromResource("mullItOverExample2.txt").getLines().toList) == 322L)
  "The sum of all valid multiplications " should " be 163931492 in the exercise file " in:
    assert(MullItOver.totalSumAfterParse(Source.fromResource("mullItOverExercise.txt").getLines().toList) == 163931492L)
  "The sum of all valid multiplications taking into account the dont and do instructions " should " be 48 in the example file " in :
    assert(MullItOver.totalSumAfterParseWithDontAndDoIns(Source.fromResource("mullItOverExample3.txt").getLines().toList) == 48L)
  "The sum of all valid multiplications taking into account the dont and do instructions " should " be 76911921 in the exercise file " in :
    assert(MullItOver.totalSumAfterParseWithDontAndDoIns(Source.fromResource("mullItOverExercise.txt").getLines().toList) == 76911921L)


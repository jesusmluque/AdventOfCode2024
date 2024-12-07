import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class BridgeRepairTest extends AnyFlatSpec:
  "The sum of all the valid equation result in the example1 file " should " be 3749 " in:
    assert(BridgeRepair.getSumAllValidEquationIn(Source.fromResource("bridgeRepairExample1.txt").getLines().toList, List("+", "*")) == 3749L)
  "The sum of all the valid equation result in the exercise file " should " be 3598800864292 " in :
    assert(BridgeRepair.getSumAllValidEquationIn(Source.fromResource("bridgeRepairExercise.txt").getLines().toList, List("+", "*")) == 3598800864292L)
  "The sum of all the valid equation result taking into account a third operation in the example1 file " should " be 11387 " in :
    assert(BridgeRepair.getSumAllValidEquationIn(Source.fromResource("bridgeRepairExample1.txt").getLines().toList, List("+", "*", "||")) == 11387L)
  "The sum of all the valid equation result taking into account a third operation in the exercise file " should " be 340362529351427 " in :
    assert(BridgeRepair.getSumAllValidEquationIn(Source.fromResource("bridgeRepairExercise.txt").getLines().toList, List("+", "*", "||")) == 340362529351427L)

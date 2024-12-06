import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class PrintQueueTest extends AnyFlatSpec:
  
  "The sum of all middle page numbers from the correct ordering pages into example1 file " should " be 143 " in:
    assert(PrintQueue.getTotalMiddleAgePageNumbersFrom(Source.fromResource("PrintQueueExample1.txt").getLines().toList) == 143L)
  "The sum of all middle page numbers from the correct ordering pages into exercise file " should " be 143 " in :
    assert(PrintQueue.getTotalMiddleAgePageNumbersFrom(Source.fromResource("PrintQueueExercise.txt").getLines().toList) == 6242L)
  "The sum of all middle page numbers from the incorrect ordering pages once ordered into example1 file " should " be 123 " in :
    assert(PrintQueue.getTotalMiddleAgePageNumbersOnceCorrectedFrom(Source.fromResource("PrintQueueExample1.txt").getLines().toList) == 123L)
  "The sum of all middle page numbers from the incorrect ordering pages once ordered into exercise file " should " be 5169 " in :
    assert(PrintQueue.getTotalMiddleAgePageNumbersOnceCorrectedFrom(Source.fromResource("PrintQueueExercise.txt").getLines().toList) == 5169L)
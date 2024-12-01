import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HistorianHysteriaTest extends AnyFlatSpec {

  "The sum of all distance on the example file " should " be 11 " in {
    assert(HistorianHysteria.getTotalDistance(Source.fromResource("HistorianHysteria.txt").getLines().toList) == 11L)
  }

  "The sum of all distance on the user data file " should " be 11 " in {
    assert(HistorianHysteria.getTotalDistance(Source.fromResource("HistorianHysteria2.txt").getLines().toList) == 2086478L)
  }

  "The sum of all products on the example file " should " be 31 " in {
    assert(HistorianHysteria.getTotalProducts(Source.fromResource("HistorianHysteria.txt").getLines().toList) == 31L)
  }

  "The sum of all products on the user data file " should " be 11 " in {
    assert(HistorianHysteria.getTotalProducts(Source.fromResource("HistorianHysteria2.txt").getLines().toList) == 24941624L)
  }
}
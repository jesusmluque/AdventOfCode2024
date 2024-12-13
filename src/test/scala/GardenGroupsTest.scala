import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class GardenGroupsTest extends AnyFlatSpec:
  "The total price (perimeter x area) of fencing all regions on the map described in the Example 1 " should " be 140 " in:
    assert(GardenGroups.totalPrice(Source.fromResource("GardenGroupsExample1.txt").getLines().toList) == 140L)
  "The total price (perimeter x area) of fencing all regions on the map described in the Example 2 " should " be 1930 " in :
    assert(GardenGroups.totalPrice(Source.fromResource("GardenGroupsExample2.txt").getLines().toList) == 1930L)
  "The total price (perimeter x area) of fencing all regions on the map described in the Exercise " should " be 1387004 " in :
    assert(GardenGroups.totalPrice(Source.fromResource("GardenGroupsExercise.txt").getLines().toList) == 1387004L)
  "The total price (perimeter x area) of fencing all regions on the map described in the Example 1 when perimeter counts only side of fences " should " be 140 " in :
    assert(GardenGroups.totalPriceWithNumberOfSide(Source.fromResource("GardenGroupsExample1.txt").getLines().toList) == 80L)
  "The total price (perimeter x area) of fencing all regions on the map described in the Example 4 when perimeter counts only side of fences " should " be 236 " in :
    assert(GardenGroups.totalPriceWithNumberOfSide(Source.fromResource("GardenGroupsExample4.txt").getLines().toList) == 236L)
  "The total price (perimeter x area) of fencing all regions on the map described in the Exercise when perimeter counts only side of fences " should " be 844198 " in :
    assert(GardenGroups.totalPriceWithNumberOfSide(Source.fromResource("GardenGroupsExercise.txt").getLines().toList) == 844198L)
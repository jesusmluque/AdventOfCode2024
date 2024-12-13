import org.scalatest.flatspec.AnyFlatSpec

class PlutonianPebblesTest extends AnyFlatSpec:

  "The number of stones after blinking 25 times with the initial configuration of 125 17 " should " be 55312 " in:
    assert(PlutonianPebbles.getTotalStonesFor("125 17", 25) == 55312L)
  "The number of stones after blinking 25 times with the initial configuration of 0 27 5409930 828979 4471 3 68524 170 " should " be 194482 " in :
    assert(PlutonianPebbles.getTotalStonesFor("0 27 5409930 828979 4471 3 68524 170", 25) == 194482L)
  "The number of stones after blinking 75 times with the initial configuration of 0 27 5409930 828979 4471 3 68524 170 " should " be 232454623677743 " in :
    assert(PlutonianPebbles.getTotalStonesFor("0 27 5409930 828979 4471 3 68524 170", 75) == 232454623677743L)
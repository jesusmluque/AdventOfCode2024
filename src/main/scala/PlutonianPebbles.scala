import scalaz.Memo

object PlutonianPebbles:

  def getTotalStonesFor(initConfig: String, times: Int): Long =
    lazy val newStoneConfigFrom: ((Long, Int)) => Long = Memo.immutableHashMapMemo {
      case (_, 0) => 1L
      case (stone, blinks) if stone.toString == "0" => newStoneConfigFrom((1L, blinks - 1))
      case (stone, blinks) if stone.toString.length % 2 == 0 =>
        val (leftStone, rightStone) = stone.toString.splitAt(stone.toString.length / 2)
        newStoneConfigFrom((leftStone.toLong, blinks - 1)) + newStoneConfigFrom((rightStone.toLong, blinks - 1))
      case (stone, blinks) => newStoneConfigFrom((stone * 2024L, blinks - 1))
    }
    initConfig.split(" ").map(stone => newStoneConfigFrom((stone.toLong, times))).sum
object ClawContraption:

  def getRequiredTokensToWin(rawData: List[String], extraPrize: Long): Long =
    val rawMachineInstructions = rawData.foldLeft(List[List[String]]()): (acc, next) =>
      if next == "" then
        List() :: acc.head.reverse :: acc.tail
      else
        val lastUpdated = next :: acc.headOption.getOrElse(List())
        lastUpdated :: (if acc.isEmpty then List() else acc.tail)

    rawMachineInstructions.foldLeft(0L): (acc, next) =>
      val inst = next.foldLeft(List[(String, (Long, Long))]()): (acc, n) =>
        (n match
          case s"Button A: X+${buttonAX}, Y+${buttonAY}" => ("A", (buttonAX.toLong, buttonAY.toLong))
          case s"Button B: X+${buttonBX}, Y+${buttonBY}" => ("B", (buttonBX.toLong, buttonBY.toLong))
          case s"Prize: X=${prizeX}, Y=${prizeY}" => ("P", (prizeX.toLong + extraPrize, prizeY.toLong + extraPrize)))
        :: acc
      val (x,y) = inst.filter(_._1 == "P").head._2
      val (ax, ay) = inst.filter(_._1 == "A").head._2
      val (bx, by) = inst.filter(_._1 == "B").head._2
      val b = (y*ax - ay*x)/(by*ax - bx*ay)
      val a = (x - bx*b)/ax
      val isValid = (y*ax - ay*x) % (by*ax - bx*ay) == 0 && (x - bx*b) % ax == 0 && a >= 0L && b > 0L
      if isValid then a*3 + b + acc else acc
object BridgeRepair:

  def getSumAllValidEquationIn(rawEquations: List[String], operations: List[String]): Long =
    def solveEquation(target: Long, equation: List[Long]): Option[Long] =
      if equation.size == 1 then
        if equation.head == target then Some(target) else None
      else
        val first = equation.head
        val second = equation.tail.head
        val rest = equation.tail.tail
        operations.iterator.map:
            case "+" => solveEquation(target, (first + second) :: rest)
            case "*" => solveEquation(target, (first * second) :: rest)
            case "||" => solveEquation(target, (first.toString + second.toString).toLong :: rest)
        .collectFirst {case Some(r) => r}

    rawEquations.map: raw =>
      val Array(target, n) = raw.split(":")
      val numbers = n.trim.split("\\s+").map(_.toLong).toList
      solveEquation(target.toLong, numbers)
    .map(_.getOrElse(0L)).sum
object BridgeRepair:

  def getSumAllValidEquationIn(rawEquations: List[String], operations: List[String]): Long =
    def solveEquation(target: Long, equation: List[Long]): Option[Long] =
      if equation.size == 1 then
        if equation.head == target then Some(target) else None
      else
        val first = equation.head
        val second = equation.tail.head
        val rest = equation.tail.tail
        operations.foldLeft(List[Option[Long]]()): (acc, next) =>
          next match
            case "+" => solveEquation(target, (first + second) :: rest) :: acc
            case "*" => solveEquation(target, (first * second) :: rest) :: acc
            case "||" => solveEquation(target, (first.toString + second.toString).toLong :: rest) :: acc
        .find(_.isDefined) match
          case None => None
          case Some(r) => r

    rawEquations.map: raw =>
      val eq = raw.split(":")
      val target = eq(0).toLong
      val numbers = eq(1).split(" ").filter(_ != "").map(_.toLong).toList
      solveEquation(target, numbers)
    .map(_.getOrElse(0L)).sum
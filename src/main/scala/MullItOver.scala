object MullItOver:

  def totalSumAfterParse(rawInstructions: List[String]):Long =
    rawInstructions.foldLeft(List[Long]()): (acc, next) =>
      "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r.findAllMatchIn(next).map(_.subgroups).toList.map: pair =>
        pair.map(_.toLong).product
      ++ acc
    .sum

  def totalSumAfterParseWithDontAndDoIns(rawInstructions: List[String]): Long =
    val allInstructions = rawInstructions.foldLeft(""): (acc, str) =>
      acc ++ str
    val instructionsSeparatedByDonts = allInstructions.split("don't\\(\\)").toList
    val instructionsFiltered = instructionsSeparatedByDonts.tail.foldLeft(List(instructionsSeparatedByDonts.head)): (acc, partialStr) =>
      partialStr.split("do\\(\\)").toList.tail ++ acc
    totalSumAfterParse(instructionsFiltered)
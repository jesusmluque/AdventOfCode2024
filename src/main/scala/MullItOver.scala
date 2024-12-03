object MullItOver:

  def totalSumAfterParse(rawInstructions: List[String]):Long =
    rawInstructions.flatMap: next =>
      "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r.findAllMatchIn(next).map(_.subgroups).toList.map: pair =>
        pair.map(_.toLong).product
    .sum

  def totalSumAfterParseWithDontAndDoIns(rawInstructions: List[String]): Long =
    val allInstructionsString = rawInstructions.foldLeft("")( (acc, str) => acc ++ str)
    val instructionsSeparatedByDonts = allInstructionsString.split("don't\\(\\)").toList
    val instructionsFiltered = instructionsSeparatedByDonts.head :: instructionsSeparatedByDonts.tail.flatMap: partialStr =>
      partialStr.split("do\\(\\)").toList.tail
    totalSumAfterParse(instructionsFiltered)
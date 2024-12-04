object CeresSearch:

  def countWordInto(rawLetters: List[String], wordToFind: String): Int =
    val letters: Vector[Vector[(String, (Int, Int))]] = buildLettersMatrix(rawLetters)
    (getAllHorizontalVerticalWordsStartingBy(letters, wordToFind)
    ++ getAllDiagonalWordsWIthLengthAndStartingBy(wordToFind, letters).map(_._1))
    .count(_ == wordToFind)

  def countXShapeWordInto(rawLetters: List[String], wordToFind: String): Int =
    val letters: Vector[Vector[(String, (Int, Int))]] = buildLettersMatrix(rawLetters)
    getAllDiagonalWordsWIthLengthAndStartingBy(wordToFind, letters)
      .filter(_._1 == wordToFind).groupBy(_._2).count(_._2.size == 2)

  private def getAllHorizontalVerticalWordsStartingBy(letters: Vector[Vector[(String, (Int, Int))]], wordToFind: String) =
    val wordLength = wordToFind.length
    val firstLetter = wordToFind(0).toString
    val limits = (letters.size, letters(0).size)
    val firstLetterPos: List[(String, (Int, Int))] = getFirstLetterPositions(firstLetter, letters)
    firstLetterPos.foldLeft(List[String]()): (acc, pos) =>
      val right = if pos._2._2 + wordLength - 1 < limits._2 then
        Range(pos._2._2, pos._2._2 + wordLength).foldLeft(List[String]()): (acc2, r) =>
          letters(pos._2._1)(r)._1 :: acc2
        .reverse.mkString("")
      else
        ""
      val down = if pos._2._1 + wordLength - 1 < limits._1 then
        Range(pos._2._1, pos._2._1 + wordLength).foldLeft(List[String]()): (acc2, r) =>
          letters(r)(pos._2._2)._1 :: acc2
        .reverse.mkString("")
      else ""
      val left = if pos._2._2 - wordLength + 1 >= 0 then
        Range(pos._2._2, pos._2._2 - wordLength, -1).foldLeft(List[String]()): (acc2, r) =>
          letters(pos._2._1)(r)._1 :: acc2
        .reverse.mkString("")
      else ""
      val up = if pos._2._1 - wordLength + 1 >= 0 then
        Range(pos._2._1, pos._2._1 - wordLength, -1).foldLeft(List[String]()): (acc2, r) =>
          letters(r)(pos._2._2)._1 :: acc2
        .reverse.mkString("")
      else ""
      List(right, down, left, up).filter(_ != "") ++ acc

  private def getAllDiagonalWordsWIthLengthAndStartingBy(wordToFind: String, letters: Vector[Vector[(String, (Int, Int))]]): List[(String, (Int,Int))] =
    val limits = (letters.size, letters(0).size)
    val wordLength = wordToFind.length
    val firstLetter = wordToFind(0).toString
    val firstLetterPos: List[(String, (Int, Int))] = getFirstLetterPositions(firstLetter, letters)
    val indexInTheMiddleOfTheWord = wordLength / 2
    firstLetterPos.foldLeft(List[(String, (Int, Int))]()): (acc, pos) =>
      val rightUp = if pos._2._2 + wordLength - 1 < limits._2 && pos._2._1 - wordLength + 1 >= 0 then
        (Range(pos._2._1, pos._2._1 - wordLength, -1).zip(Range(pos._2._2, pos._2._2 + wordLength)).foldLeft(List[String]()): (acc2, r) =>
          letters(r._1)(r._2)._1 :: acc2
        .reverse.mkString(""), (pos._2._1 - indexInTheMiddleOfTheWord, pos._2._2 + indexInTheMiddleOfTheWord))
      else
        ("", (0, 0))
      val rightDown = if pos._2._2 + wordLength - 1 < limits._2 && pos._2._1 + wordLength - 1 < limits._1 then
        (Range(pos._2._1, pos._2._1 + wordLength).zip(Range(pos._2._2, pos._2._2 + wordLength)).foldLeft(List[String]()): (acc2, r) =>
          letters(r._1)(r._2)._1 :: acc2
        .reverse.mkString(""), (pos._2._1 + indexInTheMiddleOfTheWord, pos._2._2 + indexInTheMiddleOfTheWord))
      else
        ("", (0, 0))
      val leftDown = if pos._2._2 - wordLength + 1 >= 0 && pos._2._1 + wordLength - 1 < limits._1 then
        (Range(pos._2._1, pos._2._1 + wordLength).zip(Range(pos._2._2, pos._2._2 - wordLength, -1)).foldLeft(List[String]()): (acc2, r) =>
          letters(r._1)(r._2)._1 :: acc2
        .reverse.mkString(""), (pos._2._1 + indexInTheMiddleOfTheWord, pos._2._2 - indexInTheMiddleOfTheWord))
      else ("", (0, 0))
      val leftUp = if pos._2._2 - wordLength + 1 >= 0 && pos._2._1 - wordLength + 1 >= 0 then
        (Range(pos._2._1, pos._2._1 - wordLength, -1).zip(Range(pos._2._2, pos._2._2 - wordLength, -1)).foldLeft(List[String]()): (acc2, r) =>
          letters(r._1)(r._2)._1 :: acc2
        .reverse.mkString(""), (pos._2._1 - indexInTheMiddleOfTheWord, pos._2._2 - indexInTheMiddleOfTheWord))
      else ("", (0, 0))
      List(rightUp, rightDown, leftDown, leftUp).filter(_._1 != "") ++ acc

  private def buildLettersMatrix(rawLetters: List[String]) =
    rawLetters.zipWithIndex.toVector.map: rows =>
      rows._1.split("").toVector.zipWithIndex.map: p =>
        (p._1, (rows._2, p._2))

  private def getFirstLetterPositions(firstLetter: String, letters: Vector[Vector[(String, (Int, Int))]]) =
    letters.foldLeft(List[(String, (Int, Int))]()): (acc, next) =>
      next.foldLeft(acc): (acc2, n) =>
        if n._1 == firstLetter then
          n :: acc2
        else
          acc2
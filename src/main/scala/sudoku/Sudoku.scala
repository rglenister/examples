package sudoku

import scala.annotation.tailrec

/**
 * A sudoku puzzle solver
 */
object Sudoku extends App {

//  val puzzleDefinition = """
//                         ------4--
//                         7-2-13--8
//                         --64-2--7
//                         --45-----
//                         8--9-7--6
//                         -----82--
//                         -----47--
//                         5--28-3-4
//                         --3------
//                         """
val puzzleDefinition = """
                       4--958--1
                       -7316254-
                       1--743-2-
                       3--21---5
                       71-39526-
                       2--48-1--
                       ---871-5-
                       -3752491-
                       5-1639--2
                       """

  val board: Array[Array[Int]] =
    List.tabulate(9) {
      (x: Int) => puzzleDefinition.replaceAll("\\s", "").replaceAll("-", "0").substring(x * 9, x * 9 + 9).toArray map {
        _.toString.toInt
      }
    }.toArray


  def print(): Unit = {
    println()
    println(board.map(_.mkString(" ")).mkString("\n"))
  }

  /**
   * Brute force search.
   *
   * The value of the square with the given co-ordinates is tested. If it is non-empty (i.e. > 0) then the search
   * is recursively continued at the next square. If it is empty (i.e. 0) then the search is
   * recursively continued using each legal square value in the range 1..9.
   *
   * @param x the x co-ordinate of the current square
   * @param y the y co-ordinate of the current square
   * @param numSolutionsFound the number of solutions found so far
   * @return the number of solutions found
   */
  def search(x: Int, y: Int, numSolutionsFound: Int): Int = {

    /**
     * If the proposed square value doesn't cause any illegal duplicate square values then the square at the current
     * x and y co-ordinates is set to the proposed square value and the search method called to continue the search.
     * Otherwise the proposed square value is not used.
     *
     * The function recurses until each value in the range 1..9 has been tried
     *
     * @param squareValue is the square value
     * @param numSolutionsFound the number of solutions found so far
     * @return the number of solutions found
     */
    def fillSquareValue(squareValue: Int, numSolutionsFound: Int): Int = {

      /**
       * Checks whether the use of the proposed square value will cause illegal duplicate square values.
       *
       * @param squareIndex is the index into the current row, column and sub-square
       * @return true if there are no repeats else false
       */
      @tailrec
      def isLegalSquareValue(squareIndex: Int): Boolean = {
        squareIndex >= 9 ||
          (board(x)(squareIndex) != squareValue && board(squareIndex)(y) != squareValue &&
            board(x / 3 * 3 + squareIndex % 3)(y / 3 * 3 + squareIndex / 3) != squareValue && isLegalSquareValue(squareIndex + 1))
      }

      if (squareValue > 9) {
        numSolutionsFound
      } else if (isLegalSquareValue(0)) {
        board(x)(y) = squareValue
        val nextNumSolutionsFound = fillSquareValue(squareValue + 1, search(x + 1, y, numSolutionsFound))
        board(x)(y) = 0
        nextNumSolutionsFound
      } else {
        fillSquareValue(squareValue + 1, numSolutionsFound)
      }
    }

    (x, y) match {
      case (9, _) => search(0, y + 1, numSolutionsFound) // move to next row
      case (0, 9) => print(); numSolutionsFound + 1        // all squares filled out so we have a solution
      case _ =>
        if (board(x)(y) != 0) {
          search(x + 1, y, numSolutionsFound)
        } else {
          fillSquareValue(1, numSolutionsFound)
        }
    }
  }

  print()
  printf("\n%s solution(s) found", search(0, 0, 0))
}
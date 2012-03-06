package quote


object Quote {

  val pounds = "\u00a3"
  
  case class Lender(name: String, rate: BigDecimal, available: BigDecimal)

  abstract class Loan {
    def amount: BigDecimal
    def rate: BigDecimal
    lazy val nominalMonthlyRate = Math.pow(1.0 + rate.toDouble, (1.0 / 12.0)) - 1
    lazy val monthlyRepayment = BigDecimal("%.2f".format(
        amount * (nominalMonthlyRate + (nominalMonthlyRate / ((BigDecimal(1) + nominalMonthlyRate).pow(36) - 1)))).toString)
    lazy val totalRepayment = monthlyRepayment * 36
  }

  case class LoanItem(lender: Lender, amount: BigDecimal) extends Loan {
    val rate = lender rate
  }

  case class CompositeLoan(loans: List[Loan]) extends Loan {
    val amount = loans.foldLeft(BigDecimal(0))(_ + _.amount)
    val rate = loans.foldLeft(BigDecimal(0))((sum, loan) => sum + loan.amount / amount * loan.rate)
  }

  def createLenders(marketFile: String) = {
    scala.io.Source.fromFile(marketFile).getLines.toList.flatMap(_.split(",") match {
      case Array(name, rate, available) => Some(Lender(name, BigDecimal(rate), BigDecimal(available)))
      case _ => None
    })
  }

  def allocateLoan(amount: BigDecimal, lenders: List[Lender], loans: List[Loan] = Nil): Option[Loan] = lenders match {
    case h :: t if (amount <= h.available) => Some(CompositeLoan(LoanItem(h, amount) :: loans))
    case h :: t => allocateLoan(amount - h.available, t, LoanItem(h, h.available) :: loans)
    case _ => None
  }

  def printLoan(loan: Loan) {
    println("Requested amount: \u00a3%.2f".format(loan.amount))
    println("Rate: %.1f%%".format(loan.rate * 100))
    println("Monthly repayment: \u00a3%.2f".format(loan.monthlyRepayment))
    println("Total repayment: \u00a3%.2f".format(loan.totalRepayment))
  }

  def getLoanAmount(loanAmountStr: String) = {
    val loanAmount = BigDecimal(loanAmountStr)
    if ((loanAmount < BigDecimal(1000)) || loanAmount > 15000 || loanAmount % 100 != 0)
      throw new IllegalArgumentException("Loan amount should be of any \u00a3100 increment between \u00a31000 and \u00a315000")
    loanAmount
  }

  def getArgs(args: Array[String]) = args match {
    case Array(marketFile, loanAmountStr) => (marketFile, getLoanAmount(loanAmountStr))
    case _ => throw new IllegalArgumentException("quote marketFile loanAmount")
  }

  def main(args: Array[String]): Unit = {
    val (marketFile, loanAmount) = getArgs(args)
    val lenders = createLenders(marketFile).sortWith(_.rate < _.rate)
    allocateLoan(loanAmount, lenders) match {
      case Some(loan) => printLoan(loan)
      case None => println("Sorry we don't have enough to lend you")
    }
  }
}
package atm


import scala.annotation.tailrec


case class ATM(availableCash: Int) {
  def withdraw(amount: Int) = if (availableCash >= amount) Some(ATM(availableCash - amount)) else None
}

case class Account(accountNumber: Int, balance: Int, overdraftFacility: Int) {
  def availableBalance = balance + overdraftFacility
  def withdraw(amount: Int) = {
    if (availableBalance >= amount)
      Some(Account(accountNumber, balance - amount, overdraftFacility))
    else None
  }
}

case class ATMAndAccount(atm: ATM, account: Account)

object ATM {
  
  def main(args: Array[String]): Unit = {
    if (!args.isEmpty) {
      Console.setIn(new java.io.FileInputStream(args(0)))
    }
    processBlock(ATM(0), readLine)
  }
  
  val AtmRefillHeaderRe = """^([0-9]+)$""".r
  val AccountHeaderLine1Re = """^([0-9]{8}) ([0-9]{4}) ([0-9]{4})$""".r
  val AccountHeaderLine2Re = """^([0-9]+) ([0-9]+)$""".r
  val BalanceRe = """^B$""".r
  val WithdrawalRe = """^W ([0-9]+)$""".r
  
  def readLine = {
    val s = Console.readLine
    if (s != null) s else ""
  }
  
  def skipToNextBlock {
    if (!readLine.isEmpty()) skipToNextBlock
  }
  
  @tailrec
  def processBlock(atm: ATM, header: String): Option[ATM] = {
    if (header.isEmpty) None
    else {
      processAtmRefillBlock(atm, header).orElse(processAccountBlock(atm, header)) match {
        case Some(nextAtm) => processBlock(nextAtm, readLine)
        case None => skipToNextBlock; processBlock(atm, readLine)
      }      
    }
  }
  
  def processAtmRefillBlock(atm: ATM, header: String): Option[ATM] = {
    AtmRefillHeaderRe findFirstIn(header) flatMap { s: String => readLine; Some(ATM(atm.availableCash + s.toInt)) } 
  }
  
  def processAccountBlock(atm: ATM, header: String): Option[ATM] = {
    AccountHeaderLine1Re findFirstIn(header) match {
      case Some(_) => {
        val AccountHeaderLine1Re(accountNumber, actualPin, enteredPin) = header
        if (actualPin == enteredPin) {
          processAccountLine2(atm, accountNumber.toInt, readLine)
        } else {
          println("ACCOUNT_ERR")
          skipToNextBlock
          Some(atm)
        }
      }
      case None => None
    }
  }

  def processAccountLine2(atm: ATM, accountNumber: Int, header: String): Option[ATM] = {
    val AccountHeaderLine2Re(balance, overdraftFacility) = header
    processUserCommand(atm, Account(accountNumber, balance.toInt, overdraftFacility.toInt), readLine) match {
      case Some(atmAndAccount) => Some(atm)
      case None => None
    }    
  }

  @tailrec
  def processUserCommand(atm: ATM, account: Account, line: String): Option[ATM] = {
    processBalanceEnquiry(atm, account, line).orElse(processWithdrawal(atm, account, line)) match {
      case Some(atmAndAccount) => processUserCommand(atmAndAccount.atm, atmAndAccount.account, readLine)
      case None => Some(atm)
    }
  }
  
  def processBalanceEnquiry(atm: ATM, account: Account, line: String): Option[ATMAndAccount] = {
    BalanceRe findFirstIn(line) match {
      case Some(_) => println(account.balance); Some(ATMAndAccount(atm, account))
      case None => None
    }
  }
  
  def processWithdrawal(atm: ATM, account: Account, line: String) = {
    
    def processWithdrawal(atmOption: Option[ATM], accountOption: Option[Account]) = (atmOption, accountOption) match {
      case (_, None) => println("FUNDS_ERR"); Some(ATMAndAccount(atm, account))
      case (None, Some(account)) => println("ATM_ERR"); Some(ATMAndAccount(atm, account))
      case (Some(nextAtm), Some(nextAccount)) => println(nextAccount.balance); Some(ATMAndAccount(nextAtm, nextAccount))
    }
    
    WithdrawalRe findFirstIn(line) match {
      case Some(_) => {
        val WithdrawalRe(amnt) = line
        val amount = amnt.toInt
        processWithdrawal(atm.withdraw(amount), account.withdraw(amount))
      }
      case None => None
    }
  }
}  

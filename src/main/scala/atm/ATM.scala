package atm


import scala.annotation.tailrec


case class ATM(availableCash: Int) {
  def withdraw(amount: Int) = Some(ATM(availableCash - amount)) filter { _.availableCash >= 0 }
}

case class Account(accountNumber: Int, balance: Int, overdraftFacility: Int) {
  def withdraw(amount: Int) = Some(Account(accountNumber, balance - amount, overdraftFacility)) filter { -overdraftFacility <= _.balance }
}

case class ATMAndAccount(atm: ATM, account: Account)

object ATM {
  
  def main(args: Array[String]): Unit = {
    if (!args.isEmpty) {
      Console.setIn(new java.io.FileInputStream(args(0)))
    }
    processBlock(ATM(0))
  }
  
  val AtmRefillHeaderRe = """^([0-9]+)$"""r
  val AccountHeaderLine1Re = """^([0-9]{8}) ([0-9]{4}) ([0-9]{4})$"""r
  val AccountHeaderLine2Re = """^([0-9]+) ([0-9]+)$"""r
  val BalanceRe = """^B$"""r
  val WithdrawalRe = """^W ([0-9]+)$"""r
  
  def readLine = {
    val s = Console.readLine
    if (s != null) s else ""
  }
  
  @tailrec
  def skipToNextBlock { if (!readLine.isEmpty()) skipToNextBlock }
  
  @tailrec
  def processBlock(atm: ATM): Option[ATM] = {
    
    def processAtmRefillBlock: PartialFunction[String, ATM] = {
      case header if (AtmRefillHeaderRe.findFirstIn(header) != None) => readLine; ATM(atm.availableCash + header.toInt) 
    }
  
    def processAccountBlock: PartialFunction[String, ATM] = {
      case header if (AccountHeaderLine1Re.findFirstIn(header) != None) => {
        val AccountHeaderLine1Re(accountNumber, actualPin, enteredPin) = header
        if (actualPin == enteredPin) {
          val AccountHeaderLine2Re(balance, overdraftFacility) = readLine
          processUserCommand(atm, Account(accountNumber.toInt, balance.toInt, overdraftFacility.toInt))
        } else {
          println("ACCOUNT_ERR")
          skipToNextBlock
          atm
        }
      }
    }
    
    val header = readLine
    if (!header.isEmpty) processBlock((processAtmRefillBlock orElse processAccountBlock)(header))     
    else None
  }

  @tailrec
  def processUserCommand(atm: ATM, account: Account): ATM = {
    
    def processBalanceEnquiry: PartialFunction[String, ATMAndAccount] = {
      case line if (BalanceRe.findFirstIn(line) != None) => {
        println(account.balance); ATMAndAccount(atm, account)
      }
    }
  
    def processWithdrawal: PartialFunction[String, ATMAndAccount] = {    
      case line if (WithdrawalRe.findFirstIn(line) != None) => {
        val WithdrawalRe(amnt) = line
        val amount = amnt.toInt
        (atm.withdraw(amount), account.withdraw(amount)) match {
          case (_, None) => println("FUNDS_ERR"); ATMAndAccount(atm, account)
		  case (None, Some(account)) => println("ATM_ERR"); ATMAndAccount(atm, account)
		  case (Some(nextAtm), Some(nextAccount)) => println(nextAccount.balance); ATMAndAccount(nextAtm, nextAccount)
        }
      }
    }
    
    val line = readLine
    if (!line.isEmpty) {
      val atmAndAccount = (processBalanceEnquiry orElse processWithdrawal)(line) 
      processUserCommand(atmAndAccount.atm, atmAndAccount.account)
    } else atm
  }
}  

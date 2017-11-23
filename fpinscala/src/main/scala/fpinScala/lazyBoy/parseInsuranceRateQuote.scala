def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Option[Double] = {
        val optAge : Option[Int] = Try(age.toInt)
        val optTickets : Option[Int] = Try(numberOfSpeedingTickets.toInt)
        insuranceRateQuote(optAge, optTickets)
    }

def Try[A](a: => A): Option[A] = {    //laziness
    try Some(a)
    catch (case e: Exception => None)
}

def map2_Option[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C]
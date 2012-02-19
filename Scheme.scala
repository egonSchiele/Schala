object Scheme {

  // wtf is this returning? seems like a lot of different types.
  // what do we do about stuff like "lambda" that has varying # of args?
  def eval(line: List[String]) = line match {
    case "if" :: test :: true_val :: false_val :: Nil => if (eval(test)) eval(true_val) else eval(false_val)
    case "quote" :: exp => exp
    case "set!" :: _var :: exp => env(_var) = eval(exp)
    case "define" :: _var :: exp => env += (_var -> eval(exp))
    // case "lambda" TODO
    // case "begin" :: TODO
    // some sort of method call
    case _ => {
      evald = line.map(eval)
      // how would this work? why is this now a function in scala?
      evald.head(evald.tail)
    }
  }

  def eval(str: String) = env(str)

  def eval(num: Int) = num
  def eval(num: Float) = num

  def main(args: Array[String]) {
    val a = List("a", "b", "C", "D")
    println(eval(a))
    println(eval(1))
  }

}

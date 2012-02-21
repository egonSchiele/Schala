abstract class SchemeVal
case class SchemeInt(value: Int) extends SchemeVal { override def toString() = value.toString }
case class SchemeFloat(value: Float) extends SchemeVal { override def toString = value.toString }
case class SchemeStr(value: String) extends SchemeVal { override def toString = value }
case class SchemeList(value: List[SchemeVal]=List[SchemeVal]()) extends SchemeVal { override def toString = "[" + value.map(_.toString).mkString(", ") + "]" }
case class SchemeFunc2(value: Function2[Int, Int, Int]) extends SchemeVal
case class SchemeNull extends SchemeVal { override def toString = "null" }

class Env(params: List[String] = List(), args: List[SchemeVal] = List(), outer: Env = null) {
  var inner =  (Map[String, SchemeVal]() /: (params.zip(args)))((acc: Map[String, SchemeVal], vals: Tuple2[String, SchemeVal]) => acc.update(vals._1, vals._2))
  
  // Should be None here, which means I should really be using an Option here
  def get(name: String): SchemeVal = if (inner.get(name).isEmpty) (if (outer != null) outer.get(name) else SchemeNull()) else inner.get(name).get
  def set(k: String, v: SchemeVal) = inner += (k -> v)
  def reset(k: String, v: SchemeVal) = inner = (inner(k) = v)

}

class SchemeInterpreter {

  def tokenize(line: String): List[String] = List.fromArray((("" /: line)((acc: String, c: Char) => if (c == '(') (acc + " ( ") else if (c == ')') (acc + " ) ") else (acc + c.toString))).trim.split(" ")).map(_.trim).filter(_ != "")

  var read = _read _ compose tokenize _

  def run(line: String, env: Env) = eval(read(line).value.head, env)

  def _read(tokens: List[String]): SchemeList = tokens match {
    case "(" :: _tokens => SchemeList(List(_read(_tokens)))
    case ")" :: _ => SchemeList()
    case Nil => SchemeList()
    case tok :: _tokens => SchemeList(List(atom(tok)) ::: _read(_tokens).value)
  }

  def atom(tok: String): SchemeVal = {
    try {
      SchemeInt(tok.toInt)
    } catch { 
      case e: NumberFormatException => try {
        SchemeFloat(tok.toFloat)
      } catch {
        case e: NumberFormatException => SchemeStr(tok)
      }
    }
  }

  def eval(line: SchemeVal, env: Env): SchemeVal = line match {
    case SchemeList(tokens) => tokens match {
      case SchemeStr("if") :: test :: true_val :: false_val :: Nil => if (eval(test, env).asInstanceOf[Boolean]) eval(true_val, env) else eval(false_val, env)
      case SchemeStr("quote") :: exp :: Nil => exp
      case SchemeStr("set!") :: (_var: SchemeStr) :: (exp: SchemeVal) :: Nil => { env.reset(_var.value, eval(exp, env)); SchemeNull() }
      case SchemeStr("define") :: (_var: SchemeStr) :: (exp: SchemeVal) :: Nil => {env.set(_var.value, eval(exp, env)); SchemeNull() }
      case SchemeStr("lambda") :: (_var1: SchemeStr) :: (_var2: SchemeStr) :: (exp: SchemeVal) :: Nil => SchemeFunc2((var1: Int, var2: Int) => (eval(exp, new Env(List(_var1.value, _var2.value), List(SchemeInt(var1), SchemeInt(var2)), env))).asInstanceOf[SchemeInt].value)
      case SchemeStr("begin") :: exp => exp.map(eval(_, env)).last
      // some sort of method call
      case func :: exp1 :: exp2 :: Nil => {
        var e_func = eval(func, env)
        e_func match {
          case (e_func: SchemeFunc2) => {
            var e_exp1 = eval(exp1, env).asInstanceOf[SchemeInt].value
            var e_exp2 = eval(exp2, env).asInstanceOf[SchemeInt].value
            SchemeInt(e_func.value(e_exp1, e_exp2))
          }
          case _ => SchemeStr("Error! Value is not a function or is not an appropriate function (we only support functions with the signature (Int, Int) => Int).")
        }
      }
    }
    case SchemeStr(value) => env.get(value)
    case SchemeInt(value) => line
    case SchemeFloat(value) => line
  }
}

object Scheme {
  def main(args: Array[String]) {
    var global_env = new Env(List("+", "-", "*", "/"), List(SchemeFunc2((_:Int) + (_:Int)), SchemeFunc2((_:Int) - (_:Int)), SchemeFunc2((_:Int) * (_:Int)), SchemeFunc2((_:Int) / (_:Int))     ))
    var s = new SchemeInterpreter()
    print("> ")
    for( ln <- io.Source.stdin.getLines ) {
      println( s.run(ln.trim, global_env) )
      print("> ")
    }
  }
}

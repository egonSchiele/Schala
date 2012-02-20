class Env(params: List[String] = List(), args: List[Any] = List(), outer: Env = null) {
  var inner = Map[String, Any]()
  (params.zip(args)).toMap.foreach(((t: Tuple2[String, Any]) => t match {case (k, v) => (inner.update(k, v))}))

  def get(name: String): Any = if (inner.get(name).isEmpty) (if (outer != null) outer.get(name) else None) else inner.get(name)
  def set(k: String, v: Any) = inner += (k -> v)
  def reset(k: String, v: Any) = inner(k) = v

}

class SchemeInterpreter {


  def split(line: String, acc: List[String]): List[String] = {
    if (line(0) == ' ' && line(1) != ' ')
      split(line.substring(2, line.length), acc ::: List(line(1).toString))
    else if (line(0) == ' ' && line(1) == ' ')
      split(line.substring(2, line.length), acc)
    else
      split(line.tail, acc.init + List(acc.last + line.head.toString))
  }

  def tokenize(line: String): List[String] = split(("" /: line)((acc: String, c: Char) => if (c == '(') (acc + " ( ") else if (c == ')') (acc + " ) ") else (acc + c.toString)), new List[String]())

  var read = _read _ compose tokenize _

  def run(line: String, env: Env) = {
    println("got: " + line)
    var evald = read(line)
    println("read: ")
    println(evald)
    evald.map{ (tok: Any) => {
      if (tok.isInstanceOf[List[Any]])
        eval(tok.asInstanceOf[List[String]], env)
      else if (tok.isInstanceOf[String])
        eval(tok.asInstanceOf[String], env)
      else if (tok.isInstanceOf[Int])
        eval(tok.asInstanceOf[Int], env)
      else
        throw new Exception("wtf")
    }
  }
}

  def _read(tokens: List[String]): List[Any] = tokens match {
    case "(" :: _tokens => {
      println("tokens: ")
      tokens.map(println)
      _read(List(_tokens.head)) ::: _read(_tokens.tail)
    }
    case ")" :: _ => {
      println("tokens: ")
      tokens.map(println)
      List[String]()
    }
    case tok :: _tokens => {
      println("tokens: ")
      tokens.map(println)
      List(atom(tok)) ::: _read(_tokens)
    }
  }

  def atom(tok: String) = {
    try {
      tok.toInt
    } catch { 
      case e: NumberFormatException => try {
        tok.toDouble
      } catch {
        case e: NumberFormatException => tok
      }
    }
  }

  def eval(line: List[String], env: Env): Any = line match {
    case "if" :: test :: true_val :: false_val :: Nil => if (eval(test, env).asInstanceOf[Boolean]) eval(true_val, env) else eval(false_val, env)
    case "quote" :: exp => exp
    case "set!" :: _var :: exp => env.reset(_var, eval(exp, env).asInstanceOf[String])
    case "define" :: _var :: exp => env.set(_var, eval(exp, env).asInstanceOf[String])
    case "lambda" :: _var :: exp => ((args: List[Any]) => eval(exp, new Env(List(_var), args, env)))
    case "begin" :: exp => exp.map(eval(_, env)).last
    // some sort of method call
    case func :: exp => {
      var e_func = eval(func, env).asInstanceOf[Function1[Any, Any]]
      var e_exp = exp.map(eval(_, env))
      e_func(e_exp)
    }
  }

  def eval(str: String, env: Env) = env.get(str)
  def eval(num: Int, env: Env) = num
  def eval(num: Float, env: Env) = num
}

object Scheme {
  def main(args: Array[String]) {
    var global_env = new Env(List("+", "-"), List(((_:Int) + (_:Int)), ((_:Int) - (_:Int))))
    var s = new SchemeInterpreter()
    for( ln <- io.Source.stdin.getLines ) {
      println( s.run(ln.trim, global_env) )
      println(global_env.inner)
    }
  }
}

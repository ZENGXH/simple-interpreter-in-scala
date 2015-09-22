package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    // Initialize the context for the main method
    val ectx = new MainMethodContext

    // Evaluate each statement of the main method
    prog.main.stats.foreach(evalStatement(ectx, _))
  }
  
  // CONTEXT COMBINE: 
  // add the field of the object into the current context
  // context(main or method) + obj => context'(method)
  def interContext(obj: ObjectValue, meth: MethodDecl,paras: List[ExprTree],ectx: EvaluationContext): MethodContext = {
    val mecx = new MethodContext(obj)
    
    // s1: fields of object add to the context
    if(!obj.fields.isEmpty) mecx.vars = obj.fields
    
    // s2: pass the argument, 
    // get value form methcall.vars(need to be evaluation under old context)
    if(!meth.args.isEmpty)
      for(i <- 0 to meth.args.size - 1) {
        mecx.declareVariable(meth.args(i).id.value) 
        mecx.setVariable(  meth.args(i).id.value  , evalExpr(ectx,paras(i)) )
    }
     
    // s3: methdecl also has it varlist -- to be declare in the context
    if(!meth.vars.isEmpty)
      for(v <- meth.vars) 
        // the val in method may have same name with classfield -- override
        if(!mecx.vars.contains(v.id.value)) 
          mecx.declareVariable(v.id.value)

    return mecx
  } 
  
/**
 *  everytime get in to evalStatements: 
 *  context may be change, here is pass by reference
 *  MainMethodContext do not contain vars
 */
  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = {   

//    println("\n -------the statement is " + stmt)
    ectxTraceLol(ectx)
    stmt match {
       

    case Block(stats) => for(stat <- stats)evalStatement(ectx, stat)  
//    println("  [ in Block")          
//      println("block done")
    
    
  /**
   * semantic 
   * Env => true + expr
   * -- Env => thn + Env
   * 
   * Env => true + expr
   * -- Env => els + Env
   *      * if expr call method
   */
    case If(expr, thn, els) => {
//      println("  [in If ")      
      if(evalExpr(ectx,expr).asBool) evalStatement(ectx, thn)
      else els.foreach(evalStatement(ectx, _))
    }

        
    /**
     * semantics:
     * Env => exp + false 
     * -- Env => while(exp) stm + Env
     * or
     * Env => exp + trune
     * -- Env => stm + Env'
     * --- Env' => while(exp) stm + Env'
     * ---- Env => while(exp) stm + Env''
     */
    case While(expr, stat) => {
      //println("  [in while")
      if(evalExpr(ectx,expr).asBool) {
          evalStatement(ectx, stat)
          evalStatement(ectx,stmt)  // excecute the statement
       } 
    }
    
   
    case Println(expr) => {
     
      val str2p = evalExpr(ectx,expr)
      str2p match{
       case StringValue(s) => println(s)
       case IntValue(s) => println(s)
       case BoolValue(s) => println(s)
       case ArrayValue(entries,size) => println(entries)
       case ObjectValue(cd) => println("object"+cd.id.value)
       case _ => println("ERROR: value in println is not match.")
     }
    }
    
    /**
     * eg: util = new util
     * 
     * semantic 
     * Env => stm + id
     * -- Env => x = exp +　Env'[x = val]
     */
    case Assign(id, expr) => {
 //     println("***** before ASSIGH " + id.value )
      
      val rhs = evalExpr(ectx,expr)
      
  //    println("before set ")    
      
      // when we assigh value to a variable, the variable should be already define
      ectx.setVariable(id.value, rhs)
      
      // if the value is also the field of the object --> should be update
      if(ectxTraceLol(ectx)){
        if(ectx.asInstanceOf[MethodContext].obj.fields.contains(id.value))
          ectx.asInstanceOf[MethodContext].obj.setField(id.value,rhs)
      }
      
    }
    
    case ArrayAssign(id, index, expr) => {
      // println("  [in arrayassign")
      
      val arr = evalExpr(ectx,id).asArray
      arr.setIndex(evalExpr(ectx,index).asInt,evalExpr(ectx,expr).asInt)
      
      // update the ArrayAssign into the context 
      ectx.asInstanceOf[MethodContext].setVariable(id.value,arr)
      
       if(ectxTraceLol(ectx)){
        if(ectx.asInstanceOf[MethodContext].obj.fields.contains(id.value))
          ectx.asInstanceOf[MethodContext].obj.setField(id.value,arr)
      }
    }
  }
    
}
  
  def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = {
//    println("-- eva expr: " + e)
//    ectxTraceLol(ectx)
    e match {
   
    case IntLit(value)    => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True()           => BoolValue(true)
    case False()          => BoolValue(false)
    case And(lhs, rhs) => {
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => if(l == 0 && r ==0)true else false
        case (BoolValue(l), BoolValue(r)) => l && r
        case (lr, rr) => fatal("please deal with And for " + lr + rr)
      }
      BoolValue(res)      
    }
    
    case Or(lhs, rhs)  => {
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => if(l == 0 || r ==0)true else false
        case (BoolValue(l), BoolValue(r)) => l || r
        case (lr, rr) => fatal("please deal with Or for " + lr + rr)
      }
      BoolValue(res)      
    }
    
    case Plus(lhs, rhs) => {
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => return IntValue(l + r)
        case (StringValue(l), StringValue(r)) => return StringValue(l+r)
        case (StringValue(l), IntValue(r)) => return StringValue(l + r.toString)
        case (IntValue(l), StringValue(r)) => return StringValue(l.toString + r)
        case (lr, rr) => fatal("please deal with plus for " + lr + rr)
      }
    }

    
    case Minus(lhs, rhs) => {
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => return IntValue(l - r)
        case (lr, rr) => fatal("please deal with minus for " + lr + rr)
      }
    }
    case Times(lhs, rhs) => {
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => return IntValue(l * r)
        case (lr, rr) => fatal("please deal with times for " + lr + rr)
      }
    }
    case Div(lhs, rhs) => {
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => return IntValue(l / r)
        case (lr, rr) => fatal("please deal with div for " + lr + rr)
      }
    }
    case LessThan(lhs, rhs) => {
      val lv = evalExpr(ectx,lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => if(l<r)return BoolValue(true)else return BoolValue(false)
        case (lr, rr) => fatal("please deal with lessthan for " + lr + rr)
      }
    }
    
    case Not(expr) => {
      val e = evalExpr(ectx,expr)
      e match {
        case (IntValue(eInt)) => return IntValue(-eInt)
        case _ =>{
          val boo = evalExpr(ectx,expr)
          boo match{
            case BoolValue(b) => return new BoolValue(!b)
            case _ => fatal("Not operator found in NOT " + boo)}
        }
      }
    }
    
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) => {
//      println("  [in ArrayRead")
      return (new IntValue(evalExpr(ectx,arr).asArray.getIndex(evalExpr(ectx,index).asInt)))
    }
    
    case ArrayLength(arr) => {
//      println("  [in ArrayLength")
      return (new IntValue(evalExpr(ectx,arr).asArray.size))
    }
    
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    case MethodCall(obj, meth, args) => {
//     println("\n METHOD CALL: " + meth.value + " with arg: "+ args )
//     println("\t obj: "+obj + " \t\t meth: " + meth +"\t\t\t args: " + args)

      val objThis = evalExpr(ectx, obj).asObject
//      println("i method call in class " + objThis.cd.id.value + " findmeth " + meth.value)
      // FIND methDecl(id, args, retExpr, stats)
      val methCall = findMethod(objThis.cd,meth.value) 
      
      // deal with the change of the context
      val mectx = interContext(objThis,methCall,args,ectx)
      
      // ready to run the statements, which will change the context
      methCall.stats.foreach(evalStatement(mectx,_))
      
      return evalExpr(mectx,methCall.retExpr)
    }
      
  
  
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    case Identifier(name) => {
       // println("identifier: "+name + " context " + context.asvars)

        
        if(ectx.asInstanceOf[MethodContext].vars.contains(name))
           return ectx.asInstanceOf[MethodContext].getVariable(name)

        else fatal("cannot find id: " + name)
            
    }
    
    /**
     * semantic:
     * Env(main or method) + tpe: ClassID
     * 
     * if MainEnv:
     * -- v: ObjValue + change MainEnv into MethodEnv(v)
     * 
     * if MethodEnv:
     * -- v: ObjValue + MethodEnv'(obj change to ->v)
     */
    case New(tpe) =>{
//      println("call New with tpe: "+ tpe.value)

      val classIn = findClass(tpe.value)  // ClassDeclaration by the name of the class 
//      println("find ClassDeclration with \n\tid: "+ classIn.id + "\n\tparents" + classIn.parent + "\n\tmethods: "+classIn.methods + "\n\tvars: " + classIn.vars)
      
      val newObj = new ObjectValue(classIn)
//      println("create a new object: " + newObj + " as instance of " + classIn.id)
      
      // fields of the class -> objectValue
      for(classField <- fieldsOfClass(classIn)) newObj.declareField(classField)
      
      return newObj
    }
    
    case This() => {
//      println("calling this with context " + ectx.asInstanceOf[MethodContext].vars + " \n and the filed is "+ ectx.asInstanceOf[MethodContext].obj.fields)
//      println("this done!!! ")     
      return ectx.asInstanceOf[MethodContext].obj
      }
          
    case NewIntArray(size) => {
      val len:Int = evalExpr(ectx,size).asInt 
      val arr:Array[Int] = new Array[Int](len)
      return (new ArrayValue(arr,len))
    }
  }
  }// end of evalExpr()
  

  
  // Define the scope of evaluation, with methods to access/declare/set local variables(or arguments)
  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  // A Method context consists of the execution context within an object method.
  // getVariable can fallback to the fields of the current object
  case class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
//     println(" \t set variable: " + name + " === " + v)
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
 //     println("\t declare variable: "+ name + " in " + obj.cd.id.value)
      vars += name -> None
    }
  }

  
  // Special execution context for the main method, which is very limitted.
  case class MainMethodContext() extends EvaluationContext {
    def getVariable(name: String): Value          = fatal("The main method contains no variable and/or field")
    def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field")
    def declareVariable(name: String): Unit       = fatal("The main method contains no variable and/or field")
  }

  
  // Helper functions to query the current program
  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
//    println("findMethod in class: " + cd.id )
    cd.methods.find(_.id.value == name).orElse(
        // find whether there is a method named 'name' in the subclass
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name + cd.parent))
  }

  def findClass(name: String): ClassDecl = {
 //   println("findClass with name: " + name)
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }
  
  
  
  // Runtime evaluation values, with as* methods which act as typecasts for convenience.
  sealed abstract class Value {
    def asInt: Int            = fatal("Unnexpected value, found "+this+" expected Int")
    def asString: String      = fatal("Unnexpected value, found "+this+" expected String")
    def asBool: Boolean       = fatal("Unnexpected value, found "+this+" expected Boolean")
    def asObject: ObjectValue = fatal("Unnexpected value, found "+this+" expected Object")
    def asArray: ArrayValue   = fatal("Unnexpected value, found "+this+" expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()
    
    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal("Unknown field '"+name+"'")
      }
    }

    def getField(name: String) = {
      fields.get(name).flatten.getOrElse(fatal("Unknown field '"+name+"'"))
    }

    def declareField(name: String) {
      if(fields.contains(name)){}
      else
      fields += name -> None
    }

    override def asObject = {
 //     println("in class "+ cd.id.value +" the fields of objectvalue " + fields)
      this
    }
  }

  case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
    def setIndex(i: Int, v: Int) {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(var v: String) extends Value {
    override def asString = v
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }
  
  
  
def ectxTraceLol(ectx: EvaluationContext):Boolean = {
  ectx match {
    case MethodContext(obj) => {
//      println("context: method, in obj " + obj.cd.id.value + "\n vars: " + ectx.asInstanceOf[MethodContext].vars + "\nobject field " + obj.fields)
      true
    }
      
    case MainMethodContext() =>{ 
//      println("context: Main \n no vars: ")
      false
    }
  }
}


}

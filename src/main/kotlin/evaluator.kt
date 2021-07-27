import kotlinx.collections.immutable.PersistentMap
import kotlin.Exception

//AST => Values

sealed class Value {
    data class Number(val i: Int) : Value()
    data class Bool(val b: Boolean) : Value()
    data class Var(val b: String, val v: Value) : Value()
    data class Block(val block: List<Value>) : Value()
    data class Function(val body: List<Expr>, val arguments: List<Expr>) : Value()
    object Void : Value()
}
typealias env = MutableMap<String, Value>

fun eval(env: env, e: List<Expr>): List<Value> {
    val res = mutableListOf<Value>()
    for (expr in e) {
        res.add(evalExpr(env, expr))
    }
    return res
}

fun evalExpr(env: env, e: Expr): Value {
    return when (e) {
        is Expr.Num -> Value.Number(e.i)
        is Expr.Bool -> Value.Bool(e.b)
        is Expr.If -> {
            val cond = evalExpr(env, e.cond) as? Value.Bool ?: throw Exception("Not a Boolean")
            if (cond.b) {
                eval(env, e.e1).last()
            } else {
                eval(env, e.e2).last()
            }
        }
        is Expr.Binary -> {
            when (e.operator) {
                BinOp.Equals -> evalBool(evalExpr(env, e.i1), evalExpr(env, e.i2)) { x, y -> x == y }
                BinOp.GreaterThan -> evalBool(evalExpr(env, e.i1), evalExpr(env, e.i2)) { x, y -> x > y }
                BinOp.LessThan -> evalBool(evalExpr(env, e.i1), evalExpr(env, e.i2)) { x, y -> x < y }
                BinOp.Addition -> evalNum(evalExpr(env, e.i1), evalExpr(env, e.i2)) { x, y -> x + y }
                BinOp.Subtraction -> evalNum(evalExpr(env, e.i1), evalExpr(env, e.i2)) { x, y -> x - y }
                BinOp.Multiplication -> evalNum(evalExpr(env, e.i1), evalExpr(env, e.i2)) { x, y -> x * y }
            }
        }
        is Expr.VarAssignment -> {
            val ex = evalExpr(env, e.expr)
            env.put(e.binder, ex)
            Value.Var(e.binder, ex)
        }
        is Expr.VarDeclaration -> {
            env.put(e.binder, Value.Void)
            Value.Var(e.binder, Value.Void)
        }
        is Expr.VarInitialization -> {
            val ex = evalExpr(env, e.expr)
            if (env[e.binder] != null && env[e.binder] !is Value.Void) throw Exception("Redeclaration of Variable is not allowed")
            env.put(e.binder, ex)
            Value.Var(e.binder, ex)
        }
        is Expr.Var -> {
            if (env[e.b] is Value.Void) throw Exception("Var ${e.b} is declared but not initialized")
            env[e.b] ?: throw Exception("Var ${e.b} is not defined")
        }
        is Expr.Function -> {
            val function = Value.Function(e.body, e.args)
            if (env["FUNC:" + e.binder] != null) throw Exception("Function ${e.binder} is already defined")
            env.put("FUNC:" + e.binder, function)
            Value.Function(e.body, e.args)
        }
        is Expr.FuncCall -> {
            val func =
                env["FUNC:" + e.binder] as? Value.Function ?: throw Exception("Function ${e.binder} is not defined")
            val tmp = env.filter { true } as MutableMap
            try {
                for (i in func.arguments.indices) {
                    val key = func.arguments[i] as Expr.Var
                    val expr = evalExpr(env, e.args[i])
                    tmp.put(key.b, expr)
                }
            } catch (e: IndexOutOfBoundsException) {
                println("Argument Count of Function Call did not match the Argument Count of the Function")
            }
            when (val ev = eval(tmp, func.body).last()) {
                is Value.Block -> {
                    ev.block.last()
                }
                else -> {
                    ev
                }
            }
        }
        else -> throw Exception("Expression not defined $e")
    }
}

fun evalBool(i1: Value, i2: Value, f: (Int, Int) -> Boolean): Value {
    val x = i1 as? Value.Number ?: throw Exception("$i1 is not a number")
    val y = i2 as? Value.Number ?: throw Exception("$i2 is not a number")
    return Value.Bool(f(x.i, y.i))
}

fun evalNum(i1: Value, i2: Value, f: (Int, Int) -> Int): Value {
    val x = i1 as? Value.Number ?: throw Exception("$i1 is not a number")
    val y = i2 as? Value.Number ?: throw Exception("$i1 is not a number")
    return Value.Number(f(x.i, y.i))
}

fun main() {
    /*
    val f = Expr.If(
            Expr.Var("x"),
            Expr.Binary(BinOp.Addition, Expr.Num(5), Expr.Num(9)),
            Expr.Binary(BinOp.Multiplication, Expr.Num(10), Expr.Num(9)))

    val t = Expr.Let("x", Expr.Bool(true), f)

    val env : env = mutableMapOf()

    print(eval(env , t))
    */
}




























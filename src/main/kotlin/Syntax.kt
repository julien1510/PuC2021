sealed class Expr {
    data class Var(val b: String) : Expr()
    data class Num(val i: Int) : Expr()
    data class Bool(val b: Boolean) : Expr()
    data class If(val cond: Expr, val e1: List<Expr>, val e2: List<Expr>) : Expr()
    data class Binary(val operator: BinOp, val i1: Expr, val i2: Expr) : Expr()
    data class VarAssignment(val binder: String, val expr: Expr) : Expr()
    data class VarInitialization(val binder: String, val expr: Expr) : Expr()
    data class VarDeclaration(val binder: String) : Expr()
    data class Function(val binder: String, val args: List<Expr>, val body: List<Expr>) : Expr()
    data class FuncCall(val binder: String, val args: List<Expr>) : Expr()
    data class Return(val expr: Expr? = null) : Expr()
}

enum class BinOp {
    Equals, GreaterThan, LessThan, Addition, Subtraction, Multiplication
}
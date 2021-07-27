enum class Rule(val match: String, val size: Int) {
    NumToExpr("NUM", 1),
    Addition("EXPR" + "PLUS" + "EXPR", 3),
    Multiplication("EXPR" + "MUL" + "EXPR", 3),
    Subtraction("EXPR" + "MINUS" + "EXPR", 3),
    Equals("EXPR" + "DEQUALS" + "EXPR", 3),
    GreaterThan("EXPR" + "GTHAN" + "EXPR", 3),
    LessThan("EXPR" + "LTHAN" + "EXPR", 3),
    Parenthesis("LPAREN" + "EXPR" + "RPAREN", 3),
    BoolToExpr("BOOL", 1),

    BeExpr("BE" + "IDENT" + "EQUALS" + "EXPR", 4),
    IfExpr("NLWHITESPACE$0" + "IF" + "EXPR" + "COL" + "NLWHITESPACE$1" + "BLOCK" + "NLWHITESPACE$0" + "ELSE" + "COL" + "NLWHITESPACE$1" + "BLOCK", 11),
    Var("IDENT", 1),
    VarAssignment("IDENT" + "EQUALS" + "EXPR" , 3),
    Newline("NLWHITESPACE$0" + "NLWHITESPACE$1", 2),
    Block("EXPR",1),
    BlockSeq("NLWHITESPACE$0" + "BLOCK" + "NLWHITESPACE$0" + "BLOCK", 4),
    Function("NLWHITESPACE$0" + "DEF" + "IDENT" + "ARGS" + "COL" + "NLWHITESPACE$1" + "BLOCK", 7),
    FunctionCall("IDENT" + "ARGS", 2),
    ArgList("IDENT" + "EXPR", 2),
    ArgListSeq("ARGS" + "COMMA" + "EXPR", 3)
}
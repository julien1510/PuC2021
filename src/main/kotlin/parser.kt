import java.util.*
import kotlin.Exception

sealed class Token {

    override fun toString(): String {
        return this.javaClass.simpleName
    }

    fun toTypeString(): String {
        if (this is NUMBER_LIT) return "NUM"
        if (this is BOOLEAN_LIT) return "BOOL"
        if (this is EXPR) return "EXPR"
        if (this is IDENT) return "IDENT"
        if (this is NLWHITESPACE) return "NLWHITESPACE" + this.count
        if (this is BLOCK) return "BLOCK"
        if (this is ARGS) return "ARGS"
        return this.toString()
    }

    object IF : Token()
    object ELSE : Token()
    object WHILE : Token()
    object GOTO : Token()
    object BE : Token()
    object RET : Token()
    object DEF : Token()

    object LPAREN : Token()
    object RPAREN : Token()
    object LBRACK : Token()
    object RBRACK : Token()
    object EQUALS : Token()
    object DEQUALS : Token()
    object GTHAN : Token()
    object LTHAN : Token()
    object COMMA : Token()

    //object WHITESPACE : Token()
    //object NEWLINE : Token()
    data class NLWHITESPACE(val count: Int) : Token()
    object COL : Token()

    object PLUS : Token()
    object MINUS : Token()
    object MUL : Token()

    data class BOOLEAN_LIT(val b: Boolean = false) : Token()
    data class NUMBER_LIT(val i: Int = 0) : Token()

    data class IDENT(val b: String) : Token()

    object EOF : Token()

    // Meta Characters for Grammar
    data class EXPR(val expr: Expr) : Token()
    data class BLOCK(val expressions: MutableList<Expr> = mutableListOf()) : Token()
    data class ARGS(val expressions: MutableList<Expr> = mutableListOf()) : Token()
}

class PeekableIterator<A>(private val iter: Iterator<A>) {

    private var lookahead: A? = null
    private var dlookahead: A? = null

    fun next(): A {
        lookahead?.let { lookahead = null; dlookahead?.let { lookahead = dlookahead; dlookahead = null }; return it }
        return iter.next()
    }

    fun dnext() {
        next()
        next()
    }

    fun peek(): A {
        lookahead?.let { return it }
        val token = next()
        lookahead = token
        return token
    }

    fun dpeek(): A? {
        val token1 = next()
        if (!iter.hasNext()) return null
        val token2 = next()
        lookahead = token1
        dlookahead = token2
        return token2
    }

    fun hasNext(): Boolean {
        return lookahead != null || iter.hasNext()
    }

    fun dhasNext(): Boolean {
        return if (iter.hasNext()) (dpeek() != null) else false
    }
}

// normal Lexer

class Lexer(input: String) {

    private val iter = PeekableIterator(input.iterator())
    private var lookahead: Token? = null
    private var dlookahead: Token? = null

    fun peek(): Token {
        lookahead?.let { return it }
        val token = next()
        lookahead = token
        return token
    }

    fun dpeek(): Token? {
        val token1 = next()
        if (!iter.hasNext()) return null
        val token2 = next()
        lookahead = token1
        dlookahead = token2
        return token2
    }

    fun hasNext(): Boolean {
        return lookahead != null || iter.hasNext()
    }

    fun next(): Token {
        lookahead?.let { lookahead = null; dlookahead?.let { lookahead = dlookahead; dlookahead = null }; return it }

        consumeWhitespace()

        if (iter.dhasNext()) {
            skipComments()
        }

        if (!iter.hasNext()) {
            return Token.EOF
        }

        return when (val c = iter.next()) {
            '=' -> {
                if (iter.peek() == '=') {
                    iter.next()
                    Token.DEQUALS
                } else {
                    Token.EQUALS
                }
            }
            '\r' -> {
                if (iter.hasNext() && iter.peek() == '\n') {
                    iter.next()
                    val count = countWhiteSpace()
                    Token.NLWHITESPACE(count)
                } else if (!iter.hasNext()) {
                    return Token.EOF
                } else {
                    throw Exception("Expected Newline Character but got $c")
                }
            }

            '\n' -> {
                val count = countWhiteSpace()
                Token.NLWHITESPACE(count)
            }
            '<' -> Token.LTHAN
            '>' -> Token.GTHAN
            '+' -> Token.PLUS
            '-' -> Token.MINUS
            '*' -> Token.MUL
            '(' -> Token.LPAREN
            ')' -> Token.RPAREN
            ':' -> Token.COL
            ',' -> Token.COMMA
            else -> return when {
                c.isJavaIdentifierStart() -> ident(c)
                c.isDigit() -> digit(c)
                else -> {
                    throw Exception("unexpected char $c")
                }
            }
        }
    }

    private fun consumeWhitespace() {
        while (iter.hasNext()) {
            val c = iter.peek()
            if (c != ' ') break
            iter.next()
        }
    }

    private fun countWhiteSpace(): Int {
        var cnt = 0
        while (iter.hasNext()) {
            val c = iter.peek()
            if (c != ' ') break
            cnt++
            iter.next()
        }
        return cnt
    }

    private fun skipComments(): Token? {
        if (iter.peek() == '/' && iter.dpeek() == '/') {
            iter.dnext()
            skipLineComment()
            if (!iter.hasNext()) return Token.EOF
            return null
        }

        if (iter.peek() == '/' && iter.dpeek() == '-') {
            iter.dnext()
            skipBlockComment()
            if (!iter.hasNext()) return Token.EOF
            return null
        }
        return null
    }

    // Überspringe Comments: NEWLINE Charaktere wie LINEFEED oder CARRIAGE RETURN in der Kommentarzeile werden ebenfalls übersprungen
    private fun skipLineComment() {
        while (iter.dhasNext() && iter.peek() != '\r' && iter.dpeek() != '\n') {
            iter.next()
        }
    }

    private fun skipBlockComment() {
        if (!iter.dhasNext()) throw Exception("Block Comment not closed")
        while (!(iter.peek() == '-' && iter.dpeek() == '/')) {
            if (!iter.dhasNext()) throw Exception("Block Comment not closed")
            iter.next()
        }
        iter.dnext()
    }

    private fun skipNewline() {
        if (iter.dhasNext() && iter.peek() == '\r' && iter.dpeek() == '\n') {
            iter.dnext()
        } else if (iter.hasNext() && iter.peek() == '\n') {
            iter.next()
        } else if (!iter.dhasNext()) return
    }

    private fun ident(c: Char): Token {
        var str = c.toString()
        while (iter.hasNext() && iter.peek().isJavaIdentifierPart()) {
            str += iter.next().toString()
        }
        return when (str) {
            "def" -> Token.DEF
            "if" -> Token.IF
            "else" -> Token.ELSE
            "goto" -> Token.GOTO
            "while" -> Token.WHILE
            "be" -> Token.BE
            "true" -> Token.BOOLEAN_LIT(true)
            "false" -> Token.BOOLEAN_LIT(false)
            "return" -> Token.RET
            else -> Token.IDENT(str)
        }
    }

    private fun digit(c: Char): Token {
        var dig = c.toString()
        while (iter.hasNext() && iter.peek().isDigit()) {
            dig += iter.next()
        }
        return Token.NUMBER_LIT(dig.toInt())
    }
}

// Shift Reduce Parser

class Parser(private val tokens: Lexer) {
    private val workStack = Stack<Token>()

    init {
        workStack.push(Token.NLWHITESPACE(0))
    }

    private fun shift(token: Token) {
        workStack.push(token)
    }

    private fun reduce(): Boolean {
        if (match(Rule.Parenthesis, workStack)) {
            return parenthesis()
        }
        if (match(Rule.NumToExpr, workStack)) {
            parseNumber(); return true
        }
        if (match(Rule.BoolToExpr, workStack)) {
            parseBool(); return true
        }
        if (match(Rule.Multiplication, workStack)) {
            return parseBinary(BinOp.Multiplication)
        }
        if (match(Rule.Subtraction, workStack)) {
            return parseBinary(BinOp.Subtraction)
        }
        if (match(Rule.Addition, workStack)) {
            return parseBinary(BinOp.Addition)
        }
        if (match(Rule.Equals, workStack)) {
            return parseBinary(BinOp.Equals)
        }
        if (match(Rule.GreaterThan, workStack)) {
            return parseBinary(BinOp.GreaterThan)
        }
        if (match(Rule.LessThan, workStack)) {
            return parseBinary(BinOp.LessThan)
        }
        if (match(Rule.BeExpr, workStack)) {
            return parseBe()
        }
        if (match(Rule.Var, workStack)) {
            return parseVar()
        }
        if (match(Rule.VarAssignment, workStack)) {
            return parseVarAssignment()
        }
        if (match(Rule.ArgListSeq, workStack)) {
            return parseArgListSeq()
        }
        if (match(Rule.ArgList, workStack)) {
            return parseArgList()
        }
        if (match(Rule.Newline, workStack)) {
            return parseNewline()
        }
        if (match(Rule.BlockSeq, workStack)) {
            return parseBlockSeq()
        }
        if (match(Rule.Block, workStack)) {
            return parseBlock()
        }
        if (match(Rule.IfExpr, workStack)) {
            return parseIf()
        }
        if (match(Rule.Function, workStack)) {
            return parseFunction()
        }
        if (match(Rule.FunctionCall, workStack)) {
            return parseFuncCall()
        }
        return false
    }

    private fun match(pattern: Rule, big: List<Token>): Boolean {
        if (pattern.size > big.size) return false
        val bigSubList = big.subList(big.size - pattern.size, big.size)
        val bigPattern = bigSubList.fold("") { acc, next ->
            acc + next.toTypeString()
        }
        val patSub = substituteWildcards(pattern, bigPattern) ?: pattern.match
        if (bigPattern == patSub) return true
        return false
    }

    fun substituteWildcards(pattern: Rule, big: String): String? {
        if (!pattern.match.contains('$')) return null

        val regexFindNumbers = Regex("[0-9]+")
        val regexSubstitute = Regex("""\$[0-9]+""")
        var pat = pattern.match

        val subs = regexSubstitute.findAll(pattern.match).toList()
        val varMap = mutableMapOf<String, String>()
        val values = regexFindNumbers.findAll(big).toList()

        if (values.size != subs.size) return null

        for ((index, value) in values.withIndex()) {
            if (varMap[subs[index].value] != null) continue
            varMap[subs[index].value] = value.value
        }

        for (variable in varMap) {
            val tmp = pat.replace(variable.key, variable.value)
            pat = tmp
        }

        return pat
    }

    fun parseFunction(): Boolean {
        val block = workStack.pop() as Token.BLOCK
        val indent2 = workStack.pop() as Token.NLWHITESPACE
        if (tokens.peek() is Token.NLWHITESPACE && (tokens.peek() as Token.NLWHITESPACE).count == indent2.count) {
            workStack.push(indent2)
            workStack.push(block)
            return false
        }
        workStack.pop()
        val args = workStack.pop() as Token.ARGS
        val ident = workStack.pop() as Token.IDENT
        workStack.pop() // Token.DEF
        workStack.pop() // Token.NLWHITESPACE
        val expr = Expr.Function(ident.b, args.expressions.toList(), block.expressions.toList())
        workStack.push(Token.EXPR(expr))
        return true
    }

    fun parseFuncCall(): Boolean {
        if (tokens.peek() is Token.COL || tokens.peek() is Token.COMMA) return false
        val args = workStack.pop() as Token.ARGS
        val ident = workStack.pop() as Token.IDENT
        workStack.push(Token.EXPR(Expr.FuncCall(ident.b, args.expressions)))
        return true
    }

    fun parseNewline(): Boolean {
        if (!tokens.hasNext()) return false
        val bottom = workStack.pop() as Token.NLWHITESPACE
        workStack.pop()
        workStack.push(bottom)
        return true
    }

    fun parseArgList(): Boolean {
        val expr = workStack.pop() as Token.EXPR
        val ident = workStack.pop()
        workStack.push(ident)
        workStack.push(Token.ARGS(mutableListOf(expr.expr)))
        if (tokens.peek() is Token.COMMA) {
            shift(tokens.next())
            shift(tokens.next())
        }
        return true
    }

    fun parseArgListSeq(): Boolean {
        val peek = tokens.peek()
        if (peek !is Token.COL && peek !is Token.COMMA && peek !is Token.NLWHITESPACE && peek !is Token.EOF && peek !is Token.RPAREN) return false
        val expr = workStack.pop() as Token.EXPR
        workStack.pop()
        val args = workStack.pop() as Token.ARGS
        args.expressions.add(expr.expr)
        workStack.push(args)
        return true
    }

    fun parseBlockSeq(): Boolean {
        val block2 = workStack.pop() as Token.BLOCK
        workStack.pop() // Token.NLWHITESPACE
        val block1 = workStack.pop() as Token.BLOCK
        val indent1 = workStack.pop() as Token.NLWHITESPACE
        for (expr in block2.expressions) {
            block1.expressions.add(expr)
        }
        workStack.push(indent1)
        workStack.push(block1)
        return true
    }

    fun parseBlock(): Boolean {
        if (tokens.peek() != Token.EOF && tokens.peek() !is Token.NLWHITESPACE) return false
        val expr = workStack.pop() as Token.EXPR
        workStack.push(Token.BLOCK(mutableListOf(expr.expr)))
        return true
    }

    fun parseIf(): Boolean {
        val expr2 = workStack.pop() as Token.BLOCK
        val indentExpr2 = workStack.pop() as Token.NLWHITESPACE
        val peekIdent = tokens.peek() as? Token.NLWHITESPACE
        if (peekIdent is Token.NLWHITESPACE && peekIdent.count == indentExpr2.count) {
            workStack.push(indentExpr2)
            workStack.push(expr2)
            return false
        }
        workStack.pop() // Token.ELSE
        workStack.pop() // Token.COL
        workStack.pop() // Token.NLWHITESPACE
        val expr1 = workStack.pop() as Token.BLOCK
        workStack.pop() // Token.NLWHITESPACE
        workStack.pop() // Token.COL
        val cond = workStack.pop() as Token.EXPR
        workStack.pop() // Token.IF
        val indentIf = workStack.pop() as Token.NLWHITESPACE
        workStack.push(indentIf)
        workStack.push(Token.EXPR(Expr.If(cond.expr, expr1.expressions, expr2.expressions)))
        return true
    }

    fun parseBe(): Boolean {
        if (tokens.peek() != Token.EOF && tokens.peek() !is Token.NLWHITESPACE && tokens.peek() !is Token.COMMA) return false
        val expr = workStack.pop() as Token.EXPR
        workStack.pop()
        val ident = workStack.pop() as Token.IDENT
        workStack.pop()
        val token = Token.EXPR(Expr.VarInitialization(ident.b, expr.expr))
        workStack.push(token)
        return true
    }

    fun parseVar(): Boolean {
        if (tokens.peek() == Token.EQUALS || tokens.peek() == Token.LPAREN || tokens.peek() is Token.IDENT ||
            tokens.peek() is Token.NUMBER_LIT || tokens.peek() is Token.BOOLEAN_LIT
        ) return false
        val v = workStack.pop() as Token.IDENT
        workStack.push(Token.EXPR(Expr.Var(v.b)))
        return true
    }

    fun parseVarAssignment(): Boolean {
        if (tokens.peek() !is Token.NLWHITESPACE && tokens.peek() !is Token.EOF) return false
        val expr = workStack.pop() as Token.EXPR
        workStack.pop()
        val ident = workStack.pop() as Token.IDENT
        workStack.push(Token.EXPR(Expr.VarAssignment(ident.b, expr.expr)))
        return true
    }

    fun parseBinary(op: BinOp): Boolean {
        val pk = tokens.peek()
        if (pk == Token.MUL && op != BinOp.Multiplication) return false
        if ((pk == Token.MUL || pk == Token.PLUS || pk == Token.MINUS) && (op == BinOp.Equals || op == BinOp.GreaterThan || op == BinOp.LessThan)) return false
        val n2 = workStack.pop() as Token.EXPR
        workStack.pop()
        val n1 = workStack.pop() as Token.EXPR
        val binExpr = Expr.Binary(op, n1.expr, n2.expr)
        workStack.push(Token.EXPR(binExpr))
        return true
    }

    fun parenthesis(): Boolean {
        if (tokens.peek() is Token.COL) return false
        workStack.pop()
        val expr = workStack.pop()
        workStack.pop()
        workStack.push(expr)
        return true
    }

    fun parseNumber() {
        val number = workStack.pop() as Token.NUMBER_LIT
        workStack.push(Token.EXPR(Expr.Num(number.i)))
    }

    fun parseBool() {
        val bool = workStack.pop() as Token.BOOLEAN_LIT
        workStack.push(Token.EXPR(Expr.Bool(bool.b)))
    }

    fun parse(): Stack<Token> {
        while (true) {
            while (true) {
                if (!reduce()) break
            }
            if (tokens.peek() == Token.EOF) break
            shift(tokens.next())
        }
        //println(workStack)
        return workStack
    }
}

fun main() {
    val input = _FileReader().readFile("src\\main\\input\\sample.txt")
    val l = Lexer(input)

    /*
    while (l.hasNext()) {
        println(l.next())
    }
    */

    val parser = Parser(l)
    val res = parser.parse()
    println(res)
    val listExpr = mutableListOf<Expr>()
    for (token in res) {
        if (token is Token.NLWHITESPACE) continue
        val expr = token as Token.BLOCK
        listExpr.addAll(expr.expressions)
    }
    /*
    for (t in res) {
        if (t is Token.NLWHITESPACE) continue
        val token = t as Token.BLOCK
        exprList.add(token.expr)
    }
    */
    println(eval(mutableMapOf(), listExpr))
}
























package wacc
import AbstractSyntaxTree._
import wacc.AbstractSyntaxTree.UnaryOpType._
import wacc.AbstractSyntaxTree.BinaryOpType._
import parsley.Parsley
import parsley.combinator.{choice, some}
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.character.{letterOrDigit, stringOfMany}
import parsley.expr.{InfixL, Ops, Prefix, precedence}
import parsley.implicits.character.charLift

object Lexer {

  import parsley.token.descriptions.numeric.NumericDesc
  import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
  import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
  import parsley.token.predicate
  import parsley.token.Lexer

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic((c: Char) => (c == '_') || c.isLetter),
      identifierLetter = predicate.Basic((c: Char) => (c == '_') || c.isLetterOrDigit)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set("true", "false", "null", "len", "ord", "chr"),
      hardOperators = Set("!", "*", "/", "%", "+", "-", ">=", ">", "<", "<=", "==", "!=", "&&", "||")
    ),
    numericDesc = NumericDesc.plain.copy(
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy() //TODO: Add escape characters
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      space = predicate.Basic(Character.isWhitespace)
    )
  )

  private val lexer = new Lexer(desc)
  val integer: Parsley[Int] = lexer.lexeme.numeric.signed.decimal32
  val character: Parsley[Char] = lexer.lexeme.text.character.ascii
  val boolean: Parsley[Boolean] = (lexer.lexeme.symbol.apply("true", "true") #> true) <|>
                                  (lexer.lexeme.symbol.apply("false", "false") #> false)
  val string: Parsley[String] = lexer.lexeme.text.string.ascii
  val emptyPair: Parsley[Unit] = lexer.lexeme.symbol.apply("null", "null")
  val identifier: Parsley[String] = lexer.lexeme.names.identifier

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  val implicits = lexer.lexeme.symbol.implicits
}

object Parser {
  import Lexer._

  object Expression {
    import Lexer.implicits._

    private lazy val intLiteral = integer.map(IntLiteral)
    private lazy val boolLiteral = boolean.map(BoolLiteral)
    private lazy val charLiteral = character.map(CharLiteral)
    private lazy val stringLiteral = Lexer.string.map(StringLiteral)
    private lazy val pairLiteral = emptyPair #> PairLiteral()
    private lazy val ident = identifier
    private lazy val maybeArrayElem: Parsley[String => Expr] = {
      choice(some('[' ~> _parseExpr <~ ']').map(ArrayElem(_)), pure(IdentLiteral(_)))
    }

    lazy val parseExprAtom: Parsley[Expr] =
      intLiteral <|>
        boolLiteral <|>
        charLiteral <|>
        stringLiteral <|>
        pairLiteral <|>
        (ident <**> maybeArrayElem) // Both an identifier and an array element can start with an 'ident'

    // Not the nicest solution, but it works for now...:
    private def without[A](q: Parsley[A], p: Parsley[A]): Parsley[A] = attempt(p <~ notFollowedBy(q))
    private lazy val identCont = stringOfMany('_' <|> letterOrDigit)

    private lazy val _parseExpr: Parsley[Expr] = precedence(parseExprAtom, '(' ~> _parseExpr <~ ')')(
      Ops[Expr](Prefix)(without(intLiteral, "-") #> UnaryOp(Neg),
        "!" #> UnaryOp(Not), "len" #> UnaryOp(Len),
        "ord" #> UnaryOp(Ord), "chr" #> UnaryOp(Chr)),
      Ops[Expr](InfixL)("*" #> BinaryOp(Mul), "/" #> BinaryOp(Div), "%" #> BinaryOp(Mod)),
      Ops[Expr](InfixL)("+" #> BinaryOp(Add), "-" #> BinaryOp(Sub)),
      Ops[Expr](InfixL)(">=" #> BinaryOp(Gte), ">" #> BinaryOp(Gt)),
      Ops[Expr](InfixL)("<=" #> BinaryOp(Lte), "<" #> BinaryOp(Lt)),
      Ops[Expr](InfixL)("==" #> BinaryOp(Eq), "!=" #> BinaryOp(Neq)),
      Ops[Expr](InfixL)("&&" #> BinaryOp(And)),
      Ops[Expr](InfixL)("||" #> BinaryOp(Or))
    )

    lazy val parseExpr: Parsley[Expr] = fully(_parseExpr)
  }
}

package tw.lanyitin.common.ast

import scala.annotation.tailrec

trait ValueHolder {
  def typ: Type
}
case class Parameter(token: Token, typ: Type, idx: Int) extends ValueHolder
case class Variable(token: Token, typ: Type, idx: Int) extends ValueHolder
case class FunctionDeclaration(token: Token, parameters: List[Parameter], typ: Type) extends ValueHolder


trait TreeNode {
  def visualize: String
}

trait Expression extends TreeNode {
  def id = this.hashCode.toString.replace("-", "")
  def typ: Type
}
abstract class DeclExpression() extends Expression

case class VariableDefinitionExpression(variable: Variable, value: Expression)
    extends DeclExpression() {
  def typ: Type = variable.typ

  def visualize = {
    (this.value.visualize ::
      "%s -> %s".format(this.id, value.id) ::
      "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                       this.variable.token.txt,
                                       this.typ,
                                       this.variable.token.line,
                                       this.variable.token.col) :: Nil)
      .mkString("\n")
  }
}

case class FunctionDefinitionExpression(declaration: FunctionDeclaration,
                                        body: Expression)
    extends DeclExpression() {
  def typ: Type =
    FunctionType(declaration.parameters.map(_.typ), declaration.typ)

  def visualize = {
    val arg_types =
      declaration.parameters.map(_.typ).map(_.toString).mkString(",")
    val ret_type = declaration.typ
    val identifier = declaration.token

    (body.visualize ::
      s"${this.id} -> ${body.id}\n" ::
      "%s [label=\"%s: %s\"]".format(
      this.id,
      identifier.txt,
      typ.toString
    ) :: Nil).mkString("\n")
  }
}

case class ExprsBlock(exprs: List[Expression]) extends Expression {
  def typ = if (exprs.size > 0) exprs.last.typ else AnyValue

  def visualize = {
    ("%s [label=\"%s\"]".format(this.id, this.typ) ::
      exprs.map(_.visualize) :::
      exprs.map(a => s"${this.id} -> ${a.id}")).mkString("\n")
  }
}

abstract class ValueExpression() extends Expression

case class IfExpression(condition: Expression,
                        true_path: Expression,
                        false_path: Expression = new ExprsBlock(Nil))
    extends ValueExpression {
  def findCommonType(t1: Type, t2: Type): Type = {
    @tailrec
    def loop(t: Type, acc: List[Type]): List[Type] = {
      if (t.parent == t) {
        t :: acc
      } else {
        loop(t.parent, t :: acc)
      }
    }
    loop(t1, Nil).intersect(loop(t2, Nil)).last
  }
  def typ = {
    findCommonType(true_path.typ, false_path.typ)
  }
  def visualize = {
    val exprs = (condition :: true_path :: false_path :: Nil)
    ("%s -> %s [label=\"condition\"]".format(this.id, condition.id) ::
      "%s -> %s [label=\"true\"]".format(this.id, true_path.id) ::
      "%s -> %s [label=\"false\"]".format(this.id, false_path.id) ::
      "%s [label=\"if expression: %s\"]".format(this.id, this.typ.toString) ::
      exprs.map(_.visualize)).mkString("\n")
  }
}

case class IdentifierExpression(token: Token, holder: ValueHolder)
    extends ValueExpression {
  def visualize =
    "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.typ,
                                     this.token.line,
                                     this.token.col)
  def typ:Type = if (holder != null) holder.typ else AnyValue
}

trait LiteralExpression[+T] extends ValueExpression {
  def token: Token
  def value: T
  def visualize =
    "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                     this.token.txt,
                                     this.typ,
                                     this.token.line,
                                     this.token.col)
}

case class BooleanLiteralExpression(token: Token, value: Boolean)
    extends LiteralExpression[Boolean] {
  def typ = HBoolean
}

abstract class NumberLiteralExpression[T] extends LiteralExpression[T]

case class FloatLiteralExpression(token: Token, value: Float)
    extends NumberLiteralExpression[Float] {
  def typ = HFloat
}

case class IntegerLiteralExpression(token: Token, value: Int)
    extends NumberLiteralExpression[Int] {
  def typ = HInteger
}

case class OperationCallExpression(token: Token,
                                   expr1: Expression,
                                   expr2: Expression)
    extends ValueExpression() {
  def findCommonType(expr1: Expression, expr2: Expression): Type = {
    @tailrec
    def loop(t: Type, acc: List[Type]): List[Type] = {
      if (t.parent == t) {
        t :: acc
      } else {
        loop(t.parent, t :: acc)
      }
    }
    loop(expr1.typ, Nil).intersect(loop(expr2.typ, Nil)).last
  }
  def typ = {
    token.txt match {
      case "==" | "!=" | ">=" | "<=" | ">" | "<" | "and" | "or" => HBoolean
      case _                                                    => findCommonType(expr1, expr2)
    }
  }

  def visualize = {
    val exprs = (expr1 :: expr2 :: Nil)
    val viz: List[String] = exprs.map(_.visualize)
    val conn: List[String] = exprs.map(a => s"${this.id} -> ${a.id}")
    val label: String = "%s [label=\"%s:%s %d,%d\"]".format(this.id,
                                                         this.token.txt,
                                                         this.typ,
                                                         this.token.line,
                                                         this.token.col)
    (label :: viz ::: conn).mkString("\n")
  }
}

case class FunctionCallExpression(declaration: FunctionDeclaration,
                                  parameters: Expression*)
    extends ValueExpression {
  def typ: Type = declaration.typ
  def visualize = {
    ("%s [label=\"%s %d,%d\"]".format(this.id,
                                      declaration.token.txt,
                                      declaration.token.line,
                                      declaration.token.col) ::
      List(parameters: _*).map(_.visualize) :::
      List(parameters: _*).map(a => s"${this.id} -> ${a.id}")).mkString("\n")
  }
}

abstract class PropertyCallExpression(source: Expression) extends Expression
case class FieldCallExpression(source: Expression, identiferExpr: IdentifierExpression) extends PropertyCallExpression(source) {
  def typ = identiferExpr.typ
  def visualize = source.visualize + "." + identiferExpr.visualize
}
case class MethodCallExpression(source: Expression, funcionCallExpr: FunctionCallExpression) extends PropertyCallExpression(source) {
  def typ = funcionCallExpr.typ
  def visualize = source.visualize + "." + funcionCallExpr.visualize
}

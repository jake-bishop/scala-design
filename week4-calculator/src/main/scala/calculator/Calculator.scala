package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    for {
      (field, signal) <- namedExpressions
    } yield field -> Signal(eval(signal(), namedExpressions))

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      // just return the contents. literally.
      case Literal(v) => v
      // evaluate the contents of the reference, but exclude the current field to avoid circular references.
      case Ref(name) => eval(getReferenceExpr(name, references), references - name)
      // evaluate each parameter (may be an external reference), then do the arithmetic it should do:
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references
      .get(name)
      // what does this fold do? if expression is empty, returns literal NaN. otherwise, returns the signal contents.
      .fold[Expr](Literal(Double.NaN))(exprSignal => exprSignal())
  }
}

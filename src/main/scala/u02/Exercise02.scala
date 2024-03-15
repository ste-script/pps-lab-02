package u02

object Exercise02 extends App:

  def positive(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _           => "negative"

  val empty: String => Boolean = _ match
    case "" => true
    case _  => false

  val neg: (String => Boolean) => (String => Boolean) = functionPredicate =>
    (inputString => !functionPredicate(inputString))

  val notEmpty: String => Boolean = neg(empty)

  def negGeneric[A](predicate: A => Boolean): A => Boolean = input =>
    !predicate(input)

  val intPositive: Int => Boolean = _ match
    case x if x >= 0 => true
    case _           => false

  val notPositive: Int => Boolean = negGeneric(intPositive)

  val notCurriedCheckMinEquivalence: (Int, Int, Int) => Boolean =
    (x: Int, y: Int, z: Int) => (x <= y) && y == z

  val curriedCheckMinEquivalence: Int => Int => Int => Boolean =
    x => y => z => (x <= y) && y == z

  def defNotCurriedCheckMinEquivalence(x: Int, y: Int, z: Int): Boolean =
    (x <= y) && y == z

  def defCurriedCheckMinEquivalence(x: Int)(y: Int)(z: Int): Boolean =
    (x <= y) && y == z

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

  // step 3c
  def negGeneric[A](predicate: A => Boolean): A => Boolean = input =>
    !predicate(input)

  val intPositive: Int => Boolean = _ match
    case x: Int if x >= 0 => true
    case _                => false

  val notPositive: Int => Boolean = negGeneric(intPositive)

  // step 4
  val notCurriedCheckMinEquivalence: (Int, Int, Int) => Boolean =
    (x: Int, y: Int, z: Int) => (x <= y) && y == z

  val curriedCheckMinEquivalence: Int => Int => Int => Boolean =
    x => y => z => (x <= y) && y == z

  def defNotCurriedCheckMinEquivalence(x: Int, y: Int, z: Int): Boolean =
    (x <= y) && y == z

  def defCurriedCheckMinEquivalence(x: Int)(y: Int)(z: Int): Boolean =
    (x <= y) && y == z

  // step 5
  def compose(f: Int => Int, g: Int => Int)(x: Int): Int =
    f(g(x))

  def genericCompose[A, B](f: A => A, g: B => A)(x: B): A =
    f(g(x))

  // step 6
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (a, b) if b == 0 => a
    case (a, b) if a > b  => gcd(b, a % b)

package u02

import scala.compiletime.ops.double
import scala.compiletime.ops.boolean

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

  enum Shape:
    private case Square(edge: Double)
    private case Rectangle(shortEdge: Double, longEdge: Double)
    private case Circle(radius: Double)

  object Shape:
    def square(edge: Double): Shape = Square(edge)
    def rectangle(shortEdge: Double, longEdge: Double) =
      Rectangle(shortEdge, longEdge)
    def circle(radius: Double) = Circle(radius)

    def perimeter(s: Shape): Double = s match
      case Square(edge)                   => edge * 4
      case Rectangle(shortEdge, longEdge) => shortEdge * 2 + longEdge * 2
      case Circle(radius)                 => radius * 2 * math.Pi
    def scale(s: Shape, a: Double): Shape = s match
      case Square(edge) => Square((edge) * a)
      case Rectangle(shortEdge, longEdge) =>
        Rectangle(shortEdge * a, longEdge * a)
      case Circle(radius) => Circle(radius * a)

  // step7
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:
    def map[A, B](o: Optional[A])(f: A => B): Optional[B] =
      o match
        case Maybe(value) => Maybe(f(value))
        case _            => Empty()
    def filter[A](o: Optional[A])(f: A => Boolean): Optional[A] =
      o match
        case Maybe(value) if f(value) => Maybe(value)
        case _                               => Empty()

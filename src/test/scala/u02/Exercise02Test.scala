package u02

import org.junit.*
import org.junit.Assert.*
import Exercise02.*

class Exercise02Test:

  // step 3a
  @Test def testPositive =
    val a = positive(0)
    assertEquals("positive", a)

  @Test def testNegative =
    val a = positive(-1)
    assertEquals("negative", a)

  @Test def testNotEmpty =
    assertTrue(notEmpty("a"))

  @Test def testEmpty =
    assertFalse(notEmpty(""))

  // test 3c
  @Test def testTrueGenericNeg =
    assertEquals(true, notPositive(-2))

  @Test def testFalseGenericNeg =
    assertEquals(false, notPositive(0))

  // test step 4
  @Test def testNotCurriedCheckMin =
    assertTrue(notCurriedCheckMinEquivalence(1, 2, 2))

  @Test def testFalseNotCurriedCheckMin =
    assertFalse(notCurriedCheckMinEquivalence(5, 2, 2))

  @Test def testCurriedCheckMin =
    assertTrue(curriedCheckMinEquivalence(1)(2)(2))

  @Test def testFalseCurriedCheckMin =
    assertFalse(curriedCheckMinEquivalence(1)(4)(2))

  @Test def testDefNotCurriedCheckMin =
    assertFalse(defNotCurriedCheckMinEquivalence(5, 2, 2))

  @Test def testFalseDefNotCurriedCheckMin =
    assertTrue(defNotCurriedCheckMinEquivalence(1, 3, 3))

  @Test def testDefCurriedCheckMin =
    val step = defCurriedCheckMinEquivalence(1)(2)
    assertTrue(step(2))

  // test step5
  @Test def testFunctionalComposition =
    val function = compose(_ - 1, _ * 2)(5)
    assertEquals(function, 9)
    assertNotEquals(function, 8)

  @Test def testGenericFunctionalComposition =
    val function = genericCompose[String, Int](_ + "B", x => (x * 2) + "A")(5)
    assertEquals(function, "10AB")
    assertNotEquals(function, 8)

  // test step 6
  @Test def testGcd =
    assertEquals(gcd(12, 8), 4)
    assertEquals(gcd(14, 7), 7)
    assertEquals(gcd(14, 1), 1)

  // test step7
  @Test def testSquare =
    import Shape.*
    val rect = square(5)
    val alphaScale = 2
    val per: Double = 20
    assertEquals(perimeter(rect), per, 0)
    assertEquals(perimeter(scale(rect, alphaScale)), per * alphaScale, 0)

  @Test def testRectangle =
    import Shape.*
    val rect = rectangle(5, 2)
    val alphaScale = 2
    val per: Double = 14
    assertEquals(perimeter(rect), per, 0)
    assertEquals(perimeter(scale(rect, alphaScale)), per * alphaScale, 0)

  @Test def testCircle =
    import Shape.*
    val rect = circle(5)
    val alphaScale = 2
    val per: Double = 31.41
    assertEquals(perimeter(rect), per, 0.1)
    assertEquals(perimeter(scale(rect, alphaScale)), per * alphaScale, 0.1)

  // test step 8
  @Test def testMap =
    import Optional.*
    assertEquals(Maybe(true), map(Maybe(5))(_ > 2))
    assertEquals(Empty(), map(Empty[Int]())(_ > 2))

  @Test def testFilter =
    import Optional.*
    assertEquals(Maybe(5), filter(Maybe(5))(_ > 2))
    assertEquals(Empty(), filter(Maybe(5))(_ > 8))
    assertEquals(Empty(), filter[Int](Empty())(_ > 2))

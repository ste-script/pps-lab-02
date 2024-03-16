package u02

import org.junit.*
import org.junit.Assert.*
import Exercise02.*
import u02.CaseMatch.f
import u02.Values.s

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
  @Test def testRectangle =
    import Shape.*
    val rect = rectangle(5)
    val alphaScale = 2
    val per: Double = 20
    assertEquals(perimeter(rect), per, 0)
    assertEquals(perimeter(scale(rect, alphaScale)), per * alphaScale, 0)

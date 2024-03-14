package u02

import org.junit.*
import org.junit.Assert.*
import Exercise02.*

class Exercise02Test:

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

  @Test def testTrueGenericNeg =
    assertEquals(true, notPositive(-2))

  @Test def testFalseGenericNeg =
    assertEquals(false, notPositive(0))

  @Test def testNotCurriedCheckMin =
    assertTrue(notCurriedCheckMinEquivalence(1, 2, 2))

  @Test def testFalseNotCurriedCheckMin =
    assertFalse(notCurriedCheckMinEquivalence(5, 2, 2))

  @Test def testCurriedCheckMin =
    assertTrue(curriedCheckMinEquivalence(1)(2)(2))

  @Test def testFalseCurriedCheckMin =
    assertFalse(curriedCheckMinEquivalence(1)(4)(2))

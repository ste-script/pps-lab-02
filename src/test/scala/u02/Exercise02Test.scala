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

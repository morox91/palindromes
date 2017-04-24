package example

import org.scalatest._

class SolutionSpec extends FunSuite {
  test("Empty input gives zero") {
    assert(Solution.solution("") == 0)
  }

  test("Single character gives zero") {
    assert(Solution.solution("1") == 0)
  }

  test("Input without palindromes gives zero") {
    assert(Solution.solution("472158964") == 0)
  }

  test("Correct result for odd palindromes") {
    assert(Solution.solution("3212343219") == 5)
  }

  test("Correct result for even palindromes") {
    assert(Solution.solution("21123321") == 5)
  }

  test("Correct result for both odd and even palindromes") {
    assert(Solution.solution("3412321232149") == 9)
  }

  test("Number of palindromes greater than 100,000,000 gives -1") {
    var input = "1"
    for (a <- 1 to 14) input = input + input
    assert(Solution.solution(input) == -1)
  }
}

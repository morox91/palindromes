package example

object Solution {

  object PalindromeType extends Enumeration {
    val Odd, Even = Value
  }

  def solution(s: String): Int = {
    val evenRadiuses = calculateRadiuses(s, PalindromeType.Even)
    val oddRadiuses = calculateRadiuses(s, PalindromeType.Odd)
    val palindromesCount = countResults(evenRadiuses) + countResults(oddRadiuses)
    if (palindromesCount > 100000000) -1 else palindromesCount
  }

  // for each character in input calculates radius of maximum palindrome around it
  // returns array of palindromes radiuses, array index corresponds to character index in input
  def calculateRadiuses(input: String, palindromeType: PalindromeType.Value): Array[Int] = {
    val inputLength = input.length()

    var i = 1 // index of character in input that is current palindrome center
    var radius = 0 // radius of current palindrome
    var R = new Array[Int](inputLength) // array of radiuses returned as a result

    def calculate() = {
      while (i < inputLength) {
        while (canExpandPalindrome())
          radius += 1
        
        R(i) = radius
        val (filledCount, preserveRadius) = fillKnownRadiuses()
        
        radius = if (preserveRadius) R(i) - (1 + filledCount) else 0
        i += 1 + filledCount
      }
    }

    def canExpandPalindrome() : Boolean = {
      var leftIndex = i - radius - 1
      var rightIndex = i + radius

      if (palindromeType == PalindromeType.Odd)
        rightIndex += 1

      if (leftIndex < 0 || rightIndex >= inputLength)
        false
      else 
        input.charAt(leftIndex) == input.charAt(rightIndex)
    }

    // fills next radiuses based on previous
    // returns number of radiuses that have been filled
    // and boolean that is true when found palindrome that potentially ends outside of current palindrome
    def fillKnownRadiuses() : (Int, Boolean) = {
      var k = 1
      // repeat while we are still under current palindrome (that of center in i)
      while (k < R(i)) {
        if (R(i - k) == R(i) - k) // palindrome of center in i - k ends exactly where current palindrome
          return (k - 1, true)
        if (R(i - k) < R(i) - k) // ends within the current
          R(i + k) = R(i - k)
        else // ends outside of current
          R(i + k) = R(i) - k
        k += 1
      }
      (k - 1, false)
    }

    calculate()
    R
  }

  def countResults(R: Array[Int]) : Int = {
    if (R.isEmpty) 
      0
    else
      R.reduceLeft((a:Int, b:Int) => a + b)
  }

  def main(args: Array[String]) {
    println(solution(""));
  }
}

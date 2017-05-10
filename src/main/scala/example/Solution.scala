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
        val knownValuesCount = fillKnownValues()

        radius = math.max(radius - knownValuesCount, 0)
        i += knownValuesCount
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

    // based on Manacher's algorithm
    // returns number of values that have been filled based on previous values
    def fillKnownValues() : Int = {
      var k = 1
      // repeat while we are still under current palindrome (that of center in i)
      // and palindrome of center in i - k doesn't end exactly where current palindrome
      while (k < R(i) && R(i - k) != R(i) - k) {
        if (R(i - k) < R(i) - k) // ends within the current
          R(i + k) = R(i - k)
        else // ends outside of current
          R(i + k) = R(i) - k
        k += 1
      }
      k
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

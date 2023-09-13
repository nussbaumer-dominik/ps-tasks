package at.ac.tuwien.a1.app.helper.operations

/**
 * Representation of a digit operation
 * */
data class DigitOperation(val digit: Int) : CalculatorOperation

/**
 * Checks whether the character is the representation of a digit operation
 * */
fun Char.isDigitOperation(): Boolean =
    isDigit()

/**
 * Converts the character into the representation of a digit operation
 * @throws IllegalStateException when the character is not a valid digit operation
 * */
fun Char.toDigitOperation(): DigitOperation =
    if (isDigitOperation()) DigitOperation(digitToInt()) else error("Operation for representation $this not found")

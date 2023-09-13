package at.ac.tuwien.a1.app.helper.operations

/**
 * Representation of a dot operation
 * */
data object DotOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a dot operation
 * */
fun Char.isDotOperation(): Boolean =
    this == '.'

/**
 * Converts the character into the representation of a dot operation
 * @throws IllegalStateException when the character is not a valid dot operation
 * */
fun Char.toDotOperation(): DotOperation =
    if (isDotOperation()) DotOperation else error("Operation for representation $this not found")

package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import at.ac.tuwien.a1.app.helper.data.stack.peekNthOrNull

/**
 * Representation of a copy operation
 * */
data object CopyOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a copy operation
 * */
fun Char.isCopyOperation(): Boolean =
    this == '!'

/**
 * Converts the character into the representation of a copy operation
 * @throws IllegalStateException when the character is not a valid copy operation
 * */
fun Char.toCopyOperation(): CopyOperation =
    if (isCopyOperation()) CopyOperation else error("Operation for representation $this not found")

fun CopyOperation.execute(dataStack: DataStack) {
    val value = dataStack.peekNext()

    if (value !is DataEntry.IntegerEntry) return

    val valueToCopy = dataStack.peekNthOrNull(value.value)
    dataStack.adjustTopValue { topValue -> valueToCopy ?: topValue }
}

package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of a integer inversion operation
 * */
data object IntegerConversionOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a integer inversion operation
 * */
fun Char.isIntegerInversionOperation(): Boolean =
    this == '?'

/**
 * Converts the character into the representation of a integer inversion operation
 * @throws IllegalStateException when the character is not a valid integer inversion operation
 * */
fun Char.toIntegerInversionOperation(): IntegerConversionOperation =
    if (isIntegerInversionOperation()) IntegerConversionOperation else error("Operation for representation $this not found")

fun IntegerConversionOperation.execute(dataStack: DataStack) {
    dataStack.adjustTopValue { topValue ->
        when (topValue) {
            is DataEntry.FloatEntry -> DataEntry.IntegerEntry(topValue.value.toInt())
            else -> DataEntry.StringEntry("")
        }
    }
}

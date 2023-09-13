package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of a null check operation
 * */
data object NullCheckOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a null check operation
 * */
fun Char.isNullCheckOperation(): Boolean =
    this == '_'

/**
 * Converts the character into the representation of a null check operation
 * @throws IllegalStateException when the character is not a valid null check operation
 * */
fun Char.toNullCheckOperation(): NullCheckOperation =
    if (isNullCheckOperation()) NullCheckOperation else error("Operation for representation $this not found")

fun NullCheckOperation.execute(dataStack: DataStack) {
    val value = dataStack.readNext()

    if (
        (value is DataEntry.StringEntry && value.value.isEmpty()) ||
        (value is DataEntry.IntegerEntry && value.value == 0) ||
        (value is DataEntry.FloatEntry && value.value > -DataEntry.FLOAT_EPSILON && value.value < DataEntry.FLOAT_EPSILON)
    ) {
        dataStack.push(DataEntry.IntegerEntry(1))
    } else {
        dataStack.push(DataEntry.IntegerEntry(0))
    }
}

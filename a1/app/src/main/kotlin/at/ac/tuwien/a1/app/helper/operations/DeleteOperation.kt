package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import at.ac.tuwien.a1.app.helper.data.stack.removeNthOrNull

/**
 * Representation of a delete operation
 * */
data object DeleteOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a delete operation
 * */
fun Char.isDeleteOperation(): Boolean =
    this == '$'

/**
 * Converts the character into the representation of a delete operation
 * @throws IllegalStateException when the character is not a valid delete operation
 * */
fun Char.toDeleteOperation(): DeleteOperation =
    if (isDeleteOperation()) DeleteOperation else error("Operation for representation $this not found")

fun DeleteOperation.execute(dataStack: DataStack) {
    val value = dataStack.readNext()

    if (value !is DataEntry.IntegerEntry) return
    dataStack.removeNthOrNull(value.value)
}

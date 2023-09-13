package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of a stack size operation
 * */
data object StackSizeOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a stack size operation
 * */
fun Char.isStackSizeOperation(): Boolean =
    this == '#'

/**
 * Converts the character into the representation of a stack size operation
 * @throws IllegalStateException when the character is not a valid stack size operation
 * */
fun Char.toStackSizeOperation(): StackSizeOperation =
    if (isStackSizeOperation()) StackSizeOperation else error("Operation for representation $this not found")

fun StackSizeOperation.execute(dataStack: DataStack) {
    dataStack.push(DataEntry.IntegerEntry(dataStack.size()))
}

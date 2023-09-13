package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of all supported logic operations
 * */
enum class LogicOperation(val representation: Char) : CalculatorOperation {
    AND('&'),
    OR('|'),
}

/**
 * Checks whether the character is the representation of a supported logic operation
 * */
fun Char.isLogicOperation(): Boolean =
    LogicOperation.entries.any { operation -> operation.representation == this }

/**
 * Converts the character into the representation of a logic operation
 * @throws IllegalStateException when the character is not a valid supported logic operation
 * */
fun Char.toLogicOperation(): LogicOperation =
    LogicOperation.entries.find { operation -> operation.representation == this }
        ?: error("Operation for representation $this not found")

fun LogicOperation.execute(dataStack: DataStack) {
    val valueTwo = dataStack.readNext()
    val valueOne = dataStack.readNext()

    if (valueOne !is DataEntry.IntegerEntry || valueTwo !is DataEntry.IntegerEntry) {
        dataStack.push(DataEntry.StringEntry(""))
        return
    }

    when (this) {
        LogicOperation.AND -> valueOne.value.toBoolean() && valueTwo.value.toBoolean()
        LogicOperation.OR -> valueOne.value.toBoolean() || valueTwo.value.toBoolean()
    }
}

private fun Int.toBoolean() =
    this == 1

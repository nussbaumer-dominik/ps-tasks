package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of all supported comparison operations
 * */
enum class ComparisonOperation(val representation: Char) : CalculatorOperation {
    EQUALS('='),
    LESS_THEN('<'),
    MORE_THEN('>'),
}

/**
 * Checks whether the character is the representation of a supported comparison operation
 * */
fun Char.isComparisonOperation(): Boolean =
    ComparisonOperation.entries.any { operation -> operation.representation == this }

/**
 * Converts the character into the representation of a comparison operation
 * @throws IllegalStateException when the string is not a valid supported comparison operation
 * */
fun Char.toComparisonOperation(): ComparisonOperation =
    ComparisonOperation.entries.find { operation -> operation.representation == this }
        ?: error("Operation for representation $this not found")

fun ComparisonOperation.execute(dataStack: DataStack) {
    val valueTwo = dataStack.readNext()
    val valueOne = dataStack.readNext()

    val result = when (this) {
        ComparisonOperation.EQUALS -> valueOne.compareTo(valueTwo) == 0
        ComparisonOperation.LESS_THEN -> valueOne < valueTwo
        ComparisonOperation.MORE_THEN -> valueOne > valueTwo
    }

    dataStack.push(DataEntry.IntegerEntry(if (result) 1 else 0))
}

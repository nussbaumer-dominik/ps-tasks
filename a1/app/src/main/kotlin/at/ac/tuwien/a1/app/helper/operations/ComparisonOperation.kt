package at.ac.tuwien.a1.app.helper.operations

/**
 * Representation of all supported comparison operations
 * */
enum class ComparisonOperation(val representation: Char) : CalculatorOperation {
    EQUALS('='),
    LESS_THEN('<'),
    MORE_THEN('>'),
}

/**
 * Checks whether the string is the representation of a supported comparison operation
 * */
fun Char.isComparisonOperation(): Boolean =
    ComparisonOperation.entries.any { operation -> operation.representation == this }

/**
 * Converts the string into the representation of a comparison operation
 * @throws IllegalStateException when the string is not a valid supported comparison operation
 * */
fun Char.toComparisonOperation(): ComparisonOperation =
    ComparisonOperation.entries.find { operation -> operation.representation == this }
        ?: error("Operation for representation $this not found")

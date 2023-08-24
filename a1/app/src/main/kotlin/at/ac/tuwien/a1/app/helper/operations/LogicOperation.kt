package at.ac.tuwien.a1.app.helper.operations

/**
 * Representation of all supported logic operations
 * */
enum class LogicOperation(val representation: String) : CalculatorOperation {
    AND("&"),
    OR("|"),
}

/**
 * Checks whether the string is the representation of a supported logic operation
 * */
fun String.isLogicOperation(): Boolean =
    LogicOperation.entries.any { operation -> operation.representation == this }

/**
 * Converts the string into the representation of a logic operation
 * @throws IllegalStateException when the string is not a valid supported logic operation
 * */
fun String.toLogicOperation(): LogicOperation =
    LogicOperation.entries.find { operation -> operation.representation == this }
        ?: error("Operation for representation $this not found")

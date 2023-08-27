package at.ac.tuwien.a1.app.helper.operations

/**
 * Representation of all supported arithmetic operations
 * */
enum class ArithmeticOperation(val representation: String) : CalculatorOperation {
    MULTIPLICATION("*"),
    ADDITION("+"),
    SUBTRACTION("-"),
    DIVISION("/"),
    MODULO("%"),
}

/**
 * Checks whether the string is the representation of a supported arithmetic operation
 * */
fun String.isArithmeticOperation(): Boolean =
    ArithmeticOperation.entries.any { operation -> operation.representation == this }

/**
 * Converts the string into the representation of a arithmetic operation
 * @throws IllegalStateException when the string is not a valid supported arithmetic operation
 * */
fun String.toArithmeticOperation(): ArithmeticOperation =
    ArithmeticOperation.entries.find { operation -> operation.representation == this }
        ?: error("Operation for representation $this not found")

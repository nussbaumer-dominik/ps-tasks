package at.ac.tuwien.a1.app.helper.operations


/**
 * Object representing a string operation
 * */
sealed interface StringOperation : CalculatorOperation {
    data object Open : StringOperation
    data object Close : StringOperation
}

/**
 * Representation of all supported string operations
 * */
enum class StringOperationRepresentation(val representation: Char) {
    OPEN('('),
    CLOSE(')'),
}

/**
 * Checks whether the character is the representation of a supported string operation
 * */
fun Char.isStringOperation(): Boolean =
    StringOperationRepresentation.entries.any { operation -> this == operation.representation }

/**
 * Converts the character into the representation of a string operation
 * @throws IllegalStateException when the string is not a valid supported string operation
 * */
fun Char.toStringOperation(): StringOperation {
    val operation = StringOperationRepresentation
        .entries
        .find { operation -> this == operation.representation }
        ?: error("Operation for representation $this not found")

    return when (operation) {
        StringOperationRepresentation.OPEN -> StringOperation.Open
        StringOperationRepresentation.CLOSE -> StringOperation.Close
    }
}

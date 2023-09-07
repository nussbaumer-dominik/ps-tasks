package at.ac.tuwien.a1.app.helper.operations


/**
 * Object representing a register operation
 * */
data class RegisterOperation(val target: String, val operation: RegisterOperationRepresentation) : CalculatorOperation

/**
 * Representation of all supported arithmetic operations
 * */
enum class RegisterOperationRepresentation(val representation: CharRange) {
    GET('a'..'z'),
    SAVE('A'..'Z'),
}

/**
 * Checks whether the string is the representation of a supported register operation
 * */
fun String.isRegisterOperation(): Boolean =
    RegisterOperationRepresentation.entries.any { operation -> (this.single() in operation.representation) }

/**
 * Converts the string into the representation of a register operation
 * @throws IllegalStateException when the string is not a valid supported register operation
 * */
fun String.toRegisterOperation(): RegisterOperation {
    val operation = RegisterOperationRepresentation
        .entries
        .find { operation -> (this.single() in operation.representation) }
        ?: error("Operation for representation $this not found")
    return RegisterOperation(this, operation)
}

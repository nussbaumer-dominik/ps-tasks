package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.register.Register
import at.ac.tuwien.a1.app.helper.data.register.require
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import at.ac.tuwien.a1.app.helper.operations.RegisterOperationRepresentation.GET
import at.ac.tuwien.a1.app.helper.operations.RegisterOperationRepresentation.SAVE


/**
 * Object representing a register operation
 * */
sealed interface RegisterOperation : CalculatorOperation {
    data class Get(val target: Char) : RegisterOperation
    data class Save(val target: Char) : RegisterOperation
}

/**
 * Representation of all supported arithmetic operations
 * */
enum class RegisterOperationRepresentation(val representation: CharRange) {
    GET('a'..'z'),
    SAVE('A'..'Z'),
}

/**
 * Checks whether the character is the representation of a supported register operation
 * */
fun Char.isRegisterOperation(): Boolean =
    RegisterOperationRepresentation.entries.any { operation -> this in operation.representation }

/**
 * Converts the character into the representation of a register operation
 * @throws IllegalStateException when the string is not a valid supported register operation
 * */
fun Char.toRegisterOperation(): RegisterOperation {
    val operation = RegisterOperationRepresentation
        .entries
        .find { operation -> this in operation.representation }
        ?: error("Operation for representation $this not found")

    return when (operation) {
        GET -> RegisterOperation.Get(this)
        SAVE -> RegisterOperation.Save(this)
    }
}

fun RegisterOperation.execute(register: Register, dataStack: DataStack) {
    when (this) {
        is RegisterOperation.Get -> {
            val valueToPush = register.require(target)
            dataStack.push(valueToPush)
        }

        is RegisterOperation.Save -> {
            val valueToSave = dataStack.readNext()
            register.put(target.lowercaseChar(), valueToSave)
        }
    }
}

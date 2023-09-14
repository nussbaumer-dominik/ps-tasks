package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.asCommand
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import at.ac.tuwien.a1.app.helper.data.stream.Stream

/**
 * Representation of an apply string operation
 * */
sealed interface ApplyStringOperation : CalculatorOperation {
    data object Immediately : ApplyStringOperation
    data object Later : ApplyStringOperation
}

/**
 * Representation of all supported apply string operations
 * */
enum class ApplyStringOperationRepresentation(val representation: Char) {
    IMMEDIATELY('@'),
    LATER('\\'),
}

/**
 * Checks whether the character is the representation of a supported apply string operation
 * */
fun Char.isApplyStringOperation(): Boolean =
    ApplyStringOperationRepresentation.entries.any { operation -> this == operation.representation }

/**
 * Converts the character into the representation of an apply string operation
 * @throws IllegalStateException when the character is not a valid supported apply string operation
 * */
fun Char.toApplyStringOperation(): ApplyStringOperation {
    val operation = ApplyStringOperationRepresentation
        .entries
        .find { operation -> this == operation.representation }
        ?: error("Operation for representation $this not found")

    return when (operation) {
        ApplyStringOperationRepresentation.IMMEDIATELY -> ApplyStringOperation.Immediately
        ApplyStringOperationRepresentation.LATER -> ApplyStringOperation.Later
    }
}

fun ApplyStringOperation.execute(dataStack: DataStack, stream: Stream) {
    val nextValue = dataStack.readNext()

    if (nextValue !is DataEntry.StringEntry) {
        dataStack.push(nextValue)
        return
    }

    val commands = when (this) {
        ApplyStringOperation.Immediately -> nextValue.value.reversed()
        ApplyStringOperation.Later -> nextValue.value
    }

    commands
        .map(Char::asCommand)
        .forEach { command ->
            when (this) {
                ApplyStringOperation.Immediately -> stream.insert(command)
                ApplyStringOperation.Later -> stream.append(command)
            }
        }
}

package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import kotlinx.coroutines.channels.SendChannel

/**
 * Representation of a write output operation
 * */
data object WriteOutputOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a write output operation
 * */
fun Char.isWriteOutputOperation(): Boolean =
    this == '"'

/**
 * Converts the character into the representation of a write output operation
 * @throws IllegalStateException when the character is not a valid write output operation
 * */
fun Char.toWriteOutputOperation(): WriteOutputOperation =
    if (isWriteOutputOperation()) WriteOutputOperation else error("Operation for representation $this not found")

suspend fun WriteOutputOperation.execute(dataStack: DataStack, outputStream: SendChannel<String>) {
    when (val value = dataStack.readNext()) {
        is DataEntry.FloatEntry -> outputStream.send(value.value.toString())
        is DataEntry.IntegerEntry -> outputStream.send(value.value.toString())
        is DataEntry.StringEntry -> outputStream.send(value.value)
    }
}

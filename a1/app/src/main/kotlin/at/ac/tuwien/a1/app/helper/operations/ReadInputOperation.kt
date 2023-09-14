package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import kotlinx.coroutines.channels.ReceiveChannel

/**
 * Representation of a read input operation
 * */
data object ReadInputOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a read input operation
 * */
fun Char.isReadInputOperation(): Boolean =
    this == '\''

/**
 * Converts the character into the representation of a read input operation
 * @throws IllegalStateException when the character is not a valid read input operation
 * */
fun Char.toReadInputOperation(): ReadInputOperation =
    if (isReadInputOperation()) ReadInputOperation else error("Operation for representation $this not found")

suspend fun ReadInputOperation.execute(dataStack: DataStack, inputStream: ReceiveChannel<String>) {
    println("Reading next available input!")
    val nextValue = inputStream.receive()

    nextValue.toIntOrNull()?.let { intValue ->
        dataStack.push(DataEntry.IntegerEntry(intValue))
        return
    }

    nextValue.toFloatOrNull()?.let { floatValue ->
        dataStack.push(DataEntry.FloatEntry(floatValue))
        return
    }

    if (
        nextValue.startsWith("(") &&
        nextValue.endsWith(")") &&
        nextValue.count { it == '(' } == nextValue.count { it == ')' }
    ) {
        dataStack.push(DataEntry.StringEntry(nextValue))
        return
    }

    dataStack.push(DataEntry.StringEntry(""))
}

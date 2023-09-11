package at.ac.tuwien.a1.app.helper.stream

/**
 * Base interface representing a stream holding commands to be executed by the calculator
 * */
interface Stream {
    /**
     * Appends the command to the command stream behind the other commands
     * */
    fun append(command: Command)

    /**
     * Inserts the command the start if the command stream to be executed next
     * */
    fun insert(command: Command)

    /**
     * Dispatched the commands in the correct order to be processed
     * */
    fun processCommands(callback: suspend (command: Command) -> Unit)
}

@JvmInline
value class Command(val value: Char)

fun Char.asCommand(): Command =
    Command(this)

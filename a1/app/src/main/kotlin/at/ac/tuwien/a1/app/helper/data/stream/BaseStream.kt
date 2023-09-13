package at.ac.tuwien.a1.app.helper.data.stream

import at.ac.tuwien.a1.app.helper.data.Command

/**
 * Base version of a stream storing the command string inside a list
 * */
class BaseStream : Stream {
    private val data: MutableList<Command> = mutableListOf()

    override fun append(command: Command) {
        data.add(command)
    }

    override fun insert(command: Command) {
        data.add(0, command)
    }

    override suspend fun processCommands(callback: suspend (command: Command) -> Unit) {
        while (data.isNotEmpty()) {
            callback(data.removeAt(0))
        }
    }

    override fun debug() {
        println("Stream contents: $data")
    }
}

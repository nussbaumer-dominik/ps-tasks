package at.ac.tuwien.a1.app.helper.stream

/**
 * Base version of a stream storing the command string inside a list
 * */
class BaseStream : Stream {
    override fun append(command: Command) {
        TODO("Not yet implemented")
    }

    override fun insert(command: Command) {
        TODO("Not yet implemented")
    }

    override fun processCommands(callback: suspend (command: Command) -> Unit) {
        TODO("Not yet implemented")
    }
}

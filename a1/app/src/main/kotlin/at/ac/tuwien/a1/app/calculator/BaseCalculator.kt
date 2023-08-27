package at.ac.tuwien.a1.app.calculator

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.mode.OperationMode
import at.ac.tuwien.a1.app.helper.register.BaseRegister
import at.ac.tuwien.a1.app.helper.register.Register
import at.ac.tuwien.a1.app.helper.stack.BaseDataStack
import at.ac.tuwien.a1.app.helper.stack.DataStack
import kotlinx.coroutines.channels.Channel

/**
 * Base version of a calculator using a stack for processing the input
 * */
class BaseCalculator(
    initialRegisterValues: Map<Char, DataEntry> = emptyMap(),
) : Calculator {
    /**
     * Stream of commands to be executed
     * */
    private val commandStream: Channel<Char> = Channel(capacity = Channel.UNLIMITED)

    /**
     * Current operation more of the calculator
     */
    private val mode: OperationMode = OperationMode.Executing

    /**
     * Stack data structure holding operation values
     * */
    private val dataStack: DataStack = BaseDataStack()

    /**
     * Register data structure holding data entries at register positions
     * */
    private val register: Register = BaseRegister(initialRegisterValues)

    /**
     * Stream of data input by the user read line by line
     * */
    private val inputStream: Channel<String> = Channel(capacity = Channel.UNLIMITED)

    /**
     * Stream of data output most probably printed to stdout
     * */
    private val outputStream: Channel<String> = Channel(capacity = Channel.UNLIMITED)
}

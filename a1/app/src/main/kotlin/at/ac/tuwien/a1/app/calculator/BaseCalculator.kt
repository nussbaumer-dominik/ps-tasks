package at.ac.tuwien.a1.app.calculator

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.toFloat
import at.ac.tuwien.a1.app.helper.mode.OperationMode
import at.ac.tuwien.a1.app.helper.operations.*
import at.ac.tuwien.a1.app.helper.register.BaseRegister
import at.ac.tuwien.a1.app.helper.register.Register
import at.ac.tuwien.a1.app.helper.register.getOfType
import at.ac.tuwien.a1.app.helper.register.require
import at.ac.tuwien.a1.app.helper.stack.BaseDataStack
import at.ac.tuwien.a1.app.helper.stack.DataStack
import at.ac.tuwien.a1.app.helper.stack.adjustTopTypedValue
import at.ac.tuwien.a1.app.helper.stream.BaseStream
import at.ac.tuwien.a1.app.helper.stream.Command
import at.ac.tuwien.a1.app.helper.stream.Stream
import at.ac.tuwien.a1.app.helper.stream.asCommand
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.consumeEach
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlin.math.pow

/**
 * Base version of a calculator using a stack for processing the input
 * */
class BaseCalculator(
    initialRegisterValues: Map<Char, DataEntry> = emptyMap(),
) : Calculator {
    /**
     * Stream of commands to be executed
     * */
    private val commandStream: Stream = BaseStream()

    /**
     * Current operation more of the calculator
     */
    private var mode: OperationMode = OperationMode.Executing

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

    override suspend fun start() = withContext(Dispatchers.Default) {
        // Initialize the command stream with contents of register a
        val initialCommandStream = register.getOfType<DataEntry.StringEntry>('a')
        initialCommandStream.value.forEach { command -> commandStream.append(command.asCommand()) }

        // In parallel consume console input and write it to the input stream
        launch { startConsumingInput() }

        // Start consuming the command stream
        commandStream.processCommands { command ->
            command.process()
        }

        delay(1000000)
    }

    override suspend fun consumeOutput(callback: (output: String) -> Unit) = withContext(Dispatchers.IO) {
        outputStream.consumeEach { output -> callback(output) }
    }

    private suspend fun Command.process() {
        println("Processing command \"$this\" in mode \"$mode\"")

        mode = when (val operationMode = mode) {
            is OperationMode.Executing -> operationMode.execute(this)
            is OperationMode.ReadingLeft -> operationMode.execute(this)
            is OperationMode.ReadingRight -> operationMode.execute(this)
            is OperationMode.ReadingString -> operationMode.execute(this)
        }
    }

    private suspend fun OperationMode.Executing.execute(command: Command): OperationMode {
        if (command.value.isDigit()) {
            dataStack.push(DataEntry.IntegerEntry(command.value.digitToInt()))
            return OperationMode.ReadingLeft
        }

        if (command.value == '.') {
            dataStack.push(DataEntry.FloatEntry(0f))
            return OperationMode.ReadingRight(-2)
        }

        if (command.value == '(') {
            dataStack.push(DataEntry.StringEntry(""))
            return OperationMode.ReadingString(1)
        }

        if (command.value.isRegisterOperation()) {
            command.value.toRegisterOperation().execute()
            return OperationMode.Executing
        }

        if (command.value.isComparisonOperation()) {
            command.value.toComparisonOperation().execute()
            return OperationMode.Executing
        }

        if (command.value == '#') {
            dataStack.push(DataEntry.IntegerEntry(dataStack.))
            return OperationMode.Executing
        }

        if (command.value == '$') {
            // TODO: Pop from the stack
            return OperationMode.Executing
        }

        if (command.value == '@') {
            if (dataStack.peekNext() !is DataEntry.StringEntry) return OperationMode.Executing

            val newCommands = dataStack.readNext() as DataEntry.StringEntry
            newCommands.value.forEach { newCommand -> commandStream.insert(newCommand.asCommand()) }

            return OperationMode.Executing
        }

        if (command.value == '\\') {
            if (dataStack.peekNext() !is DataEntry.StringEntry) return OperationMode.Executing

            val newCommands = dataStack.readNext() as DataEntry.StringEntry
            newCommands.value.forEach { newCommand -> commandStream.append(newCommand.asCommand()) }

            return OperationMode.Executing
        }

        if (command.value == '\'') {
            val inputLine = inputStream.receive()
            // TODO: Read in correct format
            dataStack.push(DataEntry.StringEntry(inputLine))
            return OperationMode.Executing
        }

        if (command.value == '"') {
            // TODO: Format the output correctly
            outputStream.send(dataStack.readNext().toString())
            return OperationMode.Executing
        }

        return OperationMode.Executing
    }

    private fun ComparisonOperation.execute() {
        val valueOne = dataStack.readNext()
        val valueTwo = dataStack.readNext()
        when (this) {
            ComparisonOperation.EQUALS -> TODO()
            ComparisonOperation.LESS_THEN -> TODO()
            ComparisonOperation.MORE_THEN -> TODO()
        }
    }

    private fun RegisterOperation.execute() {
        when (operation) {
            RegisterOperationRepresentation.GET -> {
                val valueToPush = register.require(target)
                dataStack.push(valueToPush)
            }

            RegisterOperationRepresentation.SAVE -> {
                val valueToSave = dataStack.readNext()
                register.put(target.lowercaseChar(), valueToSave)
            }
        }
    }

    private suspend fun OperationMode.ReadingLeft.execute(command: Command): OperationMode {
        if (command.value.isDigit()) {
            dataStack.adjustTopTypedValue<DataEntry.IntegerEntry> { entry ->
                entry.adjustDigitBy(command.value.digitToInt())
            }
            return OperationMode.ReadingLeft
        }

        if (command.value == '.') {
            dataStack.adjustTopTypedValue<DataEntry.IntegerEntry> { entry ->
                entry.toFloat()
            }
            return OperationMode.ReadingRight(-2)
        }

        return OperationMode.Executing.execute(command)
    }

    private suspend fun OperationMode.ReadingRight.execute(command: Command): OperationMode {
        if (command.value.isDigit()) {
            dataStack.adjustTopTypedValue<DataEntry.FloatEntry> { entry ->
                entry.adjustDigitBy(command.value.digitToInt(), value)
            }
            return OperationMode.ReadingRight(value - 1)
        }

        if (command.value == '.') {
            dataStack.push(DataEntry.FloatEntry(0f))
            return OperationMode.ReadingRight(-2)
        }

        return OperationMode.Executing.execute(command)
    }

    private fun OperationMode.ReadingString.execute(command: Command): OperationMode {
        if (command.value == '(') {
            dataStack.adjustTopTypedValue<DataEntry.StringEntry> { entry -> entry.copy(value = "$value(") }
            return OperationMode.ReadingString(value + 1)
        }

        if (command.value == ')') {
            if (value > 1) {
                dataStack.adjustTopTypedValue<DataEntry.StringEntry> { entry -> entry.copy(value = "$value)") }
            }
            return OperationMode.ReadingString(value - 1)
        }

        dataStack.adjustTopTypedValue<DataEntry.StringEntry> { entry -> entry.copy(value = "$value${command.value}") }
        return OperationMode.ReadingString(value)
    }

    private fun DataEntry.FloatEntry.adjustDigitBy(digit: Int, position: Int): DataEntry.FloatEntry =
        copy(value = value + digit * 10.0.pow(position.toDouble() + 1).toFloat())

    private fun DataEntry.IntegerEntry.adjustDigitBy(digit: Int): DataEntry.IntegerEntry =
        copy(value = value * 10 + digit)

    /**
     * Reads the console input and forwards it to the input stream until it is consumed from there
     * */
    private suspend fun startConsumingInput() = withContext(Dispatchers.IO) {
        while (true) {
            inputStream.send(readln())
        }
    }

    companion object {
        private const val FLOAT_EPSILON = 0.001f
    }
}

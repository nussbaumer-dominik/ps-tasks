package at.ac.tuwien.a1.app.calculator

import at.ac.tuwien.a1.app.helper.data.*
import at.ac.tuwien.a1.app.helper.data.register.BaseRegister
import at.ac.tuwien.a1.app.helper.data.register.Register
import at.ac.tuwien.a1.app.helper.data.register.require
import at.ac.tuwien.a1.app.helper.data.stack.BaseDataStack
import at.ac.tuwien.a1.app.helper.data.stack.DataStack
import at.ac.tuwien.a1.app.helper.data.stream.BaseStream
import at.ac.tuwien.a1.app.helper.data.stream.Stream
import at.ac.tuwien.a1.app.helper.mode.OperationMode
import at.ac.tuwien.a1.app.helper.operations.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.consumeEach
import kotlinx.coroutines.withContext
import kotlin.math.pow

typealias InputCallback = () -> String

/**
 * Base version of a calculator using a stack for processing the input
 * */
class BaseCalculator(
    /**
     * Values to initialise registers with
     * */
    initialRegisterValues: Map<Char, DataEntry> = emptyMap(),

    /**
     * Stack data structure holding operation values
     * */
    private val dataStack: DataStack = BaseDataStack(),

    /**
     * Register data structure holding data entries at register positions
     * */
    private val register: Register = BaseRegister(initialRegisterValues),
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
     * Stream of data input by the user read line by line
     * */
    private val inputStream: Channel<String> = Channel(capacity = Channel.UNLIMITED)


    /**
     * Stream of data output most probably printed to stdout
     * */
    private val outputStream: Channel<String> = Channel(capacity = Channel.UNLIMITED)

    override suspend fun start() = withContext(Dispatchers.Default) {
        // Initialize the command stream with contents of register a
        val initialCommandStream = register.require('a').asString()
        initialCommandStream.value
            .map(Char::asCommand)
            .forEach(commandStream::append)

        // Start consuming the command stream
        commandStream.processCommands { command ->
            command.process()
        }
    }

    override suspend fun consumeOutput(callback: (output: String) -> Unit) = withContext(Dispatchers.IO) {
        outputStream.consumeEach { output -> callback(output) }
    }

    override suspend fun sendInput(line: String) {
        inputStream.send(line)
    }

    override fun getCurrentMode(): OperationMode =
        mode

    private suspend fun Command.process() {
        mode = process(mode)
        dataStack.debug()
        register.debug()
        commandStream.debug()
        println("============================================")
    }

    private suspend fun Command.process(mode: OperationMode): OperationMode {
        println("Processing command \"$this\" in mode \"$mode\"")

        return when (val operationMode = mode) {
            is OperationMode.Executing -> operationMode.execute(this)
            is OperationMode.ReadingLeft -> operationMode.execute(this)
            is OperationMode.ReadingRight -> operationMode.execute(this)
            is OperationMode.ReadingString -> operationMode.execute(this)
        }
    }

    private suspend fun OperationMode.Executing.execute(command: Command): OperationMode =
        when (val operation = command.value.parseAsOperation()) {
            is DigitOperation -> {
                dataStack.push(DataEntry.IntegerEntry(operation.digit))
                OperationMode.ReadingLeft
            }

            is DotOperation -> {
                dataStack.push(DataEntry.FloatEntry(0f))
                OperationMode.ReadingRight(-2)
            }

            is StringOperation.Open -> {
                dataStack.push(DataEntry.StringEntry(""))
                OperationMode.ReadingString(1)
            }

            is RegisterOperation -> {
                operation.execute(register, dataStack)
                OperationMode.Executing
            }

            is ComparisonOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is ArithmeticOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is LogicOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is NullCheckOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is NegationOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is IntegerConversionOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is CopyOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is DeleteOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is ApplyStringOperation -> {
                operation.execute(dataStack, commandStream)
                OperationMode.Executing
            }

            is StackSizeOperation -> {
                operation.execute(dataStack)
                OperationMode.Executing
            }

            is ReadInputOperation -> {
                operation.execute(dataStack, inputStream)
                OperationMode.Executing
            }

            is WriteOutputOperation -> {
                operation.execute(dataStack, outputStream)
                OperationMode.Executing
            }

            else -> OperationMode.Executing
        }

    private suspend fun OperationMode.ReadingLeft.execute(command: Command): OperationMode =
        when (val operation = command.value.parseAsOperation()) {
            is DigitOperation -> {
                dataStack.adjustTopValue { entry -> entry.asInteger().adjustDigitBy(operation.digit) }
                OperationMode.ReadingLeft
            }

            is DotOperation -> {
                dataStack.adjustTopValue { entry -> entry.asInteger().toFloat() }
                OperationMode.ReadingRight(-2)
            }

            else -> command.process(OperationMode.Executing)
        }

    private suspend fun OperationMode.ReadingRight.execute(command: Command): OperationMode =
        when (val operation = command.value.parseAsOperation()) {
            is DigitOperation -> {
                dataStack.adjustTopValue { entry -> entry.asFloat().adjustDigitBy(operation.digit, value) }
                OperationMode.ReadingRight(value - 1)
            }

            is DotOperation -> {
                dataStack.push(DataEntry.FloatEntry(0f))
                OperationMode.ReadingRight(-2)
            }

            else -> command.process(OperationMode.Executing)
        }

    private fun OperationMode.ReadingString.execute(command: Command): OperationMode =
        when (command.value.parseAsOperation()) {
            is StringOperation.Open -> {
                dataStack.adjustTopValue { entry ->
                    val stringEntry = entry.asString()
                    stringEntry.copy(value = "${stringEntry.value}(")
                }
                OperationMode.ReadingString(value + 1)
            }

            is StringOperation.Close -> {
                if (value > 1) {
                    dataStack.adjustTopValue { entry ->
                        val stringEntry = entry.asString()
                        stringEntry.copy(value = "${stringEntry.value})")
                    }
                }

                if (value == 1) {
                    OperationMode.Executing
                } else {
                    OperationMode.ReadingString(value - 1)
                }
            }

            else -> {
                dataStack.adjustTopValue { entry ->
                    val stringEntry = entry.asString()
                    stringEntry.copy(value = "${stringEntry.value}${command.value}")
                }
                OperationMode.ReadingString(value)
            }
        }

    private fun DataEntry.FloatEntry.adjustDigitBy(digit: Int, position: Int): DataEntry.FloatEntry =
        copy(value = value + digit * 10.0.pow(position.toDouble() + 1).toFloat())

    private fun DataEntry.IntegerEntry.adjustDigitBy(digit: Int): DataEntry.IntegerEntry =
        copy(value = value * 10 + digit)
}

package at.ac.tuwien.a1.app

import at.ac.tuwien.a1.app.calculator.BaseCalculator
import at.ac.tuwien.a1.app.calculator.Calculator
import at.ac.tuwien.a1.app.helper.data.DataEntry
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlin.concurrent.thread

/**
 * Main entry point of the application
 * */
fun main() = runBlocking {
    // Setup and calculator with its initial register values
    val calculator: Calculator = BaseCalculator(
        initialRegisterValues = mapOf(
            'a' to DataEntry.StringEntry("y@x@"),
            // Program to ask for input and execute it repeatedly
            'x' to DataEntry.StringEntry("'@@\"x@"),
            // Output welcome message
            'y' to DataEntry.StringEntry("z\""),
            // Welcome message
            'z' to DataEntry.StringEntry("Welcome user!"),
        ),
    )

    // In parallel start listening on console input and forward it to the calculator
    thread(isDaemon = true) { startConsumingInput(calculator) }

    // In parallel start consuming the output stream and forwarding it to the console output
    val outputJob = launch { calculator.consumeOutput(::println) }

    // Startup the calculator and process the input
    calculator.start()

    // Clean up running coroutines
    outputJob.cancel()
}

/**
 * Reads the console input and forwards it to the input stream of the calculator
 * */
private fun startConsumingInput(calculator: Calculator) = runBlocking {
    while (true) {
        calculator.sendInput(readln())
    }
}

package at.ac.tuwien.a1.app

import at.ac.tuwien.a1.app.calculator.BaseCalculator
import at.ac.tuwien.a1.app.calculator.Calculator
import at.ac.tuwien.a1.app.helper.data.DataEntry
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

/**
 * Main entry point of the application
 * */
fun main() = runBlocking {
    // Setup and calculator with its initial register values
    val calculator: Calculator = BaseCalculator(
        mapOf(
            'a' to DataEntry.StringEntry("4 3(2*)@+"),
        ),
    )

    // In parallel start consuming the output stream and forwarding it to the console output
    launch { calculator.consumeOutput(::println) }

    // Startup the calculator and process the input
    calculator.start()
}

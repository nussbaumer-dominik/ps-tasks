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
    val c = "4!4\$_1+\$@"
    val a = "3!3!1-2!1=()5!($c)@2\$*"

    // Setup and calculator with its initial register values
    val calculator: Calculator = BaseCalculator(
        initialRegisterValues = mapOf(
            'a' to DataEntry.StringEntry("3($a)3!3\$3!@2\$\""),
        ),
        requestInputCallback = { readln() },
    )

    // In parallel start consuming the output stream and forwarding it to the console output
    val outputJob = launch { calculator.consumeOutput(::println) }

    // Startup the calculator and process the input
    calculator.start()

    // Clean up running coroutines
    outputJob.cancel()
}

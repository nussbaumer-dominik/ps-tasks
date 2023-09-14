package at.ac.tuwien.a1.app

import at.ac.tuwien.a1.app.calculator.BaseCalculator
import at.ac.tuwien.a1.app.calculator.Calculator
import at.ac.tuwien.a1.app.helper.data.DataEntry
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.test.runTest
import org.junit.jupiter.api.Test
import kotlin.concurrent.thread

class AppTest {
    @Test
    fun testInfiniteCalculator() = runTest {
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

        setupAndStart(calculator)
    }

    /**
     * Sets up input and output streams and starts processing
     * */
    private suspend fun setupAndStart(calculator: Calculator) = coroutineScope {
        // In parallel start listening on console input and forward it to the calculator
        setupInputStream(calculator)

        // In parallel start consuming the output stream and forwarding it to the console output
        val outputJob = launch { calculator.consumeOutput(::println) }

        // Startup the calculator and process the input
        calculator.start()

        // Clean up running coroutines
        outputJob.cancel()
    }

    /**
     * Starts processing STD_IN events in parallel
     * */
    private fun setupInputStream(calculator: Calculator) {
        thread(isDaemon = true) {
            runBlocking {
                while (true) {
                    calculator.sendInput(readln())
                }
            }
        }
    }
}

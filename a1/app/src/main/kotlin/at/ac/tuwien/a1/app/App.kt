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
            'f' to DataEntry.StringEntry("'@@ 0 0 " +" (#3-4!- (4!4!6+!+ 4!1+ 4$4$  4!4$2!@) (1$1$) (4!4\$_1+\$@)@) 2!@" // calculate sum
                    + " (Sum: )\"2!\" " // print sum
                    + " .+#1-/ " // calculate avg
                    + " (Avg: )\"2!\" " // print avg
                    + " 0 0 " +" (#4-4!- (4!4!7+!7!-2!*+ 4!1+ 4$4$ 4!4$2!@) (1$1$2$) (4!4\$_1+\$@)@) 2!@" // calculate sum of squared diffs
                    + " (Squared diff sum: )\"2!\" " // print squared diff sum
                    + "#1-.+/" //calculate variance
                    + " (Variance: )\"\" " // print variance
                ),
            'm' to  DataEntry.StringEntry(" 0 3! 0 0" // index value start offset
                    + "(#5-4!- " // loop over all elements
                    + "(3!7+!" // get next element
                    + "(6!> (3! 4!8+!)(6!6!) (4!4\$_1+$@)@)@  6!6!1+ 6$6$6$6$" // update biggest element, index & offset
                    + "6!6$2!@" // loop
                    + ") ("
                    + "6!7+! 7!7+$   7!7!7!7!7! 7$7$7$7$7$" // reorder new biggest value
                    + "(#6-5!- (4!1+ 5!1+8+! 6!1+ 2! 6$6$6$6$ 6!6$2!@)(1$1$1$1$1$) (4!4\$_1+$@)@)@ " // restart loop with start set one higher until we iterated over all elements
                    + ") (4!4\$_1+$@)@)  2!@" // now we have the input sorted
                    + "(#2% (#2/~#+!)(#2/~#+! #2/~#1++! +.+2/) (4!4\$_1+$@)@)@ \"" // calculate median of sorted list
            ),
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

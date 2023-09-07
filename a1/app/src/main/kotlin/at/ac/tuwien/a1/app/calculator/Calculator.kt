package at.ac.tuwien.a1.app.calculator

/**
 * A basic calculator that can execute simple arithmetics as well as very minimal logic operations
 * */
interface Calculator {
    /**
     * Start the calculator by initialising the command stream
     * */
    suspend fun start()

    /**
     * Output of the output stream
     * */
    suspend fun consumeOutput(callback: (output: String) -> Unit)
}

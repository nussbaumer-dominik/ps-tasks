package at.ac.tuwien.a1.app.helper.data.stack

import at.ac.tuwien.a1.app.helper.data.DataEntry

/**
 * Base interface representing the stack holding raw data of the calculator
 * */
interface DataStack {
    /**
     * Pushes the data value onto the stack
     * */
    fun push(entry: DataEntry)

    /**
     * Retrieves the next data value from the stack
     * @throws IllegalStateException when the stack is empty
     * */
    fun readNext(): DataEntry

    /**
     * Looks at the next data value from the stack without consuming it
     * @throws IllegalStateException when the stack is empty
     * */
    fun peekNext(): DataEntry

    /**
     * Looks at the nth data value from the stack without consuming it
     * @throws IllegalStateException when n is out of range
     * */
    fun peekNth(n: Int): DataEntry

    /**
     * Removes the nth data value from the stack
     * @throws IllegalStateException when n is out of range
     * */
    fun removeNth(n: Int): DataEntry

    /**
     * Retrieves the number of items currently on the stack
     * */
    fun size(): Int

    /**
     * Provides the top value of the stack and overrides it with the returned value
     * */
    fun adjustTopValue(callback: (value: DataEntry) -> DataEntry)

    /**
     * Output the contents of the registers for debug purpose
     * */
    fun debug()
}

fun DataStack.peekNthOrNull(n: Int): DataEntry? =
    runCatching { peekNth(n) }.getOrNull()

fun DataStack.removeNthOrNull(n: Int): DataEntry? =
    runCatching { removeNth(n) }.getOrNull()

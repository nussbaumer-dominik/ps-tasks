package at.ac.tuwien.a1.app.helper.stack

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
     * Retrieves the number of items currently on the stack
     * */
    fun size(): Int

    /**
     * Provides the top value of the stack and overrides it with the returned value
     * */
    fun adjustTopValue(callback: (value: DataEntry) -> DataEntry)
}

inline fun <reified T : DataEntry> DataStack.adjustTopTypedValue(crossinline callback: (value: T) -> DataEntry) {
    adjustTopValue { value ->
        if (value !is T) error("Top data stack entry is not of the correct type")
        callback(value)
    }
}

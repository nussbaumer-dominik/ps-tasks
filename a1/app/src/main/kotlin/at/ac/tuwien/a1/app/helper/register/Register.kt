package at.ac.tuwien.a1.app.helper.register

import at.ac.tuwien.a1.app.helper.data.DataEntry

/**
 * Base interface representing a register data structure with basic operations
 * */
interface Register {
    /**
     * Retrieves values from register position
     * @throws IllegalStateException when the position is not a valid register
     * */
    fun get(position: Char): DataEntry?

    /**
     * Tries to put new data into a register, overriding any previous data in the register
     * @throws IllegalStateException when the position is not a valid register
     * */
    fun put(position: Char, data: DataEntry)
}

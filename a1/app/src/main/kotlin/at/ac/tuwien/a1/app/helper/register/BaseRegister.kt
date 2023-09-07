package at.ac.tuwien.a1.app.helper.register

import at.ac.tuwien.a1.app.helper.data.DataEntry

/**
 * Base version of a register data structure storing data inside a map
 * */
class BaseRegister(
    initialValues: Map<Char, DataEntry> = emptyMap(),
) : Register {
    private val registerData: MutableMap<Char, DataEntry> = mutableMapOf()

    init {
        // Initialize the registers with the provided values
        initialValues.forEach(::put)
    }

    override fun get(position: Char): DataEntry? {
        checkPosition(position)
        return registerData[position.lowercaseChar()]
    }

    override fun put(position: Char, data: DataEntry) {
        checkPosition(position)
        registerData[position.lowercaseChar()] = data
    }

    private fun checkPosition(position: Char) {
        if (position.lowercaseChar() !in 'a'..'z') error("Invalid position $position")
    }
}

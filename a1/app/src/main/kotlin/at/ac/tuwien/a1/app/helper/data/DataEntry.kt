package at.ac.tuwien.a1.app.helper.data

sealed class DataEntry {
    data class IntegerEntry(val value: Int) : DataEntry()
    data class FloatEntry(val value: Float) : DataEntry()
    data class StringEntry(val value: String) : DataEntry()
}

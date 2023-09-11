package at.ac.tuwien.a1.app.helper.data

sealed class DataEntry : Comparable<DataEntry> {
    data class IntegerEntry(val value: Int) : DataEntry() {
        override fun compareTo(other: DataEntry): Int = when(other) {
            is FloatEntry -> TODO()
            is IntegerEntry -> TODO()
            is StringEntry -> TODO()
        }
    }

    data class FloatEntry(val value: Float) : DataEntry() {
        override fun compareTo(other: DataEntry): Int = when(other) {
            is FloatEntry -> TODO()
            is IntegerEntry -> TODO()
            is StringEntry -> TODO()
        }
    }

    data class StringEntry(val value: String) : DataEntry() {
        override fun compareTo(other: DataEntry): Int = when(other) {
            is FloatEntry -> TODO()
            is IntegerEntry -> TODO()
            is StringEntry -> TODO()
        }
    }
}

fun DataEntry.IntegerEntry.toFloat(): DataEntry.FloatEntry =
    DataEntry.FloatEntry(value = value.toFloat())

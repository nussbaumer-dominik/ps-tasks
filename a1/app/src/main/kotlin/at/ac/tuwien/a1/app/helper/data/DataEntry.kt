package at.ac.tuwien.a1.app.helper.data

import kotlin.math.abs
import kotlin.math.max

sealed class DataEntry : Comparable<DataEntry> {
    data class IntegerEntry(val value: Int) : DataEntry() {
        override fun compareTo(other: DataEntry): Int = when (other) {
            is FloatEntry -> toFloat().compareTo(other)
            is IntegerEntry -> value.compareTo(other.value)
            is StringEntry -> -1
        }
    }

    data class FloatEntry(val value: Float) : DataEntry() {
        override fun compareTo(other: DataEntry): Int = when (other) {
            is FloatEntry -> compareFloats(value, other.value)
            is IntegerEntry -> compareTo(other.toFloat())
            is StringEntry -> -1
        }
    }

    data class StringEntry(val value: String) : DataEntry() {
        override fun compareTo(other: DataEntry): Int = when (other) {
            is FloatEntry -> 1
            is IntegerEntry -> 1
            // TODO: Maybe we have to implement this on our own?
            is StringEntry -> value.compareTo(other.value)
        }
    }

    protected fun compareFloats(one: Float, two: Float): Int {
        if (floatsAreEqual(one, two)) return 0
        return if (one < two) -1 else 1
    }

    private fun floatsAreEqual(one: Float, two: Float): Boolean {
        val largerAbs = max(abs(one), abs(two))
        val dif = abs(one - two)
        return if (abs(one) < 1.0 && abs(two) < 1.0) {
            dif <= FLOAT_EPSILON
        } else {
            dif <= largerAbs * FLOAT_EPSILON
        }
    }

    companion object {
        const val FLOAT_EPSILON = 0.001f
    }
}

fun DataEntry.asInteger(): DataEntry.IntegerEntry =
    this as? DataEntry.IntegerEntry ?: error("Data entry is not of integer type")

fun DataEntry.asFloat(): DataEntry.FloatEntry =
    this as? DataEntry.FloatEntry ?: error("Data entry is not of float type")

fun DataEntry.asString(): DataEntry.StringEntry =
    this as? DataEntry.StringEntry ?: error("Data entry is not of string type")

fun DataEntry.IntegerEntry.toFloat(): DataEntry.FloatEntry =
    DataEntry.FloatEntry(value = value.toFloat())

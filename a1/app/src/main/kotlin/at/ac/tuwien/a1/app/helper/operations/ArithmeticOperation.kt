package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of all supported arithmetic operations
 * */
enum class ArithmeticOperation(val representation: Char) : CalculatorOperation {
    MULTIPLICATION('*'),
    ADDITION('+'),
    SUBTRACTION('-'),
    DIVISION('/'),
    MODULO('%'),
}

/**
 * Checks whether the character is the representation of a supported arithmetic operation
 * */
fun Char.isArithmeticOperation(): Boolean =
    ArithmeticOperation.entries.any { operation -> operation.representation == this }

/**
 * Converts the character into the representation of a arithmetic operation
 * @throws IllegalStateException when the character is not a valid supported arithmetic operation
 * */
fun Char.toArithmeticOperation(): ArithmeticOperation =
    ArithmeticOperation.entries.find { operation -> operation.representation == this }
        ?: error("Operation for representation $this not found")

fun ArithmeticOperation.execute(dataStack: DataStack) {
    val valueTwo = dataStack.readNext()
    val valueOne = dataStack.readNext()

    if (valueOne is DataEntry.StringEntry || valueTwo is DataEntry.StringEntry) {
        dataStack.push(DataEntry.StringEntry(""))
        return
    }

    if (valueOne is DataEntry.IntegerEntry && valueTwo is DataEntry.FloatEntry) {
        dataStack.push(execute(valueOne.value.toFloat(), valueTwo.value))
        return
    }

    if (valueOne is DataEntry.FloatEntry && valueTwo is DataEntry.IntegerEntry) {
        dataStack.push(execute(valueOne.value, valueTwo.value.toFloat()))
        return
    }

    if (valueOne is DataEntry.FloatEntry && valueTwo is DataEntry.FloatEntry) {
        dataStack.push(execute(valueOne.value, valueTwo.value))
        return
    }

    if (valueOne is DataEntry.IntegerEntry && valueTwo is DataEntry.IntegerEntry) {
        dataStack.push(execute(valueOne.value, valueTwo.value))
        return
    }
}

fun ArithmeticOperation.execute(one: Float, two: Float): DataEntry = when (this) {
    ArithmeticOperation.MULTIPLICATION -> DataEntry.FloatEntry(one * two)
    ArithmeticOperation.ADDITION -> DataEntry.FloatEntry(one + two)
    ArithmeticOperation.SUBTRACTION -> DataEntry.FloatEntry(one - two)
    ArithmeticOperation.DIVISION -> if (two == 0f) DataEntry.StringEntry("") else DataEntry.FloatEntry(one / two)
    ArithmeticOperation.MODULO -> DataEntry.StringEntry("")
}

fun ArithmeticOperation.execute(one: Int, two: Int): DataEntry = when (this) {
    ArithmeticOperation.MULTIPLICATION -> DataEntry.IntegerEntry(one * two)
    ArithmeticOperation.ADDITION -> DataEntry.IntegerEntry(one + two)
    ArithmeticOperation.SUBTRACTION -> DataEntry.IntegerEntry(one - two)
    ArithmeticOperation.DIVISION -> if (two == 0) DataEntry.StringEntry("") else DataEntry.IntegerEntry(one / two)
    ArithmeticOperation.MODULO -> DataEntry.IntegerEntry(one % two)
}

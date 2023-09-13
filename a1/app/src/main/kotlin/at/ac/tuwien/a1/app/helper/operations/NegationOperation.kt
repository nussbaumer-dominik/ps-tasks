package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.helper.data.DataEntry
import at.ac.tuwien.a1.app.helper.data.stack.DataStack

/**
 * Representation of a negation operation
 * */
data object NegationOperation : CalculatorOperation

/**
 * Checks whether the character is the representation of a negation operation
 * */
fun Char.isNegationOperation(): Boolean =
    this == '~'

/**
 * Converts the character into the representation of a negation operation
 * @throws IllegalStateException when the character is not a valid negation operation
 * */
fun Char.toNegationOperation(): NegationOperation =
    if (isNegationOperation()) NegationOperation else error("Operation for representation $this not found")

fun NegationOperation.execute(dataStack: DataStack) {
    dataStack.adjustTopValue { topValue ->
        when (topValue) {
            is DataEntry.FloatEntry -> DataEntry.FloatEntry(topValue.value * -1)
            is DataEntry.IntegerEntry -> DataEntry.IntegerEntry(topValue.value * -1)
            is DataEntry.StringEntry -> DataEntry.StringEntry("")
        }
    }
}

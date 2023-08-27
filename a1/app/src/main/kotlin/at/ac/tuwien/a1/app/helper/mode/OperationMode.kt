package at.ac.tuwien.a1.app.helper.mode

/**
 * Representation of all supported operation modes of the calculator
 * */
sealed class OperationMode(val representation: IntRange) {
    /**
     * The calculator is currently executing the command stream
     * */
    data object Executing : OperationMode(0..0)

    /**
     * The calculator is currently reading an integer value or digits left of the dot of a floating point number
     * */
    data object ReadingLeft : OperationMode(-1..-1)

    /**
     * The calculator is currently reading digits right of the dot of a floating point number
     * */
    data class ReadingRight(val value: Int) : OperationMode(Int.MIN_VALUE..-2)

    /**
     * The calculator is currently reading a string
     * */
    data class ReadingString(val value: Int) : OperationMode(1..Int.MAX_VALUE)
}

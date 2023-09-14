package at.ac.tuwien.a1.app.helper.operations

import at.ac.tuwien.a1.app.calculator.BaseCalculator

/**
 * Root interface representing an operation that can be executed in any of the calculator modes
 * */
sealed interface CalculatorOperation

/**
 * Helper function to parse an input command as a calculator operation
 * */
fun Char.parseAsOperation(debug: Boolean = false): CalculatorOperation? = when {
    isDigitOperation() -> toDigitOperation()
    isDotOperation() -> toDotOperation()
    isStringOperation() -> toStringOperation()
    isRegisterOperation() -> toRegisterOperation()
    isComparisonOperation() -> toComparisonOperation()
    isArithmeticOperation() -> toArithmeticOperation()
    isLogicOperation() -> toLogicOperation()
    isNullCheckOperation() -> toNullCheckOperation()
    isNegationOperation() -> toNegationOperation()
    isIntegerInversionOperation() -> toIntegerInversionOperation()
    isCopyOperation() -> toCopyOperation()
    isDeleteOperation() -> toDeleteOperation()
    isApplyStringOperation() -> toApplyStringOperation()
    isStackSizeOperation() -> toStackSizeOperation()
    isReadInputOperation() -> toReadInputOperation()
    isWriteOutputOperation() -> toWriteOutputOperation()
    else -> {
        if (BaseCalculator.DEBUG) {
            println("Encountered unknown operation $this")
        }
        null
    }
}

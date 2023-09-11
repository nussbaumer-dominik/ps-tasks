package at.ac.tuwien.a1.app.helper.stack

import at.ac.tuwien.a1.app.helper.data.DataEntry
import java.util.*

/**
 * Base version of a data stack storing data inside a java.util.Stack
 * */
class BaseDataStack : DataStack {
    private val data: Stack<DataEntry> = Stack()

    override fun push(entry: DataEntry) {
        data.push(entry)
    }

    override fun readNext(): DataEntry =
        data.pop()

    override fun peekNext(): DataEntry =
        data.peek()

    override fun size(): Int =
        data.size

    override fun adjustTopValue(callback: (value: DataEntry) -> DataEntry) {
        data.push(callback(data.pop()))
    }
}

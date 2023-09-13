package at.ac.tuwien.a1.app.helper.data.stack

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

    override fun peekNth(n: Int): DataEntry =
        runCatching { data[data.size - n] }.getOrElse { error("Could not find entry at $n") }

    override fun removeNth(n: Int): DataEntry =
        runCatching { data.removeAt(data.size - n) }.getOrElse { error("Could not find entry at $n") }

    override fun size(): Int =
        data.size

    override fun adjustTopValue(callback: (value: DataEntry) -> DataEntry) {
        data.push(callback(data.pop()))
    }

    override fun debug() {
        println("Stack contents: $data")
    }
}

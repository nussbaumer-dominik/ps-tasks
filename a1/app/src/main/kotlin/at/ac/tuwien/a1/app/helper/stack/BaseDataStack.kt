package at.ac.tuwien.a1.app.helper.stack

import at.ac.tuwien.a1.app.helper.data.DataEntry
import java.util.*

class BaseDataStack : DataStack {
    private val data: Stack<DataEntry> = Stack()

    override fun push(entry: DataEntry) {
        data.push(entry)
    }

    override fun readNext(): DataEntry {
        return data.pop()
    }
}

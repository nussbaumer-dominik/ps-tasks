package at.ac.tuwien.a1.app.helper.stack

import at.ac.tuwien.a1.app.helper.data.DataEntry

interface DataStack {
    fun push(entry: DataEntry)
    fun readNext(): DataEntry
}

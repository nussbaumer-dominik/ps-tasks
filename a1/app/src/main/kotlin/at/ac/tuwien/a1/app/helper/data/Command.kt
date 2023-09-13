package at.ac.tuwien.a1.app.helper.data

@JvmInline
value class Command(val value: Char)

fun Char.asCommand(): Command =
    Command(this)

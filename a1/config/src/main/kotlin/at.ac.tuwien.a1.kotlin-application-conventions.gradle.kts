import helper.versionCatalog

plugins {
    id("at.ac.tuwien.a1.kotlin-common-conventions")
    application
}

dependencies {
    implementation(versionCatalog().findLibrary("kotlin.coroutines").get())
}

plugins {
    id("at.ac.tuwien.a1.kotlin-application-conventions")
}

dependencies {
    // Define dependencies here
    testImplementation(libs.kotlin.coroutines.test)
}

application {
    mainClass.set("at.ac.tuwien.a1.app.AppKt")
}

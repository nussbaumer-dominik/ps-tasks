@file:Suppress("UnstableApiUsage")

import helper.versionCatalog

plugins {
    id("org.jetbrains.kotlin.jvm")
}

repositories {
    mavenCentral()
}

testing.suites.withType<JvmTestSuite> {
    useJUnitJupiter(versionCatalog().findVersion("junit.jupiter").get().requiredVersion)
}

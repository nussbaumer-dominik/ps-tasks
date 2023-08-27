package helper

import org.gradle.api.Project
import org.gradle.api.artifacts.VersionCatalog
import org.gradle.api.artifacts.VersionCatalogsExtension
import org.gradle.kotlin.dsl.getByType

fun Project.versionCatalog(name: String = "libs"): VersionCatalog =
    extensions.getByType<VersionCatalogsExtension>().named(name)

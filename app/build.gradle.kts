/* ────── explicit imports needed for Kotlin-DSL ────── */
import com.android.build.api.dsl.ApplicationExtension
import org.gradle.api.JavaVersion
import org.gradle.jvm.toolchain.JavaLanguageVersion
import org.jetbrains.kotlin.gradle.dsl.KotlinJvmOptions

plugins {
    alias(libs.plugins.jetbrains.kotlin.android)
    id("com.android.application")
    id("com.google.gms.google-services")
}

android {
    namespace       = "com.example.realizedvision"
    compileSdk      = 34

    defaultConfig {
        applicationId         = "com.csulb.realized_vision"
        minSdk                = 23
        targetSdk             = 34
        versionCode           = 1
        versionName           = "1.0"
        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"

        vectorDrawables { useSupportLibrary = true }
    }

    buildTypes {
        release {
            isMinifyEnabled      = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }

    compileOptions {
        // Source-/target-compat for Java sources *inside* the module.
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }

    kotlinOptions {
        // Cast to access JVM-specific options cleanly
        (this as KotlinJvmOptions).jvmTarget = "17"
    }

    // Android-Gradle 8+ already builds with JDK 17, but the
    // tool-chain block is nice for IDE consistency:
    compileSdkPreview = null
    packaging {
        resources.excludes += "/META-INF/{AL2.0,LGPL2.1}"
    }

    buildFeatures { compose = true }

    composeOptions {
        kotlinCompilerExtensionVersion = "1.5.1"
    }
}

/* ─────────── dependencies ─────────── */
dependencies {

    /* ---------- Android core / UI ---------- */
    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.appcompat)
    implementation(libs.androidx.constraintlayout)
    implementation(libs.androidx.recyclerview)
    implementation(libs.androidx.lifecycle.runtime.ktx)
    implementation(libs.material)                 // Material Components
    implementation(libs.androidx.material3)

    /* ---------- Jetpack Compose ---------- */
    implementation(platform(libs.androidx.compose.bom))
    implementation(libs.androidx.ui)              // foundation
    implementation(libs.androidx.ui.graphics)
    implementation(libs.androidx.ui.tooling.preview)
    debugImplementation(libs.androidx.ui.tooling)
    debugImplementation(libs.androidx.ui.test.manifest)

    /* ---------- Firebase ---------- */
    implementation(platform(libs.firebase.bom))
    implementation(libs.firebase.analytics)
    implementation(libs.firebase.auth)
    implementation(libs.firebase.firestore)
    implementation(libs.firebase.database)

    /* ---------- Google Maps / Location / Volley ---------- */
    implementation("com.google.android.gms:play-services-maps:19.2.0")
    implementation("com.google.android.gms:play-services-location:21.3.0")
    implementation("com.android.volley:volley:1.2.1")
    implementation(libs.android.maps.utils)

    /* ---------- Glide (image loading) ---------- */
    implementation(libs.glide)
//    kapt("com.github.bumptech.glide:compiler:4.16.0")

    /* ---------- Coroutines ---------- */
    implementation(libs.kotlinx.coroutines.android)

    /* ---------- Retrofit / OkHttp / Gson ---------- */
    implementation(libs.retrofit)
    implementation("com.squareup.retrofit2:converter-gson:2.9.0")
    implementation(libs.logging.interceptor)

    /* ---------- Stripe Android SDK ---------- */
    implementation("com.stripe:stripe-android:21.12.0")

    /* ---------- Kizitonwose CalendarView ---------- */
    implementation("com.kizitonwose.calendar:view:2.6.2")

    /* ---------- Testing ---------- */
    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.junit)
    androidTestImplementation(libs.androidx.espresso.core)
    androidTestImplementation(platform(libs.androidx.compose.bom))
    androidTestImplementation(libs.androidx.ui.test.junit4)
}

/* ───────── Java tool-chain for kapt / annotation processors ───────── */
java {
    toolchain.languageVersion.set(JavaLanguageVersion.of(17))
}

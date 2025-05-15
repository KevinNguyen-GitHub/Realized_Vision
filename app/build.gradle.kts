plugins {
    alias(libs.plugins.jetbrains.kotlin.android)
    id("com.android.application")
    id("com.google.gms.google-services")
    alias(libs.plugins.kotlin.compose)
}

android {
    namespace = "com.example.realizedvision"
    compileSdk = 35

    defaultConfig {
        applicationId = "com.csulb.realized_vision"
        minSdk = 24
        targetSdk = 34
        versionCode = 1
        versionName = "1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
        vectorDrawables {
            useSupportLibrary = true
        }

        // Explicitly enable BuildConfig generation
        buildConfigField("Boolean", "DEBUG", "false")

        // SMTP Credentials
        buildConfigField(
            "String",
            "SMTP_USER",
            "\"${project.findProperty("SMTP_USER")?.toString()?.replace("'", "") ?: ""}\""
        )
        buildConfigField(
            "String",
            "SMTP_PASSWORD",
            "\"${project.findProperty("SMTP_PASSWORD")?.toString()?.replace("\"", "\\\"")?: ""}\""
        )
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }

    compileOptions {
        isCoreLibraryDesugaringEnabled = true
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }

    kotlinOptions {
        jvmTarget = "1.8" //used to be "1.8"
    }

    buildFeatures {
        compose = true
        buildConfig = true
    }

    composeOptions {
        kotlinCompilerExtensionVersion = "1.5.1"
    }

    packaging {
        resources {
            excludes += "/META-INF/{AL2.0,LGPL2.1}"
            excludes += setOf(
                "META-INF/NOTICE.md",
                "META-INF/LICENSE.md",
                "META-INF/INDEX.LIST",
                "META-INF/DEPENDENCIES",
                "META-INF/LICENSE",
                "META-INF/NOTICE",
                "META-INF/ASL2.0",
                "META-INF/AL2.0",
                "META-INF/LGPL2.1"
            )
        }
    }
}
dependencies {
    // Core Android dependencies
    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.lifecycle.runtime.ktx)
    implementation(libs.androidx.activity.compose)
    implementation(libs.androidx.appcompat)
    implementation(libs.androidx.constraintlayout)
    implementation(libs.androidx.material3)

    // Jetpack Compose dependencies
    implementation(platform(libs.androidx.compose.bom))
    implementation(libs.androidx.ui)
    implementation(libs.androidx.ui.graphics)
    implementation(libs.androidx.ui.tooling.preview)
    implementation(libs.material)
    implementation(libs.androidx.recyclerview)
    implementation(libs.androidx.legacy.support.v4)
    implementation(libs.volley)
    implementation(libs.play.services.maps)
    debugImplementation(libs.androidx.ui.tooling)
    debugImplementation(libs.androidx.ui.test.manifest)

    // Firebase dependencies
    implementation(platform(libs.firebase.bom))
    implementation(libs.firebase.analytics)
    implementation(libs.firebase.auth)
    implementation(libs.firebase.firestore)
    implementation(libs.firebase.database)

    implementation(libs.firebase.config)
    implementation(libs.firebase.auth.v2300)
    implementation(libs.play.services.base)
    implementation(libs.play.services.basement)
    implementation(libs.android.maps.utils.v3)

    // Custom Calendar dependencies
    implementation(libs.view)
    implementation(libs.compose)
    coreLibraryDesugaring(libs.desugar.jdk.libs)

    //stripe + retrofit + OkHttp dependencies
    implementation(libs.stripe.android)
    implementation(libs.retrofit)
    implementation(libs.converter.gson)
    implementation(libs.okhttp)
    implementation(libs.logging.interceptor)
    
    // Testing dependencies
    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.junit)
    androidTestImplementation(libs.androidx.espresso.core)
    androidTestImplementation(platform(libs.androidx.compose.bom))
    androidTestImplementation(libs.androidx.ui.test.junit4)

    //Glide dependencies
    implementation(libs.glide)
    annotationProcessor(libs.glide.compiler)

    //Java mail dependencies
    // https://mvnrepository.com/artifact/javax.mail/mail
    implementation("com.sun.mail:android-mail:1.6.7") {
        exclude(group = "com.sun.activation", module = "javax.activation")
    }
    implementation("com.sun.mail:android-activation:1.6.7") {
        exclude(group = "com.sun.activation", module = "javax.activation")
    }
}
java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(17)
    }
}

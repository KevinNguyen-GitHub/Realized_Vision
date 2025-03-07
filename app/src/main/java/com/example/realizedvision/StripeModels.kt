package com.example.realizedvision

data class CreatePaymentIntentRequest(
    val amount: Long,
    val currency: String,
    val automatic_payment_methods: AutomaticPaymentMethods
)

data class AutomaticPaymentMethods(
    val enabled: Boolean = true
)

data class CreatePaymentIntentResponse(
    val client_secret: String
)
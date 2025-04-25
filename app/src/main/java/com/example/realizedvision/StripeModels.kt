package com.example.realizedvision

data class CreatePaymentIntentRequest(
    val amount: Long,
    val currency: String,
    val automatic_payment_methods: AutomaticPaymentMethods
)

data class AutomaticPaymentMethods(
    val enabled: Boolean = true
)

data class StripeConfigResponse(
    val stripePublishableKey: String
)
data class CreatePaymentIntentResponse(
    val client_secret: String
)

data class CreateConnectAccountRequest(
    val email: String,
    val userId: String
)

data class CreateConnectAccountResponse(
    val accountId: String
)

data class GenerateAccountLinkRequest(
    val accountId: String,
    val refreshUrl: String,
    val returnUrl: String
)

data class GenerateAccountLinkResponse(
    val url: String
)

data class GenerateDashboardLinkRequest(
    val accountId: String,
    val returnUrl: String
)

data class GenerateDashboardLinkResponse(
    val url: String
)

data class AccountDetailsResponse(
    val id: String,
    val charges_enabled: Boolean,
    val payout_enabled: Boolean,
    val transfers_enabled: Boolean,
    val requirements: Requirements?
)

data class Requirements(
    val currently_due: List<String>?,
    val eventually_due: List<String>?
)
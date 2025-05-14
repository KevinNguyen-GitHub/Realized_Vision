package com.example.realizedvision.stripe

/** -----------------------  Requests  ----------------------- */
data class CreatePaymentIntentRequest(
    val amount : Long,                    // amount in the smallest currency unit (e.g. cents)
    val currency: String,                 // ISO-4217, e.g. “usd”
    val automatic_payment_methods: AutomaticPaymentMethods = AutomaticPaymentMethods()
)

data class AutomaticPaymentMethods(val enabled: Boolean = true)

data class CreateConnectAccountRequest(
    val email : String,
    val userId: String                    // internal UID, not the Stripe ID
)

data class GenerateAccountLinkRequest(
    val accountId: String,
    val refreshUrl: String,
    val returnUrl : String
)

/** -----------------------  Responses  ---------------------- */
data class StripeConfigResponse(
    val stripePublishableKey: String
)

data class CreatePaymentIntentResponse(
    val client_secret: String
)

data class CreateConnectAccountResponse(
    val accountId: String
)

data class GenerateAccountLinkResponse(
    val url: String
)

data class AccountDetailsResponse(
    val id               : String,
    val charges_enabled  : Boolean,
    val payout_enabled   : Boolean,
    val transfers_enabled: Boolean,
    val requirements     : Requirements?
)

data class Requirements(
    val currently_due : List<String>? = null,
    val eventually_due: List<String>? = null
)

package com.example.realizedvision

import com.example.realizedvision.stripe.AccountDetailsResponse
import com.example.realizedvision.stripe.CreateConnectAccountRequest
import com.example.realizedvision.stripe.CreateConnectAccountResponse
import com.example.realizedvision.stripe.CreatePaymentIntentRequest
import com.example.realizedvision.stripe.CreatePaymentIntentResponse
import com.example.realizedvision.stripe.GenerateAccountLinkRequest
import com.example.realizedvision.stripe.GenerateAccountLinkResponse
import com.example.realizedvision.stripe.StripeConfigResponse
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.GET
import retrofit2.http.POST
import retrofit2.http.Path

interface StripeApiService {
    @POST("/create-payment-intent")
    suspend fun createPaymentIntent(@Body request: CreatePaymentIntentRequest): Response<CreatePaymentIntentResponse>

    @GET("/config")
    suspend fun getStripeConfig(): Response<StripeConfigResponse>

    @POST("/create-connect-account")
    suspend fun createConnectAccount(@Body request: CreateConnectAccountRequest): Response<CreateConnectAccountResponse>

    @POST("/generate-account-link")
    suspend fun generateAccountLink(@Body request: GenerateAccountLinkRequest): Response<GenerateAccountLinkResponse>

    @GET("/connect-account/{accountId}")
    suspend fun getAccountDetails(@Path("accountId") accountId: String): Response<AccountDetailsResponse>
}
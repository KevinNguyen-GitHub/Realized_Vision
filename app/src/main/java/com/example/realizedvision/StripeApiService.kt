package com.example.realizedvision

import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.POST

interface StripeApiService {
    @POST("create-payment-intent") // New endpoint on your Node.js server
    suspend fun createPaymentIntent(@Body request: CreatePaymentIntentRequest): Response<CreatePaymentIntentResponse>


}
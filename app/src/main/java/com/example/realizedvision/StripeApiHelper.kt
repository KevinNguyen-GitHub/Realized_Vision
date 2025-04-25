package com.example.realizedvision

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import retrofit2.Response

class StripeApiHelper(private val stripeApiService: StripeApiService) {

    interface Callback<T> {
        fun onSuccess(response: Response<T>)
        fun onError(throwable: Throwable)
    }

    fun generateDashboardLink(
        accountId: String,
        refreshUrl: String,
        returnUrl: String,
        callback: Callback<GenerateDashboardLinkResponse>
    ) {
        CoroutineScope(Dispatchers.IO).launch {
            try {
                val request = GenerateDashboardLinkRequest(
                    accountId = accountId,
                    refreshUrl = refreshUrl,
                    returnUrl = returnUrl,
                    failedToGenerateUrl = returnUrl
                )
                val response = stripeApiService.generateDashboardLink(request)
                withContext(Dispatchers.Main) {
                    callback.onSuccess(response)
                }
            } catch (t: Throwable) {
                withContext(Dispatchers.Main) {
                    callback.onError(t)
                }
            }
        }
    }

    fun createConnectAccount(
        email: String,
        userId: String,
        callback: Callback<CreateConnectAccountResponse>
    ) {
        CoroutineScope(Dispatchers.IO).launch {
            try {
                val request = CreateConnectAccountRequest(email, userId)
                val response = stripeApiService.createConnectAccount(request)
                withContext(Dispatchers.Main) {
                    callback.onSuccess(response)
                }
            } catch (t: Throwable) {
                withContext(Dispatchers.Main) {
                    callback.onError(t)
                }
            }
        }
    }

    fun generateAccountLink(
        accountId: String,
        refreshUrl: String,
        returnUrl: String,
        callback: Callback<GenerateAccountLinkResponse>
    ){
        CoroutineScope(Dispatchers.IO).launch {
            try {
                val request = GenerateAccountLinkRequest(accountId, refreshUrl, returnUrl)
                val response = stripeApiService.generateAccountLink(request)
                withContext(Dispatchers.Main) {
                    callback.onSuccess(response)
                }
            } catch (t: Throwable) {
                withContext(Dispatchers.Main) {
                    callback.onError(t)
                }
            }
        }
    }
}
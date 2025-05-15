package com.example.realizedvision

import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import android.widget.Button
import android.widget.ImageButton
import android.widget.TextView
import android.widget.Toast
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import androidx.lifecycle.lifecycleScope
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FirebaseFirestore
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.tasks.await
import kotlinx.coroutines.withContext
import java.util.UUID

class VendorOnboardActivity : AppCompatActivity() {

    private lateinit var onboardButton: Button
    private lateinit var accountStatusTextView: TextView
    private lateinit var accountRequirementsTextView: TextView
    private lateinit var backButton: ImageButton

    private var accountStatus: String = "Not Onboarded"
    private var accountRequirements: String = "Please note that you are responsible for any refunds, disputes, or fees."
    private var accountId: String? = null
    private val auth = FirebaseAuth.getInstance()
    private val db = FirebaseFirestore.getInstance()
    private val stripeApiService = NetworkModule.provideStripeApiService()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_vendor_onboard)

        accountStatusTextView = findViewById(R.id.onboardStatusTextView)
        accountRequirementsTextView = findViewById(R.id.onboardRequirementsTextView)
        onboardButton = findViewById(R.id.vendorOnboardButton)
        backButton  = findViewById(R.id.backButtonOnboarding)

        updateStatus(accountStatus)
        updateRequirements(accountRequirements)
        checkForExistingAccount()
        backButton.setOnClickListener{
            finish()
        }
        onboardButton.setOnClickListener {
            handleOnboardButtonClick()
        }
        handleIntent(intent)
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        handleIntent(intent)
    }

    private fun handleOnboardButtonClick() {
        when(accountStatus) {
            "Not Onboarded" -> {
                createConnectAccount()
            }
            "Loading..." -> {
                Toast.makeText(this, "Please wait...", Toast.LENGTH_SHORT).show()
            }
            "Action Needed", "Account Created" -> {
                generateOnboardingLink()
            }
            "Account Onboarded" -> {
                Toast.makeText(this, "Account already onboarded", Toast.LENGTH_SHORT).show()
            }
            else -> {
                Toast.makeText(this, "Error: $accountStatus", Toast.LENGTH_SHORT).show()
            }
        }
    }

    private fun createConnectAccount() {
        lifecycleScope.launch {
            updateStatus("Loading...")
            val newAccountId = createVendorAccount()
            if (newAccountId != null) {
                accountId = newAccountId
                saveStripeAccountIdToFirestore(newAccountId)
                checkAccountStatus(newAccountId)
            } else {
                updateStatus("Error creating account")
                Toast.makeText(this@VendorOnboardActivity, "Failed to create connect account", Toast.LENGTH_SHORT).show()
            }
        }
    }

    private fun checkForExistingAccount() {
        lifecycleScope.launch {
            val userId = auth.currentUser?.uid
            if (userId != null) {
                val vendorRef = db.collection("Vendors").document(userId)
                val document = vendorRef.get().await()
                val existingAccountId = document.getString("stripeAccountId")
                if (existingAccountId != null) {
                    accountId = existingAccountId
                    Log.d("Stripe", "Found existing account ID: $existingAccountId")
                    checkAccountStatus(existingAccountId)
                } else {
                }
            } else {
                updateStatus("Error: User not authenticated")
                Toast.makeText(this@VendorOnboardActivity, "User not authenticated", Toast.LENGTH_SHORT).show()
            }

        }
    }
    private suspend fun checkAccountStatus(accountId: String) {
        try {
            val accountDetails = stripeApiService.getAccountDetails(accountId)
            if (accountDetails.isSuccessful) {
                val account = accountDetails.body()
                if (account != null) {
                    val requirementsMap = mutableMapOf<String, Any>()
                    requirementsMap["eventually_due"] = account.requirements?.eventually_due ?: emptyList<String>()

                    if ((requirementsMap["eventually_due"] as? List<String>)?.isNotEmpty() == true) {
                        updateStatus("Action Needed")
                        updateButtonText("Continue Onboarding")

                        val requirementsMessage = formatRequiredFieldsMessage(requirementsMap)
                        updateRequirements(requirementsMessage)
                    } else if (account.charges_enabled) {
                        updateStatus("Account Onboarded")
                        updateButtonText("Account Onboarded")
                    } else {
                        updateStatus("Account Created")
                        updateButtonText("Continue Onboarding")
                    }
                } else {
                    updateStatus("Error: Account details are null")
                    Log.e("StripeOnboard", "Account details response body is null")
                }
            } else {
                val errorBody = accountDetails.errorBody()?.string()
                Log.e("StripeOnboard", "Error retrieving account status: $errorBody")
                updateStatus("Error retrieving status: $errorBody")
            }
        } catch (e: Exception) {
            Log.e("StripeOnboard", "Exception retrieving account status: ${e.message}")
            updateStatus("Error: ${e.message}")
        }
    }

    private fun handleIntent(intent: Intent?) {
        intent?.data?.let { uri ->
            val pathSegments = uri.pathSegments
            if (pathSegments.isNotEmpty()) {
                when (pathSegments[0]) {
                    "onboarding_complete" -> {
                        val id = uri.getQueryParameter("id")
                        Log.d("Stripe", "Onboarding completed! ID: $id")
                        Toast.makeText(this, "Stripe onboarding complete!", Toast.LENGTH_SHORT).show()
                    }
                    "refresh" -> {
                        val id = uri.getQueryParameter("id")
                        Log.d("Stripe", "Refresh link clicked! ID: $id")
                        Toast.makeText(this, "Regenerating Stripe onboarding link...", Toast.LENGTH_SHORT).show()
                    }
                    else -> {
                        Log.w("Stripe", "Unknown link path: $uri")
                    }
                }
            } else {
                Log.w("Stripe", "Empty path in link: $uri")
            }
        }
    }

    private fun generateOnboardingLink() {
        if (accountId == null) {
            Toast.makeText(this, "Please create a Connect account first", Toast.LENGTH_SHORT).show()
            return
        }
        val uniqueId = UUID.randomUUID().toString()
        val returnUrl = "https://f-andrade27.github.io/stripe_return.html?id=$uniqueId"
        val refreshUrl = "https://f-andrade27.github.io/stripe_refresh.html?id=$uniqueId"

        lifecycleScope.launch {
            val onboardingUrl = onboardVendor(accountId!!, refreshUrl, returnUrl)
            if (onboardingUrl != null) {
                Log.d("Stripe", "Redirecting to: $onboardingUrl")

                openUrlInBrowser(this@VendorOnboardActivity, onboardingUrl)

            } else {
                Toast.makeText(this@VendorOnboardActivity, "Failed to generate onboarding link", Toast.LENGTH_SHORT).show()
            }
        }
    }

    private suspend fun onboardVendor(accountId: String, refreshUrl: String, returnUrl: String): String? {
        return try {
            val request = GenerateAccountLinkRequest(accountId, refreshUrl, returnUrl)
            val response = stripeApiService.generateAccountLink(request)
            if (response.isSuccessful) {
                response.body()?.url
            } else {
                Log.e("Stripe", "Error generating link: ${response.errorBody()?.string()}")
                null
            }
        } catch (e: Exception) {
            Log.e("Stripe", "Exception generating link: ${e.message}", e)
            null
        }
    }

    private suspend fun createVendorAccount(): String? = withContext(Dispatchers.IO) {
        try {
            val currentUser = auth.currentUser
            if (currentUser != null) {
                val request = CreateConnectAccountRequest(
                    email = currentUser.email ?: "",
                    userId = currentUser.uid
                )
                val response = stripeApiService.createConnectAccount(request)
                if (response.isSuccessful) {
                    response.body()?.accountId
                } else {
                    Log.e("Stripe", "Error creating account: ${response.errorBody()?.string()}")
                    null
                }
            } else {
                Log.e("Stripe", "User not authenticated when attempting to create connect account")
                null
            }
        } catch (e: Exception) {
            Log.e("Stripe", "Exception creating account: ${e.message}", e)
            null
        }
    }

    private fun formatRequiredFieldsMessage(requirements: Map<String, Any>?): String {
        if (requirements == null) {
            return "Unable to determine required information."
        }

        val eventuallyDue = (requirements["eventually_due"] as? List<String>) ?: emptyList()

        if (eventuallyDue.isEmpty()) {
            return "No additional information is currently required."
        }

        val fieldDescriptions = eventuallyDue.mapNotNull { field ->
            when (field) {
                "individual.dob.day" -> "Day of birth"
                "individual.dob.month" -> "Month of birth"
                "individual.dob.year" -> "Year of birth"
                "individual.ssn_last_4" -> "Last 4 digits of Social Security Number"
                "individual.address.line1" -> "Address Line 1"
                "individual.address.city" -> "City"
                "individual.address.state" -> "State"
                "individual.address.postal_code" -> "Postal Code"
                else -> null
            }
        }

        if (fieldDescriptions.isEmpty()) {
            return "Additional information is required, but details are unavailable."
        }

        val message = "To complete your onboarding, please provide the following information:\n" +
                fieldDescriptions.joinToString("\n- ", prefix = "- ")

        return message
    }

    private fun saveStripeAccountIdToFirestore(accountId: String) {
        val userId = auth.currentUser?.uid
        val vendorRef = db.collection("Vendors").document(userId ?: "")
        vendorRef.update("stripeAccountId", accountId)
            .addOnSuccessListener {
                Log.d("Firestore", "Stripe account ID saved to Firestore")
            }
            .addOnFailureListener { e ->
                Log.e("Firestore", "Error saving Stripe account ID to Firestore", e)
            }
    }

    private fun updateStatus(status: String) {
        accountStatus = status
        runOnUiThread {
            accountStatusTextView.text = "Account Status:\n$status"
        }
    }

    private fun updateRequirements(requirements: String) {
        runOnUiThread {
            accountRequirementsTextView.text = requirements
        }
    }

    private fun updateButtonText(buttonText: String) {
        runOnUiThread {
            onboardButton.text = buttonText
        }
    }

    private fun openUrlInBrowser(context: Context, url: String) {
        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(url))
        context.startActivity(intent)
    }
}
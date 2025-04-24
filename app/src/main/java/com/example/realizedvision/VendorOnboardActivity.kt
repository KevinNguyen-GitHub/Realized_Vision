package com.example.realizedvision.onboard

import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import androidx.lifecycle.lifecycleScope
import com.example.realizedvision.*
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FirebaseFirestore
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.tasks.await
import kotlinx.coroutines.withContext
import java.util.*

/**
 * Handles Stripe Connect onboarding for vendors.
 *
 * Flow
 * ────────────────────────────────────────────────────────────────────────────
 *  1.  “Create account” → POST /v1/accounts → store accountId in `Vendors`.
 *  2.  “Continue onboarding” → POST /v1/account_links → open browser.
 *  3.  Deep-link redirects back here ⇒ `checkAccountStatus()`.
 *  4.  When `charges_enabled = true` we flip the UI to “Onboarded”.
 */
class VendorOnboardActivity : AppCompatActivity() {

    /* ────────────────── Firebase / Stripe ─────────────────── */
    private val auth get() = FirebaseAuth.getInstance()
    private val db   get() = FirebaseFirestore.getInstance()
    private val stripe = NetworkModule.provideStripeApiService()

    /* ────────────────────── view refs ─────────────────────── */
    private val btn   by lazy { findViewById<com.google.android.material.button.MaterialButton>(R.id.vendorOnboardButton) }
    private val tvSta by lazy { findViewById<android.widget.TextView>(R.id.onboardStatusTextView) }
    private val tvReq by lazy { findViewById<android.widget.TextView>(R.id.onboardRequirementsTextView) }

    /* ────────────────────── state ─────────────────────────── */
    private var accountId:   String? = null
    private var currentUser  = auth.currentUser

    /* ═════════════════════════ lifecycle ═════════════════════ */
    override fun onCreate(saved: Bundle?) {
        super.onCreate(saved)
        setContentView(R.layout.activity_vendor_onboard)

        btn.setOnClickListener { onButtonClick() }
        handleDeepLink(intent)
        fetchExistingAccount()
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        handleDeepLink(intent)
    }

    /* ───────────────────── UI actions ─────────────────────── */
    private fun onButtonClick() {
        when (btn.text.toString()) {
            "Create Connect Account" -> createConnectAccount()
            "Continue Onboarding"    -> createOnboardingLink()
            else                     -> toast("Nothing to do")
        }
    }

    /* ────────────────── 1) create account ─────────────────── */
    private fun createConnectAccount() = lifecycleScope.launch {
        setLoading(true)
        val id = runCatching { stripe.createConnectAccount(
            CreateConnectAccountRequest(currentUser?.email ?: "", currentUser!!.uid)
        ) }.getOrNull()?.body()?.accountId

        if (id == null) {
            toast("Failed to create account"); setLoading(false); return@launch
        }

        accountId = id
        db.collection("Vendors").document(currentUser!!.uid)
            .update("stripeAccountId", id)

        toast("Account created")
        checkAccountStatus()
    }

    /* ────────────────── 2) account link ───────────────────── */
    private fun createOnboardingLink() = lifecycleScope.launch {
        val id = accountId ?: return@launch toast("Account id missing")
        val uuid = UUID.randomUUID().toString()

        val url = runCatching { stripe.generateAccountLink(
            GenerateAccountLinkRequest(
                id,
                "https://f-andrade27.github.io/stripe_refresh.html?id=$uuid",
                "https://f-andrade27.github.io/stripe_return.html?id=$uuid"
            )
        ) }.getOrNull()?.body()?.url

        if (url == null) { toast("Failed to create link"); return@launch }
        startActivity(Intent(Intent.ACTION_VIEW, Uri.parse(url)))
    }

    /* ────────────────── 3) polling / status ───────────────── */
    private fun fetchExistingAccount() = lifecycleScope.launch {
        val id = db.collection("Vendors").document(currentUser!!.uid)
            .get().await().getString("stripeAccountId")
        if (id != null) { accountId = id; checkAccountStatus() }
    }

    private fun checkAccountStatus() = lifecycleScope.launch {
        val id = accountId ?: return@launch
        setLoading(true)

        val resp = stripe.getAccountDetails(id)
        if (!resp.isSuccessful) {
            updateState("Error", resp.errorBody()?.string())
            return@launch
        }
        val acc = resp.body()!!
        when {
            acc.requirements?.eventually_due.orEmpty().isNotEmpty() -> {
                updateState("Action Needed",
                    formatRequirements(acc.requirements?.eventually_due))
                btn.text = "Continue Onboarding"
            }
            acc.charges_enabled        -> updateState("Onboarded")
            else                       -> {
                updateState("Account Created")
                btn.text = "Continue Onboarding"
            }
        }
        setLoading(false)
    }

    /* ─────────────────── deep-link handler ────────────────── */
    private fun handleDeepLink(i: Intent?) = i?.data?.let { uri ->
        if (uri.pathSegments.firstOrNull()=="onboarding_complete") {
            toast("Stripe onboarding complete"); checkAccountStatus()
        }
    }

    /* ──────────────────── helpers ─────────────────────────── */
    private fun setLoading(b:Boolean){ btn.isEnabled=!b; btn.text =
        if(b) "Please wait…" else btn.text }

    private fun updateState(status:String, req:String?=null){
        tvSta.text = "Account Status:\n$status"
        tvReq.text = req ?: getString(R.string.vendor_onboard_default_req)
    }

    private fun formatRequirements(list: List<String>?): String =
        buildString {
            appendLine("Provide the following:")
            list.orEmpty().forEach { appendLine("• ${fieldName(it)}") }
        }

    private fun fieldName(f:String) = when(f){
        "individual.dob.day"     -> "Day of birth"
        "individual.dob.month"   -> "Month of birth"
        "individual.dob.year"    -> "Year of birth"
        "individual.ssn_last_4"  -> "SSN (last 4)"
        else                     -> f.replace('_',' ')
    }

    private fun toast(m:String) = Toast.makeText(this,m,Toast.LENGTH_SHORT).show()
}

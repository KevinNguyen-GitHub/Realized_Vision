package com.example.realizedvision

import android.content.Intent
import android.os.Bundle
import android.util.Log
import android.widget.Button
import android.widget.ExpandableListView
import android.widget.ImageButton
import android.widget.TextView
import androidx.appcompat.app.AppCompatActivity
import androidx.lifecycle.lifecycleScope
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FirebaseFirestore
import com.stripe.android.PaymentConfiguration
import com.stripe.android.paymentsheet.PaymentSheet
import com.stripe.android.paymentsheet.PaymentSheetResult
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.util.Date

/**
 * Displays a checkout summary, creates a Stripe PaymentIntent, and (on success)
 * moves the user’s cart into **Order History**.
 *
 * Uses Firebase for user/cart storage and a simple Retrofit service wrapper for
 * Stripe‐server calls (see [NetworkModule.provideStripeApiService]).
 */
class CheckoutActivity : AppCompatActivity() {

    /* ─────────────────────────── Firebase / Stripe ────────────────────────── */
    private val db   by lazy { FirebaseFirestore.getInstance() }
    private val auth by lazy { FirebaseAuth.getInstance() }
    private val stripeApi  = NetworkModule.provideStripeApiService()

    /* ─────────────────────────── UI refs ─────────────────────────── */
    private lateinit var listView: ExpandableListView
    private lateinit var subtotalTv: TextView
    private lateinit var payBtn: Button
    private lateinit var backBtn: ImageButton

    private lateinit var paymentSheet: PaymentSheet
    private var clientSecret = ""
    private var subtotal = 0.0

    /* ───────────────────────── life-cycle ─────────────────────────── */
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_checkout)

        initViews()
        paymentSheet = PaymentSheet(this, ::onPaymentSheetResult)

        fetchStripeConfig()
        fetchShoppingCart()
    }

    /* ─────────────────────── view wiring ─────────────────────────── */
    private fun initViews() = with(findViewById<ExpandableListView>(R.id.orderSummaryExpandableList)) {
        listView   = this
        subtotalTv = findViewById(R.id.checkout_subtotal)
        payBtn     = findViewById(R.id.pay_now_button)
        backBtn    = findViewById(R.id.backButtonCheckout)

        payBtn.setOnClickListener { createPaymentIntent() }
        backBtn.setOnClickListener {
            startActivity(Intent(this@CheckoutActivity, ShoppingCartActivity::class.java))
        }
    }

    /* ─────────────────────── Stripe config ───────────────────────── */
    private fun fetchStripeConfig() = lifecycleScope.launch {
        runCatching {
            withContext(Dispatchers.IO) { stripeApi.getStripeConfig() }
        }.onSuccess { res ->
            if (res.isSuccessful) {
                res.body()?.stripePublishableKey?.let {
                    PaymentConfiguration.init(applicationContext, it)
                } ?: Log.e(TAG, "Publishable key missing")
            } else {
                Log.e(TAG, "Stripe config error: ${res.errorBody()?.string()}")
            }
        }.onFailure { e -> Log.e(TAG, "Stripe config request failed", e) }
    }

    /* ─────────────────────── cart fetch / UI ─────────────────────── */
    private fun fetchShoppingCart() {
        val uid = auth.currentUser?.uid ?: return Log.e(TAG, "User not logged in")

        db.collection("Users").document(uid).collection("Shopping Cart")
            .get()
            .addOnSuccessListener { docs ->
                val items = docs.mapNotNull { it.toObject(Item::class.java) }
                updateUi(items)
            }
            .addOnFailureListener { e -> Log.w(TAG, "Error loading cart", e) }
    }

    private fun updateUi(items: List<Item>) {
        listView.setAdapter(
            OrderSummaryExpandableListAdapter(
                this,
                "Order Summary for ${items.size} item${if (items.size == 1) "" else "s"}",
                items
            )
        )
        listView.expandGroup(0)

        subtotal = items.sumOf { it.price * it.quantity }
        subtotalTv.text = getString(R.string.total_fmt, subtotal)
    }

    /* ───────────────────── PaymentIntent flow ─────────────────────── */
    private fun createPaymentIntent() = lifecycleScope.launch {
        if (subtotal == 0.0) return@launch

        val req = CreatePaymentIntentRequest(
            amount = (subtotal * 100).toLong(),
            currency = "usd",
            automatic_payment_methods = AutomaticPaymentMethods()
        )

        runCatching {
            withContext(Dispatchers.IO) { stripeApi.createPaymentIntent(req) }
        }.onSuccess { res ->
            if (res.isSuccessful) {
                clientSecret = res.body()?.client_secret.orEmpty()
                presentPaymentSheet()
            } else {
                Log.e(TAG, "PaymentIntent error: ${res.errorBody()?.string()}")
            }
        }.onFailure { e -> Log.e(TAG, "PaymentIntent request failed", e) }
    }

    private fun presentPaymentSheet() {
        paymentSheet.presentWithPaymentIntent(
            clientSecret,
            PaymentSheet.Configuration("Example, Inc.")
        )
    }

    private fun onPaymentSheetResult(r: PaymentSheetResult) = when (r) {
        is PaymentSheetResult.Completed -> {
            Log.d(TAG, "Payment complete")
            moveCartToOrderHistory()
            startActivity(Intent(this, MainActivity::class.java))
        }
        PaymentSheetResult.Canceled -> Log.d(TAG, "Payment canceled")
        is PaymentSheetResult.Failed -> Log.e(TAG, "Payment failed", r.error)
    }

    /* ─────────────────── order-history migration ──────────────────── */
    private fun moveCartToOrderHistory() {
        val uid = auth.currentUser?.uid ?: return Log.e(TAG, "User not logged in")

        val cartRef = db.collection("Users").document(uid).collection("Shopping Cart")
        cartRef.get().addOnSuccessListener { cartSnap ->
            if (cartSnap.isEmpty) return@addOnSuccessListener

            val order = hashMapOf(
                "userId" to uid,
                "items"  to cartSnap.documents.map { it.data },
                "timestamp" to Date()
            )

            db.collection("Order History").add(order)
                .addOnSuccessListener {
                    cartSnap.documents.forEach { d -> cartRef.document(d.id).delete() }
                }
                .addOnFailureListener { e -> Log.e(TAG, "Failed to place order", e) }
        }.addOnFailureListener { e -> Log.e(TAG, "Failed to fetch cart", e) }
    }

    companion object { private const val TAG = "CheckoutActivity" }
}

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
import com.google.firebase.firestore.DocumentReference
import com.google.firebase.firestore.FirebaseFirestore
import com.google.firebase.firestore.QuerySnapshot
import com.stripe.android.PaymentConfiguration
import com.stripe.android.paymentsheet.PaymentSheet
import com.stripe.android.paymentsheet.PaymentSheetResult
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.util.Date


class CheckoutActivity : AppCompatActivity() {

    private lateinit var orderSummaryExpandableList: ExpandableListView
    private lateinit var checkoutSubtotal: TextView
    private val db = FirebaseFirestore.getInstance()
    private val auth = FirebaseAuth.getInstance()
    private lateinit var paymentSheet: PaymentSheet
    private lateinit var paymentIntentClientSecret: String
    private val stripeApiService = NetworkModule.provideStripeApiService()
    private lateinit var payNowButton: Button
    private lateinit var backButton: ImageButton
    private var subtotal: Double = 0.0

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_checkout)

        orderSummaryExpandableList = findViewById(R.id.orderSummaryExpandableList)
        checkoutSubtotal = findViewById(R.id.checkout_subtotal)
        payNowButton = findViewById(R.id.pay_now_button)
        backButton = findViewById(R.id.backButtonCheckout)

        paymentSheet = PaymentSheet(this, ::onPaymentSheetResult)
        fetchStripeConfig()
        fetchShoppingCart()
        payNowButton.setOnClickListener {
            createPaymentIntent()
        }
        backButton.setOnClickListener{
            val intent = Intent(this, ShoppingCartActivity::class.java)
            startActivity(intent)
        }
    }
    private fun fetchStripeConfig() {
        CoroutineScope(Dispatchers.Main).launch {
            try {
                val response = withContext(Dispatchers.IO) {
                    stripeApiService.getStripeConfig()
                }
                if (response.isSuccessful) {
                    val config = response.body()
                    val publishableKey = config?.stripePublishableKey
                    if (publishableKey != null) {
                        PaymentConfiguration.init(applicationContext, publishableKey)
                        Log.d("MyApplication", "Stripe PaymentConfiguration initialized successfully")
                    } else {
                        Log.e("MyApplication", "Publishable key is null")
                    }
                } else {
                    Log.e("MyApplication", "Error fetching Stripe config: ${response.errorBody()?.string()}")
                }
            } catch (e: Exception) {
                Log.e("MyApplication", "Exception fetching Stripe config", e)
            }
        }
    }

    private fun fetchShoppingCart() {
        val userId = auth.currentUser?.uid
        if (userId == null) {
            Log.e("CheckoutActivity", "User not logged in")
            return
        }

        db.collection("Users").document(userId).collection("Shopping Cart")
            .get()
            .addOnSuccessListener { cartDocuments ->
                val items = mutableListOf<Item>()
                for (document in cartDocuments) {
                    val item = document.toObject(Item::class.java)
                    if (item != null) {
                        items.add(item)
                    }
                }
                updateUI(items)
            }
            .addOnFailureListener { exception ->
                Log.w("CheckoutActivity", "Error getting shopping cart", exception)
            }
    }

    private fun updateUI(items: List<Item>) {

        val adapter = OrderSummaryExpandableListAdapter(this, "Order Summary for ${items.size} items", items)
        orderSummaryExpandableList.setAdapter(adapter)

        subtotal = items.sumOf { it.getPrice() * it.getQuantity() }
        checkoutSubtotal.text = "Total: $${String.format("%.2f", subtotal)}"

        orderSummaryExpandableList.expandGroup(0)
    }

    private fun createPaymentIntent() {
        lifecycleScope.launch {
            try {
                val amountInCents = (subtotal * 100).toLong()
                val request = CreatePaymentIntentRequest(
                    amount = amountInCents,
                    currency = "usd",
                    automatic_payment_methods = AutomaticPaymentMethods()
                )

                val response = withContext(Dispatchers.IO) {
                    stripeApiService.createPaymentIntent(request)
                }

                if (response.isSuccessful) {
                    val paymentIntentResponse = response.body()
                    paymentIntentClientSecret = paymentIntentResponse?.client_secret ?: ""
                    presentPaymentSheet()
                } else {
                    Log.e("CheckoutActivity", "Error creating payment intent: ${response.errorBody()?.string()}")
                }
            } catch (e: Exception) {
                Log.e("CheckoutActivity", "Exception creating payment intent", e)
            }
        }
    }

    private fun presentPaymentSheet() {
        paymentSheet.presentWithPaymentIntent(paymentIntentClientSecret, PaymentSheet.Configuration("Example, Inc."))
    }
    private fun onPaymentSheetResult(paymentSheetResult: PaymentSheetResult) {
        when (paymentSheetResult) {
            is PaymentSheetResult.Completed -> {
                Log.d("CheckoutActivity", "Payment completed")
                addOrderToHistory()
                val intent = Intent(this, MainActivity::class.java)
                startActivity(intent)
            }
            is PaymentSheetResult.Canceled -> {
                Log.d("CheckoutActivity", "Payment canceled")
            }
            is PaymentSheetResult.Failed -> {
                Log.e("CheckoutActivity", "Payment failed", paymentSheetResult.error)
            }
        }
    }
    private fun addOrderToHistory(){
        val userId = auth.currentUser?.uid
        if (userId == null) {
            Log.e("CheckoutActivity", "User not logged in")
            return
        }

        // Gets the shopping cart items
        db.collection("Users").document(userId).collection("Shopping Cart").get()
            .addOnSuccessListener { queryDocumentSnapshots: QuerySnapshot ->
                val items: MutableList<Map<String, Any>?> = ArrayList()
                for (document in queryDocumentSnapshots) {
                    items.add(document.data)
                }

                // Creates a new order document
                val order: MutableMap<String, Any> = HashMap()
                order["userId"] = userId
                order["items"] = items
                order["timestamp"] = Date()

                db.collection("Order History").add(order)
                    .addOnSuccessListener { documentReference: DocumentReference? ->
                        for (document in queryDocumentSnapshots) {
                            db.collection("Users").document(userId).collection("Shopping Cart")
                                .document(document.id)
                                .delete()
                        }
                    }
                    .addOnFailureListener { e: java.lang.Exception? -> println("Shopping cart is empty for user: $e")}
            }
            .addOnFailureListener { e: java.lang.Exception? -> println("Error retrieving shopping cart for user: $e")}

    }

}
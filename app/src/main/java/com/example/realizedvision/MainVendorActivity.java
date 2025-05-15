package com.example.realizedvision;

import android.app.AlertDialog;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.*;

import java.util.*;

import retrofit2.Response;

public class MainVendorActivity extends AppCompatActivity {

    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private String vendorId;
    private String stripeAccountId;
    private StripeApiService stripeApiService = NetworkModule.INSTANCE.provideStripeApiService();
    private StripeApiHelper stripeApiHelper;
    private TextView profileNameTextView;

    private RecyclerView vendorRequestsRecyclerView;
    private CommissionVendorAdapter vendorAdapter;
    private List<CommissionRequest> vendorRequests = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendormain);

        // Firebase and Stripe Initialization
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();
        vendorId = currentUser.getUid();
        stripeApiHelper = new StripeApiHelper(stripeApiService);

        // Initialize UI Elements
        profileNameTextView = findViewById(R.id.vendor_greeting_text);
        ImageView messageIcon = findViewById(R.id.nav_messages_icon);
        ImageView profileIcon = findViewById(R.id.nav_profile_icon);

        Button addItemButton = findViewById(R.id.add_item_button);
        Button deleteItemButton = findViewById(R.id.delete_item_button);
        Button viewOrdersButton = findViewById(R.id.view_orders_button);
        Button viewMoreButton = findViewById(R.id.view_analytics_button);

        // RecyclerView Setup for Vendor Requests
        vendorRequestsRecyclerView = findViewById(R.id.vendor_requests_recycler_view);
        vendorAdapter = new CommissionVendorAdapter(this, vendorRequests);
        vendorAdapter.setOnStatusChangedListener(vendorAdapter::notifyDataSetChanged);
        vendorRequestsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        vendorRequestsRecyclerView.setAdapter(vendorAdapter);

        // Button Listeners
        viewMoreButton.setOnClickListener(v -> openStripeDashboard());
        addItemButton.setOnClickListener(v -> addItem(vendorId));
        deleteItemButton.setOnClickListener(v -> deleteItem(vendorId));
        viewOrdersButton.setOnClickListener(v -> navigateTo(OrderHistoryActivity.class));

        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(StorefrontActivity.class));

        // Load Data
        fetchUserData();
        checkForStripeAccount();
        loadVendorRequests();
    }

    private void loadVendorRequests() {
        firestore.collection("CommissionRequests")
                .get()
                .addOnSuccessListener(snapshots -> {
                    vendorRequests.clear();
                    if (snapshots != null && !snapshots.isEmpty()) {
                        for (QueryDocumentSnapshot doc : snapshots) {
                            CommissionRequest req = doc.toObject(CommissionRequest.class);
                            if (req != null) {
                                req.setDocumentId(doc.getId());
                                vendorRequests.add(req);
                            }
                        }
                        vendorAdapter.notifyDataSetChanged();
                    }
                })
                .addOnFailureListener(e -> showToast("Failed to load requests"));
    }

    private void fetchUserData() {
        if (currentUser != null) {
            DocumentReference userDocRef = firestore.collection("Vendors").document(vendorId);
            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful() && task.getResult().exists()) {
                    String companyName = task.getResult().getString("companyName");
                    profileNameTextView.setText("Welcome, " + (companyName != null ? companyName : "Vendor") + "!");
                } else {
                    showToast("User data not found.");
                }
            });
        }
    }

    private void addItem(String vendorId) {
        View viewInflated = LayoutInflater.from(this).inflate(R.layout.dialog_add_item, null);
        EditText inputName = viewInflated.findViewById(R.id.input_name);
        EditText inputDescription = viewInflated.findViewById(R.id.input_description);
        EditText inputPrice = viewInflated.findViewById(R.id.input_price);
        EditText inputCategory = viewInflated.findViewById(R.id.input_category);

        new AlertDialog.Builder(this)
                .setTitle("Add New Item")
                .setView(viewInflated)
                .setPositiveButton("Add", (dialog, which) -> {
                    String name = inputName.getText().toString().trim();
                    String description = inputDescription.getText().toString().trim();
                    String priceStr = inputPrice.getText().toString().trim();
                    String category = inputCategory.getText().toString().trim();

                    if (!name.isEmpty() && !description.isEmpty() && !priceStr.isEmpty() && !category.isEmpty()) {
                        checkDuplicateAndAddItem(name, description, Double.parseDouble(priceStr), category, vendorId);
                    }
                })
                .setNegativeButton("Cancel", (dialog, which) -> dialog.cancel())
                .show();
    }

    private void checkDuplicateAndAddItem(String name, String description, double price, String category, String vendorId) {
        firestore.collection("Storefront")
                .whereEqualTo("name", name)
                .whereEqualTo("vendorID", vendorId)
                .get()
                .addOnSuccessListener(querySnapshot -> {
                    if (querySnapshot.isEmpty()) {
                        addNewItemToFirestore(name, description, price, category, vendorId);
                    } else {
                        showToast("Item with this name already exists!");
                    }
                })
                .addOnFailureListener(e -> showToast("Error checking existing items."));
    }

    private void addNewItemToFirestore(String name, String description, double price, String category, String vendorId) {
        String itemID = firestore.collection("Storefront").document().getId();
        Map<String, Object> item = new HashMap<>();
        item.put("name", name);
        item.put("description", description);
        item.put("price", price);
        item.put("category", category);
        item.put("itemID", itemID);
        item.put("vendorID", vendorId);
        item.put("quantity", 1);
        item.put("imageUrl", "");

        firestore.collection("Storefront").document(itemID)
                .set(item)
                .addOnSuccessListener(aVoid -> showToast("Item Added Successfully"))
                .addOnFailureListener(e -> showToast("Error Adding Item"));
    }

    private void deleteItem(String vendorId) {
        View viewInflated = LayoutInflater.from(this).inflate(R.layout.dialog_delete_item, null);
        EditText inputItem = viewInflated.findViewById(R.id.input_item_id);

        new AlertDialog.Builder(this)
                .setTitle("Delete Item")
                .setView(viewInflated)
                .setPositiveButton("Delete", (dialog, which) -> {
                    String inputText = inputItem.getText().toString().trim();
                    if (!inputText.isEmpty()) {
                        attemptDeleteByIdOrName(inputText, vendorId);
                    }
                })
                .setNegativeButton("Cancel", (dialog, which) -> dialog.cancel())
                .show();
    }

    private void attemptDeleteByIdOrName(String itemIdOrName, String vendorId) {
        DocumentReference itemRef = firestore.collection("Storefront").document(itemIdOrName);
        itemRef.get().addOnSuccessListener(documentSnapshot -> {
            if (documentSnapshot.exists() && vendorId.equals(documentSnapshot.getString("vendorID"))) {
                itemRef.delete().addOnSuccessListener(aVoid -> showToast("Item Deleted Successfully"));
            } else {
                searchAndDeleteByName(itemIdOrName, vendorId);
            }
        }).addOnFailureListener(e -> showToast("Error retrieving item data."));
    }

    private void searchAndDeleteByName(String itemName, String vendorId) {
        firestore.collection("Storefront")
                .whereEqualTo("name", itemName)
                .whereEqualTo("vendorID", vendorId)
                .get()
                .addOnSuccessListener(snapshot -> {
                    if (!snapshot.isEmpty()) {
                        snapshot.forEach(doc -> doc.getReference().delete());
                        showToast("Item Deleted Successfully");
                    } else {
                        showToast("Item not found.");
                    }
                })
                .addOnFailureListener(e -> showToast("Error searching for item."));
    }

    private void openStripeDashboard() {
        if (stripeAccountId == null) {
            createConnectAccount();
        } else {
            generateDashboardLink();
        }
    }

    private void generateDashboardLink() {
        if (stripeAccountId == null || stripeAccountId.isEmpty()) {
            showToast("Stripe account not set up");
            createConnectAccount();
            return;
        }

        String returnUrl = "https://f-andrade27.github.io/stripe_return.html?id=" + UUID.randomUUID();
        stripeApiHelper.generateDashboardLink(stripeAccountId, returnUrl, new StripeApiHelper.Callback<GenerateDashboardLinkResponse>() {
            @Override
            public void onSuccess(@NonNull Response<GenerateDashboardLinkResponse> response) {
                if (response.isSuccessful() && response.body() != null) {
                    openUrlInBrowser(response.body().getUrl());
                } else {
                    showToast("Failed to generate URL");
                }
            }

            @Override
            public void onError(@NonNull Throwable throwable) {
                showToast("Error: " + throwable.getMessage());
            }
        });
    }

    private void checkForStripeAccount() {
        firestore.collection("Vendors").document(vendorId)
                .get()
                .addOnSuccessListener(doc -> {
                    if (doc.exists()) stripeAccountId = doc.getString("stripeAccountId");
                })
                .addOnFailureListener(e -> Log.e("Vendor Main", "Error checking vendor: " + e.getMessage()));
    }

    private void createConnectAccount() {
        new AlertDialog.Builder(this)
                .setTitle("Stripe Account Setup")
                .setMessage("You must create a Stripe Account to view your analytics")
                .setPositiveButton("Set up", (dialog, which) -> startStripeAccountCreation())
                .setNegativeButton("Cancel", null)
                .show();
    }

    private void startStripeAccountCreation() {
        View loadingView = getLayoutInflater().inflate(R.layout.loading_overlay, null);
        ((TextView) loadingView.findViewById(R.id.loading_text)).setText("Creating account...");
        ((ViewGroup) findViewById(android.R.id.content)).addView(loadingView);

        stripeApiHelper.createConnectAccount(currentUser.getEmail(), vendorId, new StripeApiHelper.Callback<CreateConnectAccountResponse>() {
            @Override
            public void onSuccess(@NonNull Response<CreateConnectAccountResponse> response) {
                ((ViewGroup) findViewById(android.R.id.content)).removeView(loadingView);
                if (response.isSuccessful() && response.body() != null) {
                    stripeAccountId = response.body().getAccountId();
                    saveStripeAccountId(stripeAccountId);
                    generateDashboardLink();
                }
            }

            @Override
            public void onError(@NonNull Throwable throwable) {
                ((ViewGroup) findViewById(android.R.id.content)).removeView(loadingView);
                showToast("Error: " + throwable.getMessage());
            }
        });
    }

    private void saveStripeAccountId(String accountId) {
        firestore.collection("Vendors").document(vendorId)
                .update("stripeAccountId", accountId)
                .addOnSuccessListener(aVoid -> Log.d("Vendor Main", "Account ID saved successfully"))
                .addOnFailureListener(e -> Log.e("Vendor Main", "Error saving account ID: " + e.getMessage()));
    }

    private void openUrlInBrowser(String url) {
        try {
            startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(url)));
        } catch (ActivityNotFoundException e) {
            showToast("No browser available");
        }
    }

    private void navigateTo(Class<?> targetActivity) {
        startActivity(new Intent(this, targetActivity));
    }

    private void showToast(String message) {
        Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
    }
}

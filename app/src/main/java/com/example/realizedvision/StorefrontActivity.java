package com.example.realizedvision;

import android.content.Context;
import android.content.Intent;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.Gravity;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.QuerySnapshot;
import com.google.firebase.firestore.Source;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class StorefrontActivity extends AppCompatActivity {

    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private FirebaseFirestore firestore;
    private FirebaseAuth auth;
    private FirebaseUser currentUser;
    private TextView profileNameTextView;
    private String vendorId;
    private static final String PRESET_USER_ID = "presetUserId";
    private static final String PRESET_VENDOR_ID = "presetVendorId";


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendor_profile);

        profileNameTextView = findViewById(R.id.profile_name);
        auth = FirebaseAuth.getInstance();
        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        // Initialize RecyclerView
        recyclerView = findViewById(R.id.recyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));


        // Initialize item list and adapter
        itemList = new ArrayList<>();
        itemAdapter = new ItemAdapter(this, itemList, false);
        recyclerView.setAdapter(itemAdapter);


        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        ImageView settingsIcon = findViewById(R.id.settings_icon);
        ImageView calendarIcon = findViewById(R.id.calendar_icon);

        homeIcon.setOnClickListener(view -> checkUserType());
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        calendarIcon.setOnClickListener(view -> navigateTo(ViewCalendarActivity.class));

        profileIcon.setOnClickListener(view -> navigateTo(StorefrontActivity.class));

        settingsIcon.setOnClickListener(view -> navigateTo(SettingsActivity.class));


        //Listens for if user clicks add button, if they do then envoke add item function for popup and adding of
        //item to database and visible for recycler view for this specific user

        Intent intent = getIntent();
        if (intent != null && intent.hasExtra("vendorID")) {
            vendorId = intent.getStringExtra("vendorID");

            } else {
                Log.e("StorefrontActivity", "vendorID extra not found in Intent");
                vendorId = currentUser.getUid();
            }


        loadStorefrontItems(vendorId);
        fetchUserData();

        // Commission Request button
        MaterialButton requestButton = findViewById(R.id.RequestButton);
        requestButton.setOnClickListener(v -> {

            // Inflate the popup layout
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            final View popupView = inflater.inflate(R.layout.popup_commission_form, null);

            // Convert 350dp to pixels for popup width
            int widthPx = dpToPx(350);

            // Create a PopupWindow
            final PopupWindow popupWindow = new PopupWindow(
                    popupView,
                    widthPx,
                    ViewGroup.LayoutParams.WRAP_CONTENT,
                    true  // focusable
            );

            // Dismiss on outside touch
            popupWindow.setOutsideTouchable(true);
            popupWindow.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));

            // Show popup at center of screen
            popupWindow.showAtLocation(findViewById(android.R.id.content), Gravity.CENTER, 0, 0);

            final TextInputLayout nameInputLayout = popupView.findViewById(R.id.nameInputLayout);
            final TextInputLayout typeInputLayout = popupView.findViewById(R.id.typeInputLayout);
            final TextInputLayout sizeInputLayout = popupView.findViewById(R.id.sizeInputLayout);
            final TextInputLayout styleInputLayout = popupView.findViewById(R.id.styleInputLayout);

            final TextInputEditText nameEditText = popupView.findViewById(R.id.nameEditText);
            final TextInputEditText typeEditText = popupView.findViewById(R.id.typeEditText);
            final TextInputEditText sizeEditText = popupView.findViewById(R.id.sizeEditText);
            final TextInputEditText styleEditText = popupView.findViewById(R.id.styleEditText);

            Button submitButton = popupView.findViewById(R.id.submitButton);
            submitButton.setOnClickListener(view -> {
                boolean valid = true;
                String name = nameEditText.getText().toString().trim();
                String type = typeEditText.getText().toString().trim();
                String size = sizeEditText.getText().toString().trim();
                String style = styleEditText.getText().toString().trim();

                // Basic field validation
                if (TextUtils.isEmpty(name)) {
                    nameInputLayout.setError("Name is required");
                    valid = false;
                } else {
                    nameInputLayout.setError(null);
                }
                if (TextUtils.isEmpty(type)) {
                    typeInputLayout.setError("Type is required");
                    valid = false;
                } else {
                    typeInputLayout.setError(null);
                }
                if (TextUtils.isEmpty(size)) {
                    sizeInputLayout.setError("Size is required");
                    valid = false;
                } else {
                    sizeInputLayout.setError(null);
                }
                if (TextUtils.isEmpty(style)) {
                    styleInputLayout.setError("Style is required");
                    valid = false;
                } else {
                    styleInputLayout.setError(null);
                }

                if (valid) {
                    // Write all fields, including vendorId, userId, and timestamp
                    HashMap<String, Object> commissionRequest = new HashMap<>();
                    commissionRequest.put("name", name);
                    commissionRequest.put("type", type);
                    commissionRequest.put("size", size);
                    commissionRequest.put("style", style);

                    // Default status is "Pending"
                    commissionRequest.put("status", "Pending");

                    // vendorId, userId, timestamp
                    commissionRequest.put("vendorId", PRESET_VENDOR_ID);
                    commissionRequest.put("userId", PRESET_USER_ID);
                    commissionRequest.put("timestamp", System.currentTimeMillis());

                    firestore.collection("CommissionRequests")
                            .add(commissionRequest)
                            .addOnSuccessListener((DocumentReference documentReference) -> {
                                Toast.makeText(StorefrontActivity.this,
                                        "Commission request submitted", Toast.LENGTH_SHORT).show();

                                // Create a new CommissionRequest object
                                CommissionRequest newRequest = new CommissionRequest(
                                        name,
                                        type,
                                        "Pending",
                                        size,
                                        style,
                                        /* budget */ "",
                                        /* additionalNote */ ""
                                );
                                // Set vendorId, userId, and timestamp
                                //String userId = currentUser.toString();
                                newRequest.setVendorId(PRESET_VENDOR_ID);
                                newRequest.setUserId(PRESET_USER_ID);
                                newRequest.setTimestamp(System.currentTimeMillis());

                                // Set doc ID
                                newRequest.setDocumentId(documentReference.getId());

                                // Add the new request to both lists
                                //userRequests.add(newRequest);
                                //vendorRequests.add(newRequest);

                                //userAdapter.notifyDataSetChanged();
                                //vendorAdapter.notifyDataSetChanged();

                                popupWindow.dismiss();
                            })
                            .addOnFailureListener(e -> Toast.makeText(
                                    StorefrontActivity.this,
                                    "Error submitting request",
                                    Toast.LENGTH_SHORT).show()
                            );
                }
            });
        });

        fetchUserData();
    }




    private void loadStorefrontItems(String userId) {
        CollectionReference itemsRef = firestore.collection("Storefront");

        itemsRef.whereEqualTo("vendorID", userId)
                .addSnapshotListener(new EventListener<QuerySnapshot>() {
                    @Override
                    public void onEvent(@Nullable QuerySnapshot snapshot, @Nullable FirebaseFirestoreException error) {
                        if (error != null) {
                            Log.e("Firestore", "Error loading items", error);
                            return;
                        }
                        if (snapshot != null) {
                            itemList.clear();

                            for (DocumentSnapshot itemDoc : snapshot.getDocuments()) {
                                Item item = itemDoc.toObject(Item.class);

                                if (item != null) {
                                    itemList.add(item);
                                }
                            }
                            itemAdapter.notifyDataSetChanged();
                        }
                    }
                });
    }
    private void fetchUserData() {
        if(vendorId == null || vendorId.isEmpty()){
            Log.e("Storefront", "Inalid vendorId");
        }

        if (currentUser != null) {
            String userId = vendorId;

            DocumentReference userDocRef = firestore.collection("Vendors").document(userId);

            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot snapshot = task.getResult();

                    if (snapshot.exists() /*&& Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))*/) {
                        String companyName = snapshot.getString("companyName");

                        // Handle null values
                        companyName = (companyName != null) ? companyName : "";

                        // Use resource string with placeholders
                        String displayCompanyName = getString(R.string.profile_name_format, companyName, "");
                        profileNameTextView.setText(displayCompanyName);
                    } else {
                        Toast.makeText(StorefrontActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                } else {
                    Toast.makeText(StorefrontActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }
    private void checkUserType() {
        FirebaseUser currentUser = auth.getCurrentUser();
        if (currentUser == null) {
            Toast.makeText(this, "User not authenticated", Toast.LENGTH_SHORT).show();
            return;
        }

        String userID = currentUser.getUid();
        //check user type
        FirebaseFirestore.getInstance().collection("Users").document(userID).get()
                .addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        DocumentSnapshot document = task.getResult();
                        if (document.exists()) {
                            // Check if user is vendor
                            Boolean isVendor = document.getBoolean("isVendor");
                            Intent intent;

                            if (isVendor != null && isVendor) {
                                //User is vendor
                                intent = new Intent(StorefrontActivity.this, MainVendorActivity.class);
                            } else {
                                //Regular user
                                intent = new Intent(StorefrontActivity.this, MainActivity.class);
                            }
                            startActivity(intent);
                            finish();
                        } else {
                            //document doesn't exist, treat like regular user
                            startActivity(new Intent(StorefrontActivity.this, MainActivity.class));
                            finish();
                        }
                    } else {
                        Log.e("LoginActivity", "Error checking user type", task.getException());
                        //Treat as regular user if error
                        Toast.makeText(StorefrontActivity.this, "Error verifying account", Toast.LENGTH_SHORT).show();
                        startActivity(new Intent(StorefrontActivity.this, MainActivity.class));
                        finish();
                    }
                });
    }

    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(StorefrontActivity.this, targetActivity);
        startActivity(intent);
    }
    private int dpToPx(int dp) {
        return (int) (dp * getResources().getDisplayMetrics().density);
    }
}
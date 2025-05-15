package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.compose.ui.text.font.FontVariation;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

public class SettingsActivity extends AppCompatActivity {
    private FirebaseUser currentUser;
    private FirebaseFirestore firestore;
    private Button enablePayments;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_settings);

        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        getVendorStatus();


        Button termsAndConditionsButton = findViewById(R.id.tcButton);
        Button orderHistoryButton = findViewById(R.id.historyButton);
        Button changeUsernameButton = findViewById(R.id.changeUsernameButton);
        Button changePasswordButton = findViewById(R.id.changePasswordButton);
        Button notificationsButton = findViewById(R.id.notificationsButton);
        Button vendorInfoButton = findViewById(R.id.upgradeButton);
        ImageButton backButton = findViewById(R.id.backButtonChangePass);
        Button logoutButton = findViewById(R.id.btn_logout);
        Button addAddressButton = findViewById(R.id.addAddressButton);
        enablePayments = findViewById(R.id.enablePayments);

        addAddressButton.setOnClickListener(v -> {
            FirebaseFirestore firestore = FirebaseFirestore.getInstance();
            FirebaseUser currentUser = FirebaseAuth.getInstance().getCurrentUser();

            if (currentUser != null) {
                String userId = currentUser.getUid();
                firestore.collection("Users").document(userId).get()
                        .addOnSuccessListener(snapshot -> {
                            Boolean isVendor = snapshot.getBoolean("isVendor");
                            if (isVendor != null && isVendor) {
                                Toast.makeText(SettingsActivity.this, "You’ve already provided an address as a vendor.", Toast.LENGTH_SHORT).show();
                            } else {
                                startActivity(new Intent(SettingsActivity.this, AddAddressActivity.class));
                            }
                        })
                        .addOnFailureListener(e -> {
                            Toast.makeText(SettingsActivity.this, "Failed to check vendor status.", Toast.LENGTH_SHORT).show();
                        });
            }
        });

        //button handling for vendor filters (Search engine filters)
        Button setVendorFiltersButton = findViewById(R.id.setVendorFiltersButton);
        setVendorFiltersButton.setOnClickListener(view -> {
            FirebaseUser user = FirebaseAuth.getInstance().getCurrentUser();
            if (user != null) {
                FirebaseFirestore.getInstance().collection("Users")
                        .document(user.getUid())
                        .get()
                        .addOnSuccessListener(documentSnapshot -> {
                            Boolean isVendor = documentSnapshot.getBoolean("isVendor");
                            if (Boolean.TRUE.equals(isVendor)) {
                                startActivity(new Intent(SettingsActivity.this, VendorFilterActivity.class));
                            } else {
                                Toast.makeText(SettingsActivity.this, "You must be a vendor to set vendor filters.", Toast.LENGTH_SHORT).show();
                            }
                        });
            }
        });



        termsAndConditionsButton.setOnClickListener(view -> navigateTo(TermsAndConditionsActivity.class));
        orderHistoryButton.setOnClickListener(view -> navigateTo(OrderHistoryActivity.class));
        changeUsernameButton.setOnClickListener(view -> navigateTo(ChangeNameActivity.class));
        changePasswordButton.setOnClickListener(view -> navigateTo(ChangePasswordActivity.class));
        notificationsButton.setOnClickListener(view -> navigateTo(NotificationsActivity.class));
        vendorInfoButton.setOnClickListener(view -> navigateTo(VendorInfoActivity.class));
        enablePayments.setOnClickListener(view -> navigateTo(VendorOnboardActivity.class));

        backButton.setOnClickListener(view -> {
            if (currentUser != null) {
                String userId = currentUser.getUid();

                DocumentReference userDocRef = firestore.collection("Users").document(userId);

                userDocRef.get().addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        DocumentSnapshot snapshot = task.getResult();

                        if (snapshot.exists() && Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))) {
                            navigateToVendorStorefront(userId);
                        } else if (snapshot.exists() && Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))) {
                            navigateTo(ProfileActivity.class);
                        } else {
                            Toast.makeText(SettingsActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                        }
                    } else {
                        Toast.makeText(SettingsActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                    }
                });
            }
        });

        logoutButton.setOnClickListener(view -> {
            FirebaseAuth.getInstance().signOut(); // Sign out the user
            Intent intent = new Intent(SettingsActivity.this, LoginActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK); // Clear activity stack
            startActivity(intent);
            finish(); // Close current activity
        });
    }

    private void getVendorStatus() {
        if (currentUser != null) {
            String userId = currentUser.getUid();
            firestore.collection("Users").document(userId).get()
                    .addOnSuccessListener(snapshot -> {
                        Boolean isVendor = snapshot.getBoolean("isVendor");
                        if (isVendor != null && isVendor) {
                            enablePayments.setVisibility(View.VISIBLE);
                        } else {
                            enablePayments.setVisibility(View.GONE);
                        }
                    })
                    .addOnFailureListener(e -> {
                        Toast.makeText(SettingsActivity.this, "Failed to check vendor status.", Toast.LENGTH_SHORT).show();
                    });
        }
    }
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(SettingsActivity.this, targetActivity);
        startActivity(intent);
    }

    private void navigateToVendorStorefront(String vendorID) {
        Intent intent = new Intent(SettingsActivity.this, StorefrontActivity.class);
        intent.putExtra("vendorID", vendorID); // Pass the vendor ID to the storefront activity
        startActivity(intent);
    }
}

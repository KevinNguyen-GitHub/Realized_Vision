package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.ArrayList;
import java.util.List;

public class ProfileActivity extends AppCompatActivity {

    private TextView profileNameTextView;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;

    private RecyclerView userRequestsRecyclerView;
    private CommissionUserAdapter userAdapter;
    private List<CommissionRequest> userRequests = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_profile);

        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        // Initialize views
        profileNameTextView = findViewById(R.id.profile_name);
        userRequestsRecyclerView = findViewById(R.id.userRequestsRecyclerView);

        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView favoriteIcon = findViewById(R.id.favorites_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        ImageView settingsIcon = findViewById(R.id.settings_icon);

        // Set navigation listeners
        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        favoriteIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));
        settingsIcon.setOnClickListener(view -> navigateTo(SettingsActivity.class));

        // Fetch user data
        fetchUserData();

        // Setup and load user requests
        setupRequestsRecyclerView();
        loadUserRequests();
    }

    private void fetchUserData() {
        if (currentUser != null) {
            String userId = currentUser.getUid();
            firestore.collection("Users").document(userId).get()
                    .addOnSuccessListener(snapshot -> {
                        Boolean isVendor = snapshot.getBoolean("isVendor");
                        if (isVendor != null && isVendor) {
                            navigateToVendorStorefront(userId);
                        } else if (snapshot.exists() && Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))) {
                            DocumentReference userDocRef = firestore.collection("Users").document(userId);
                            userDocRef.get().addOnCompleteListener(task -> {
                                if (task.isSuccessful()) {
                                    DocumentSnapshot snapshotTwo = task.getResult();
                                    if (snapshotTwo.exists()) {
                                        String firstName = snapshotTwo.getString("firstName");
                                        String lastName = snapshotTwo.getString("lastName");
                                        firstName = (firstName != null) ? firstName : "";
                                        lastName = (lastName != null) ? lastName : "";
                                        String fullName = getString(R.string.profile_name_format, firstName, lastName);
                                        profileNameTextView.setText(fullName);
                                    } else {
                                        showToast("User data not found.");
                                    }
                                } else {
                                    showToast("Error: " + task.getException().getMessage());
                                }
                            });
                        } else {
                            showToast("User data not found.");
                        }
                    })
                    .addOnFailureListener(e -> showToast("Failed to check vendor status."));
        }
    }

    private void setupRequestsRecyclerView() {
        userAdapter = new CommissionUserAdapter(this, userRequests);
        userAdapter.setOnOrderCanceledListener(userAdapter::notifyDataSetChanged);

        userRequestsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        userRequestsRecyclerView.setAdapter(userAdapter);
    }

    private void loadUserRequests() {
        firestore.collection("CommissionRequests")
                .get()
                .addOnSuccessListener(snapshots -> {
                    userRequests.clear();
                    if (snapshots != null && !snapshots.isEmpty()) {
                        for (DocumentSnapshot doc : snapshots) {
                            CommissionRequest req = doc.toObject(CommissionRequest.class);
                            if (req != null) {
                                req.setDocumentId(doc.getId());
                                userRequests.add(req);
                            }
                        }
                        userAdapter.notifyDataSetChanged();
                    }
                })
                .addOnFailureListener(e -> showToast("Failed to load requests."));
    }

    private void navigateTo(Class<?> targetActivity) {
        startActivity(new Intent(ProfileActivity.this, targetActivity));
    }

    private void navigateToVendorStorefront(String vendorID) {
        Intent intent = new Intent(ProfileActivity.this, StorefrontActivity.class);
        intent.putExtra("vendorID", vendorID);
        startActivity(intent);
    }

    private void showToast(String message) {
        Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
    }
}

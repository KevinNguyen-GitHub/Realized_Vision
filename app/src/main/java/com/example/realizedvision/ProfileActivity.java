package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

public class ProfileActivity extends AppCompatActivity {

    private TextView profileNameTextView, addressTextView;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private String viewedUserId;

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
        addressTextView = findViewById(R.id.profile_address);

        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView favoriteIcon = findViewById(R.id.favorites_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        ImageView settingsIcon = findViewById(R.id.settings_icon);

        // Set navigation
        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        favoriteIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));
        settingsIcon.setOnClickListener(view -> navigateTo(SettingsActivity.class));

        // Check if viewing another user's profile
        viewedUserId = getIntent().getStringExtra("viewedUserId");
        if (viewedUserId == null) {
            viewedUserId = (currentUser != null) ? currentUser.getUid() : null;
        }


        // Fetch user data
        fetchProfileData(viewedUserId);
    }
    private void fetchProfileData(String userId) {
        if (userId != null) {
            DocumentReference userDocRef = firestore.collection("Users").document(userId);

            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot snapshot = task.getResult();
                    if (snapshot.exists()) {
                        String firstName = snapshot.getString("firstName");
                        String lastName = snapshot.getString("lastName");
                        String userAddress = snapshot.getString("address");

                        profileNameTextView.setText((firstName != null ? firstName : "") + " " + (lastName != null ? lastName : ""));
                        if (userAddress != null && !userAddress.isEmpty()) {
                            addressTextView.setText(userAddress);
                            addressTextView.setVisibility(View.VISIBLE);

                            // Make address clickable
                            addressTextView.setOnClickListener(v -> {
                                Intent intent = new Intent(ProfileActivity.this, MapActivity.class);
                                intent.putExtra("selectedAddress", userAddress); //userAddress
                                startActivity(intent);
                            });
                        } else {
                            addressTextView.setText("");
                        }
                    } else {
                        Toast.makeText(ProfileActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                } else {
                    Toast.makeText(ProfileActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }

    // Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(ProfileActivity.this, targetActivity);
        startActivity(intent);
    }
}





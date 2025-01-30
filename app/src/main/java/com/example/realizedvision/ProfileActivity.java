package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.DataSnapshot;
import com.google.firebase.database.DatabaseError;
import com.google.firebase.database.ValueEventListener;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

public class ProfileActivity extends AppCompatActivity {

    private TextView profileNameTextView;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;

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

        // Fetch user data
        fetchUserData();
    }

    /*private void fetchUserData() {
        if (currentUser != null) {
            String userId = currentUser.getUid();

            databaseReference.child(userId).addListenerForSingleValueEvent(new ValueEventListener() {
                @Override
                public void onDataChange(DataSnapshot snapshot) {

                    if (snapshot.exists() && snapshot.child("isVendor").getValue().equals(false)) {
                        String firstName = snapshot.child("firstName").getValue(String.class);
                        String lastName = snapshot.child("lastName").getValue(String.class);

                        // Handle null values
                        firstName = (firstName != null) ? firstName : "";
                        lastName = (lastName != null) ? lastName : "";

                        // Use resource string with placeholders
                        String fullName = getString(R.string.profile_name_format, firstName, lastName);
                        profileNameTextView.setText(fullName);

                    } else if (snapshot.exists() && snapshot.child("isVendor").getValue().equals(true)) {
                        String displayName = snapshot.child("companyInfo").child("companyName").getValue(String.class);

                        // Handle null values
                        displayName = (displayName != null) ? displayName : "";

                        profileNameTextView.setText(displayName);

                    } else {
                        Toast.makeText(ProfileActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                }

                @Override
                public void onCancelled(DatabaseError error) {
                    Toast.makeText(ProfileActivity.this, "Error: " + error.getMessage(), Toast.LENGTH_LONG).show();
                }
            });
        }
    }*/

    private void fetchUserData() {
        if (currentUser != null) {
            String userId = currentUser.getUid();

            DocumentReference userDocRef = firestore.collection("Users").document(userId);

            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot snapshot = task.getResult();

                    if (snapshot.exists() && Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))) {
                        DocumentReference companyDocRef = firestore.collection("Users")
                                .document(userId)
                                .collection("Company")
                                .document("Info");

                        companyDocRef.get().addOnCompleteListener(companyTask -> {
                            if (companyTask.isSuccessful()){
                                DocumentSnapshot companySnapshot = companyTask.getResult();

                                if (companySnapshot.exists()) {
                                    String displayName =  companySnapshot.getString("companyName");
                                    // Handle null values
                                    displayName = (displayName != null) ? displayName : "";

                                    profileNameTextView.setText(displayName);
                                } else {
                                    Toast.makeText(ProfileActivity.this, "Company info not found.", Toast.LENGTH_SHORT).show();
                                }
                            } else {
                                Toast.makeText(ProfileActivity.this, "Error: " + companyTask.getException().getMessage(), Toast.LENGTH_SHORT).show();
                            }
                        });
                    } else if (snapshot.exists() && Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))) {
                        String firstName = snapshot.getString("firstName");
                        String lastName = snapshot.getString("lastName");

                        // Handle null values
                        firstName = (firstName != null) ? firstName : "";
                        lastName = (lastName != null) ? lastName : "";

                        // Use resource string with placeholders
                        String fullName = getString(R.string.profile_name_format, firstName, lastName);
                        profileNameTextView.setText(fullName);
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





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


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_settings);

        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }



        Button termsAndConditionsButton = findViewById(R.id.tcButton);
        Button orderHistoryButton = findViewById(R.id.historyButton);
        Button changeUsernameButton = findViewById(R.id.changeUsernameButton);
        Button changePasswordButton = findViewById(R.id.changePasswordButton);
        Button notificationsButton = findViewById(R.id.notificationsButton);
        Button vendorInfoButton = findViewById(R.id.upgradeButton);
        ImageButton backButton = findViewById(R.id.backButtonChangePass);
        Button logoutButton = findViewById(R.id.btn_logout);



        termsAndConditionsButton.setOnClickListener(view -> navigateTo(TermsAndConditionsActivity.class));
        orderHistoryButton.setOnClickListener(view -> navigateTo(OrderHistoryActivity.class));
        changeUsernameButton.setOnClickListener(view -> navigateTo(ChangeNameActivity.class));
        changePasswordButton.setOnClickListener(view -> navigateTo(ChangePasswordActivity.class));
        notificationsButton.setOnClickListener(view -> navigateTo(NotificationsActivity.class));
        vendorInfoButton.setOnClickListener(view -> navigateTo(VendorInfoActivity.class));

        backButton.setOnClickListener(view -> {
            if (currentUser != null) {
                String userId = currentUser.getUid();

                DocumentReference userDocRef = firestore.collection("Users").document(userId);

                userDocRef.get().addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        DocumentSnapshot snapshot = task.getResult();

                        if (snapshot.exists() && Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))) {
                            navigateTo(StorefrontActivity.class);
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

        logoutButton.setOnClickListener(view -> navigateTo(LoginActivity.class));
    }

    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(SettingsActivity.this, targetActivity);
        startActivity(intent);
    }
}

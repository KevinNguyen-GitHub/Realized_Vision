package com.example.realizedvision;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.AuthCredential;
import com.google.firebase.auth.EmailAuthProvider;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;

public class ChangeNameActivity extends AppCompatActivity {

    private EditText editTextFirstName, editTextLastName, editTextCurrentPassword;
    private Button saveButton;
    private FirebaseUser currentUser;
    private DatabaseReference userDatabaseRef;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_changename);

        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        userDatabaseRef = FirebaseDatabase.getInstance().getReference("Users");

        // Initialize UI elements
        editTextFirstName = findViewById(R.id.firstName);
        editTextLastName = findViewById(R.id.lastName);
        editTextCurrentPassword = findViewById(R.id.currentPassword);
        saveButton = findViewById(R.id.saveButton);
        ImageButton backButton = findViewById(R.id.backButtonChangeName);

        // Back button logic
        backButton.setOnClickListener(v -> finish());

        // Save button logic
        saveButton.setOnClickListener(v -> updateUserName());
    }

    private void updateUserName() {
        String newFirstName = editTextFirstName.getText().toString().trim();
        String newLastName = editTextLastName.getText().toString().trim();
        String currentPassword = editTextCurrentPassword.getText().toString().trim();

        if (TextUtils.isEmpty(newFirstName) || TextUtils.isEmpty(newLastName)) {
            Toast.makeText(this, "Please fill in both name fields.", Toast.LENGTH_SHORT).show();
            return;
        }

        if (TextUtils.isEmpty(currentPassword)) {
            Toast.makeText(this, "Please enter your current password for verification.", Toast.LENGTH_SHORT).show();
            return;
        }

        // Re-authenticate user with current password
        AuthCredential credential = EmailAuthProvider.getCredential(currentUser.getEmail(), currentPassword);
        currentUser.reauthenticate(credential)
                .addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        // Update name in Firebase Realtime Database
                        String userId = currentUser.getUid();
                        userDatabaseRef.child(userId).child("firstName").setValue(newFirstName);
                        userDatabaseRef.child(userId).child("lastName").setValue(newLastName)
                                .addOnCompleteListener(updateTask -> {
                                    if (updateTask.isSuccessful()) {
                                        Toast.makeText(ChangeNameActivity.this, "Name updated successfully.", Toast.LENGTH_SHORT).show();
                                        finish(); // Go back to previous screen
                                    } else {
                                        Toast.makeText(ChangeNameActivity.this, "Failed to update name.", Toast.LENGTH_SHORT).show();
                                    }
                                });
                    } else {
                        Toast.makeText(ChangeNameActivity.this, "Authentication failed. Please check your password.", Toast.LENGTH_SHORT).show();
                    }
                });
    }
}

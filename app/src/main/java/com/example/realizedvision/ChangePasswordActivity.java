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
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;

public class ChangePasswordActivity extends AppCompatActivity {

    private EditText editTextCurrentPassword, editTextNewPassword, editTextConfirmPassword;
    private Button buttonSave;
    private FirebaseAuth mAuth;
    private FirebaseUser currentUser;
    private NotificationHelper notificationHelper;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_changepassword);

        // Initialize Firebase Auth
        mAuth = FirebaseAuth.getInstance();
        currentUser = mAuth.getCurrentUser();

        // Initialize views
        editTextCurrentPassword = findViewById(R.id.currentPassword);
        editTextNewPassword = findViewById(R.id.newPassword);
        editTextConfirmPassword = findViewById(R.id.confirmPassword);
        buttonSave = findViewById(R.id.saveButton);
        notificationHelper = new NotificationHelper(this);

        ImageButton backButton = findViewById(R.id.backButtonChangePass);
        backButton.setOnClickListener(v -> finish());

        // Save button logic
        buttonSave.setOnClickListener(v -> {
            String currentPassword = editTextCurrentPassword.getText().toString().trim();
            String newPassword = editTextNewPassword.getText().toString().trim();
            String confirmPassword = editTextConfirmPassword.getText().toString().trim();
            if (validateInputs(currentPassword, newPassword, confirmPassword)) {
                changePassword(currentPassword, newPassword);
            }
        });
    }

    private boolean validateInputs(String currentPassword, String newPassword, String confirmPassword) {
        if (TextUtils.isEmpty(currentPassword)) { //trim for extra spaces
            Toast.makeText(this, "Please enter your current password.", Toast.LENGTH_SHORT).show();
            return false;
        }
        if (TextUtils.isEmpty(newPassword) || TextUtils.isEmpty(confirmPassword)) {
            Toast.makeText(this, "Please fill in all password fields.", Toast.LENGTH_SHORT).show();
            return false;
        }
        if (!newPassword.equals(confirmPassword)) { //password match check
            Toast.makeText(this, "New passwords do not match.", Toast.LENGTH_SHORT).show();
            return false;
        }
        if (newPassword.length() < 6) {
            Toast.makeText(this, "New password must be at least 6 characters long.", Toast.LENGTH_SHORT).show();
            return false;
        }
        return true;
    }

    private void changePassword(String currentPassword, String newPassword) {
        if (currentUser == null) {
            Toast.makeText(this, "User not authenticated. Please log in again.", Toast.LENGTH_SHORT).show();
            return;
        }

        // Re-authenticate the user with their current password
        AuthCredential credential = EmailAuthProvider.getCredential(currentUser.getEmail(), currentPassword);
        currentUser.reauthenticate(credential)
                .addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        // Update the password
                        currentUser.updatePassword(newPassword)
                                .addOnCompleteListener(task1 -> {
                                    if (task1.isSuccessful()) {
                                        Toast.makeText(this, "Password updated successfully!", Toast.LENGTH_SHORT).show();
                                        notificationHelper.checkAndSendNotification(
                                                "email_security",
                                                "Password Changed",
                                                "Your password was successfully updated. You can now log in to your account."
                                        );
                                        notificationHelper.checkAndSendNotification(
                                                "app_security",
                                                "Security Alert",
                                                "Password changed"
                                        );
                                        finish();
                                    } else {
                                        Toast.makeText(this, "Password update failed. Try again.", Toast.LENGTH_SHORT).show();
                                    }
                                });
                    } else {
                        Toast.makeText(this, "Current password is incorrect.", Toast.LENGTH_SHORT).show();
                    }
                });
    }
}




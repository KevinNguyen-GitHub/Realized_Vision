package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.android.gms.tasks.Task;
import com.google.firebase.auth.AuthResult;
import com.google.firebase.auth.FirebaseAuth;

public class LoginActivity extends AppCompatActivity {
    private EditText email, password;
    private Button login;
    private CheckBox rememberMe;
    private TextView forgotPassword, createAccount;
    private FirebaseAuth mAuth;  // Firebase Authentication instance

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        // Initialize UI components
        email = findViewById(R.id.emailEditText);
        password = findViewById(R.id.passwordEditText);
        login = findViewById(R.id.loginButton);
        rememberMe = findViewById(R.id.rememberMe);
        forgotPassword = findViewById(R.id.forgotPassword);
        createAccount = findViewById(R.id.createAccount);

        // Initialize Firebase Authentication
        mAuth = FirebaseAuth.getInstance();

        // Set click listener for the login button
        login.setOnClickListener(view -> {
            String emailText = email.getText().toString().trim();
            String passwordText = password.getText().toString().trim();

            // Validate inputs
            if (TextUtils.isEmpty(emailText)) {
                email.setError("Email is required.");
                return;
            }
            if (TextUtils.isEmpty(passwordText)) {
                password.setError("Password is required.");
                return;
            }


            // Attempt to sign in with Firebase Authentication
            mAuth.signInWithEmailAndPassword(emailText, passwordText)
                    .addOnCompleteListener((Task<AuthResult> task) -> {
                        if (task.isSuccessful()) {
                            // Sign in success, show a toast and navigate to home screen (or any other activity)
                            Toast.makeText(LoginActivity.this, "Login successful", Toast.LENGTH_SHORT).show();

                            // Example: Remember user session if the "rememberMe" checkbox is checked
                            if (rememberMe.isChecked()) {
                                // Save the user session data to SharedPreferences or similar storage
                                // (Implementation depends on your app's requirements)
                            }

                            Intent intent = new Intent(LoginActivity.this, MainActivity.class);
                            startActivity(intent);
                            finish();
                        } else {
                            // If sign in fails, display a message to the user.
                            Toast.makeText(LoginActivity.this,
                                    "Error: " + task.getException().getMessage(),
                                    Toast.LENGTH_LONG).show();
                        }
                    });
        });

//        // Set click listener for the "Forgot Password" text
//        forgotPassword.setOnClickListener(view -> {
//            // Navigate to the ForgotPasswordActivity (ensure this activity is created)
//            Intent intent = new Intent(LoginActivity.this, ForgotPasswordActivity.class);
//            startActivity(intent);
//        });

        // Set click listener for the "Create Account" text
        createAccount.setOnClickListener(view -> {
            // Navigate to the RegisterActivity (ensure this activity is created)
            Intent intent = new Intent(LoginActivity.this, SignUpActivity.class);
            startActivity(intent);
        });
    }
}

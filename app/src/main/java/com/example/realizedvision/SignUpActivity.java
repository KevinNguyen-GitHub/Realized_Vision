package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.HashMap;
import java.util.Map;

public class SignUpActivity extends AppCompatActivity {

    private EditText etFirstName, etLastName, etEmail, etPassword, etPhoneNumber;
    private Button btnSignUp;
    private FirebaseAuth auth;
    private FirebaseFirestore firestore;
    private TextView textViewLogin;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_signup);
        NotificationHelper notificationHelper = new NotificationHelper(this);

        etFirstName = findViewById(R.id.firstNameEditText);
        etLastName = findViewById(R.id.lastNameEditText);
        etEmail = findViewById(R.id.emailEditText);
        etPassword = findViewById(R.id.passwordEditText);
        etPhoneNumber = findViewById(R.id.phoneNumberEditText);
        btnSignUp = findViewById(R.id.signUpButton);

        auth = FirebaseAuth.getInstance();
        firestore = FirebaseFirestore.getInstance();

        textViewLogin = findViewById(R.id.login_link);

        textViewLogin.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(getApplicationContext(), LoginActivity.class);
                startActivity(intent);
                finish();
            }
        });

        btnSignUp.setOnClickListener(v -> {
            String firstName = etFirstName.getText().toString().trim();
            String lastName = etLastName.getText().toString().trim();
            String email = etEmail.getText().toString().trim();
            String password = etPassword.getText().toString().trim();
            String phoneNumber = etPhoneNumber.getText().toString().trim();

            if (firstName.isEmpty() || lastName.isEmpty() || email.isEmpty() || password.isEmpty()) {
                Toast.makeText(SignUpActivity.this, "All fields are required.", Toast.LENGTH_SHORT).show();
                return;
            }

            if (phoneNumber.isEmpty() || phoneNumber.length() < 10) {
                Toast.makeText(SignUpActivity.this, "Enter a valid phone number.", Toast.LENGTH_SHORT).show();
                return;
            }

            auth.createUserWithEmailAndPassword(email, password)
                    .addOnCompleteListener(task -> {
                        if (task.isSuccessful() && auth.getCurrentUser() != null) {
                            String userId = auth.getCurrentUser().getUid();

                            Map<String, Object> userMap = new HashMap<>();
                            userMap.put("firstName", firstName);
                            userMap.put("lastName", lastName);
                            userMap.put("email", email);
                            userMap.put("phoneNumber", phoneNumber);
                            userMap.put("isVendor", false);

                            firestore.collection("Users")
                                    .document(userId)
                                    .set(userMap)
                                    .addOnCompleteListener(task1 -> {
                                        if (task1.isSuccessful()) {
                                            notificationHelper.checkAndSendNotification(
                                                    "email_purchases",
                                                    "Welcome to Realized Vision!",
                                                    "Welcome " + firstName + ", your account was successfully created"
                                            );
                                            Log.d("Firestore", "User data saved successfully");
                                            Intent intent = new Intent(SignUpActivity.this, MainActivity.class);
                                            intent.putExtra("firstName", firstName);
                                            intent.putExtra("lastName", lastName);
                                            startActivity(intent);
                                            finish();
                                        } else {
                                            Log.e("Firestore", "Failed to save user data", task1.getException());
                                            Toast.makeText(SignUpActivity.this, "Failed to save user data.", Toast.LENGTH_SHORT).show();
                                        }
                                    });
                        } else {
                            Log.e("FirebaseAuth", "Signup failed", task.getException());
                            Toast.makeText(SignUpActivity.this, "Signup Failed: " + (task.getException() != null ? task.getException().getMessage() : ""), Toast.LENGTH_SHORT).show();
                        }
                    });
        });
    }
}

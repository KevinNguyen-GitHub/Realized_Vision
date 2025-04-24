package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.EditText;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.HashMap;
import java.util.Map;

/**
 * E-mail / password signup for customer accounts.
 *
 *  • Validates all fields locally.
 *  • Creates Firebase-Auth user, then stores a user-profile doc at
 *    <pre>Users/{uid}</pre>.
 *  • The account is <i>not</i> a vendor by default (<code>isVendor = false</code>).
 *  • On success, boots straight into {@link MainActivity}.
 */
public class SignUpActivity extends AppCompatActivity {

    private EditText etFirst, etLast, etEmail, etPass, etPhone;
    private FirebaseAuth     auth;
    private FirebaseFirestore db;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override
    protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_signup);

        auth = FirebaseAuth.getInstance();
        db   = FirebaseFirestore.getInstance();

        initViews();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void initViews() {
        etFirst = findViewById(R.id.firstNameEditText);
        etLast  = findViewById(R.id.lastNameEditText);
        etEmail = findViewById(R.id.emailEditText);
        etPass  = findViewById(R.id.passwordEditText);
        etPhone = findViewById(R.id.phoneNumberEditText);

        findViewById(R.id.login_link)
                .setOnClickListener(v -> startActivity(
                        new Intent(this, LoginActivity.class)));

        findViewById(R.id.signUpButton)
                .setOnClickListener(v -> attemptSignup());
    }

    /* ─────────────────────── core logic ───────────────────────── */
    private void attemptSignup() {

        String first = etFirst.getText().toString().trim();
        String last  = etLast .getText().toString().trim();
        String mail  = etEmail.getText().toString().trim();
        String pass  = etPass .getText().toString().trim();
        String phone = etPhone.getText().toString().trim();

        if (first.isEmpty() || last.isEmpty() || mail.isEmpty()
                || pass.isEmpty() || phone.isEmpty()) {
            toast("All fields are required"); return;
        }
        if (phone.length() < 10) {
            toast("Enter a valid phone number"); return;
        }

        auth.createUserWithEmailAndPassword(mail, pass)
                .addOnSuccessListener(res -> saveProfile(first, last, mail, phone))
                .addOnFailureListener(e   -> toast("Signup failed: " + e.getMessage()));
    }

    private void saveProfile(String f, String l, String mail, String phone) {
        String uid = auth.getCurrentUser().getUid();

        Map<String,Object> doc = new HashMap<>();
        doc.put("firstName",  f);
        doc.put("lastName",   l);
        doc.put("email",      mail);
        doc.put("phoneNumber",phone);
        doc.put("isVendor",   false);

        db.collection("Users").document(uid).set(doc)
                .addOnSuccessListener(v -> {
                    toast("Account created!");
                    startActivity(new Intent(this, MainActivity.class));
                    finish();
                })
                .addOnFailureListener(e ->
                        toast("Failed to save profile: " + e.getMessage()));
    }

    /* ───────────────────── helper wrapper ─────────────────────── */
    private void toast(String m){
        Toast.makeText(this, m, Toast.LENGTH_SHORT).show();
    }
}

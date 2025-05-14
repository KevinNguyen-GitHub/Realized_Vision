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

import com.google.firebase.auth.FirebaseAuth;

/**
 * Simple e-mail / password sign-in screen backed by Firebase Auth.
 *
 * Expects:
 *   • “Forgot password” flow (TODO: uncomment when activity exists)
 *   • “Create account” flow that routes to {@link SignUpActivity}
 */
public class LoginActivity extends AppCompatActivity {

    /* ─────────────────────────── UI refs ─────────────────────────── */
    private EditText  etEmail, etPassword;
    private Button    btnLogin;
    private CheckBox  cbRemember;
    private TextView  tvForgot,  tvCreate;

    /* ─────────────────────────── Firebase ────────────────────────── */
    private FirebaseAuth auth;

    /* ───────────────────────── lifecycle ─────────────────────────── */
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        initViews();
        auth = FirebaseAuth.getInstance();
        bindListeners();
    }

    /* ─────────────────────── view wiring ─────────────────────────── */
    private void initViews() {
        etEmail    = findViewById(R.id.emailEditText);
        etPassword = findViewById(R.id.passwordEditText);
        btnLogin   = findViewById(R.id.loginButton);
        cbRemember = findViewById(R.id.rememberMe);
        tvForgot   = findViewById(R.id.forgotPassword);
        tvCreate   = findViewById(R.id.createAccount);
    }

    private void bindListeners() {
        btnLogin.setOnClickListener(v -> attemptLogin());

        /* Uncomment when ForgotPasswordActivity is implemented
        tvForgot.setOnClickListener(v ->
                startActivity(new Intent(this, ForgotPasswordActivity.class)));
        */

        tvCreate.setOnClickListener(v ->
                startActivity(new Intent(this, SignUpActivity.class)));
    }

    /* ───────────────────── auth flow ─────────────────────────────── */
    private void attemptLogin() {
        String email    = etEmail.getText().toString().trim();
        String password = etPassword.getText().toString().trim();

        if (!validate(email, password)) return;

        btnLogin.setEnabled(false);                             // debounce
        auth.signInWithEmailAndPassword(email, password)
                .addOnCompleteListener(task -> {
                    btnLogin.setEnabled(true);
                    if (task.isSuccessful()) {
                        toast("Login successful!");
                        // TODO persist session if (cbRemember.isChecked()) …
                        startActivity(new Intent(this, MainActivity.class));
                        finish();
                    } else {
                        toast("Error: " + task.getException().getMessage());
                    }
                });
    }

    /* ───────────────────── validation ────────────────────────────── */
    private boolean validate(String email, String pass) {
        boolean ok = true;
        if (TextUtils.isEmpty(email)) {
            etEmail.setError("Email is required.");
            ok = false;
        }
        if (TextUtils.isEmpty(pass)) {
            etPassword.setError("Password is required.");
            ok = false;
        }
        return ok;
    }

    /* ───────────────────── helpers ──────────────────────────────── */
    private void toast(String msg) {
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show();
    }
}

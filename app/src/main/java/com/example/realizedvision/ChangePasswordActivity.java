package com.example.realizedvision;

import android.os.Bundle;
import android.text.TextUtils;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.AuthCredential;
import com.google.firebase.auth.EmailAuthProvider;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;

/**
 * Allows the signed-in user to change their password after a quick
 * re-authentication check with their current password.
 */
public class ChangePasswordActivity extends AppCompatActivity {

    private static final int MIN_LEN = 6;

    private EditText etCurrent, etNew, etConfirm;
    private Button   btnSave;

    private FirebaseUser user;

    // ───────────────────────── lifecycle ─────────────────────────
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_changepassword);

        user = FirebaseAuth.getInstance().getCurrentUser();
        if (user == null) {                 // signed-out guard
            toast("User not authenticated. Please log in again.");
            finish(); return;
        }

        initViews();
    }

    // ───────────────────── view wiring / listeners ───────────────────────
    private void initViews() {
        etCurrent = findViewById(R.id.currentPassword);
        etNew     = findViewById(R.id.newPassword);
        etConfirm = findViewById(R.id.confirmPassword);
        btnSave   = findViewById(R.id.saveButton);

        ImageButton back = findViewById(R.id.backButtonChangePass);
        back.setOnClickListener(v -> finish());

        btnSave.setOnClickListener(v -> {
            String curr = etCurrent.getText().toString().trim();
            String next = etNew.getText().toString().trim();
            String conf = etConfirm.getText().toString().trim();

            if (inputsValid(curr, next, conf)) changePassword(curr, next);
        });
    }

    // ────────────────────── validation helpers ───────────────────────────
    private boolean inputsValid(String curr, String next, String conf) {
        if (TextUtils.isEmpty(curr))                      { toast("Enter your current password."); return false; }
        if (TextUtils.isEmpty(next) || TextUtils.isEmpty(conf))
        { toast("Fill in all password fields.");  return false; }
        if (!next.equals(conf))                          { toast("New passwords do not match.");   return false; }
        if (next.length() < MIN_LEN)                     { toast("Password must be ≥ " + MIN_LEN + " characters."); return false; }
        return true;
    }

    // ────────────────────── change-password flow ─────────────────────────
    private void changePassword(String currentPwd, String newPwd) {
        btnSave.setEnabled(false);      // debounce taps

        AuthCredential cred = EmailAuthProvider.getCredential(
                String.valueOf(user.getEmail()), currentPwd);

        user.reauthenticate(cred).addOnCompleteListener(authTask -> {
            if (!authTask.isSuccessful()) {
                toast("Current password is incorrect.");
                btnSave.setEnabled(true);
                return;
            }

            user.updatePassword(newPwd).addOnCompleteListener(updTask -> {
                if (updTask.isSuccessful()) {
                    toast("Password updated successfully!");
                    finish();
                } else {
                    toast("Password update failed. Try again.");
                    btnSave.setEnabled(true);
                }
            });
        });
    }

    // ────────────────────────── toast helper ─────────────────────────────
    private void toast(String msg) { Toast.makeText(this, msg, Toast.LENGTH_SHORT).show(); }
}

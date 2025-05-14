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
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;

import java.util.HashMap;
import java.util.Map;

public class ChangeNameActivity extends AppCompatActivity {

    private EditText etFirst, etLast, etPassword;
    private Button   btnSave;

    private FirebaseUser      user;
    private DatabaseReference usersRef;

    // ───────────────────────────── lifecycle ─────────────────────────────
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_changename);

        user = FirebaseAuth.getInstance().getCurrentUser();
        if (user == null) {           // signed-out guard
            Toast.makeText(this, "User not signed in.", Toast.LENGTH_LONG).show();
            finish(); return;
        }
        usersRef = FirebaseDatabase.getInstance().getReference("Users");

        initViews();
    }

    // ────────────────────────────── view wiring ───────────────────────────
    private void initViews() {
        etFirst    = findViewById(R.id.firstName);
        etLast     = findViewById(R.id.lastName);
        etPassword = findViewById(R.id.currentPassword);
        btnSave    = findViewById(R.id.saveButton);

        ImageButton back = findViewById(R.id.backButtonChangeName);
        back.setOnClickListener(v -> finish());

        btnSave.setOnClickListener(v -> updateName());
    }

    // ───────────────────────────── core logic ─────────────────────────────
    private void updateName() {
        String first = etFirst.getText().toString().trim();
        String last  = etLast.getText().toString().trim();
        String pass  = etPassword.getText().toString().trim();

        if (TextUtils.isEmpty(first) || TextUtils.isEmpty(last)) {
            toast("Please fill in both name fields."); return;
        }
        if (TextUtils.isEmpty(pass)) {
            toast("Please enter your current password for verification."); return;
        }

        btnSave.setEnabled(false);                    // prevent double-clicks
        AuthCredential cred = EmailAuthProvider.getCredential(
                String.valueOf(user.getEmail()), pass);

        user.reauthenticate(cred).addOnCompleteListener(authTask -> {
            if (!authTask.isSuccessful()) {
                toast("Authentication failed. Check your password.");
                btnSave.setEnabled(true);
                return;
            }

            Map<String, Object> update = new HashMap<>();
            update.put("firstName", first);
            update.put("lastName",  last);

            usersRef.child(user.getUid()).updateChildren(update)
                    .addOnCompleteListener(dbTask -> {
                        if (dbTask.isSuccessful()) {
                            toast("Name updated successfully.");
                            finish();
                        } else {
                            toast("Failed to update name.");
                            btnSave.setEnabled(true);
                        }
                    });
        });
    }

    // ───────────────────────────── helpers ────────────────────────────────
    private void toast(String msg) {
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show();
    }
}

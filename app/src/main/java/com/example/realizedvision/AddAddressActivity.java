package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;

public class AddAddressActivity extends AppCompatActivity {

    private EditText etStreet, etCity, etState, etZip;
    private Button   btnSave;

    private final FirebaseFirestore firestore = FirebaseFirestore.getInstance();
    private final FirebaseAuth      auth      = FirebaseAuth.getInstance();
    private FirebaseUser            currentUser;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_add_address);

        currentUser = auth.getCurrentUser();
        if (currentUser == null) {
            Toast.makeText(this, "User not signed in.", Toast.LENGTH_LONG).show();
            finish(); return;
        }

        initViews();
        decideFlow(currentUser.getUid());
    }

    // ------------------------------------------------------------
    // View wiring
    // ------------------------------------------------------------
    private void initViews() {
        etStreet = findViewById(R.id.streetAddressEditText);
        etCity   = findViewById(R.id.cityEditText);
        etState  = findViewById(R.id.stateEditText);
        etZip    = findViewById(R.id.zipCodeEditText);
        btnSave  = findViewById(R.id.saveAddressButton);

        ImageButton back = findViewById(R.id.backButtonAddress);
        back.setOnClickListener(v -> navigateTo(SettingsActivity.class));
    }

    // ------------------------------------------------------------
    // Vendor check & click-listener
    // ------------------------------------------------------------
    private void decideFlow(String userId) {
        firestore.collection("Users")
                .document(userId)
                .get()
                .addOnSuccessListener(snapshot -> {
                    Boolean isVendor = snapshot.getBoolean("isVendor");
                    if (Boolean.TRUE.equals(isVendor)) {
                        showVendorAddressPopup();
                    } else {
                        btnSave.setOnClickListener(v -> saveAddress(userId));
                    }
                })
                .addOnFailureListener(e ->
                        Toast.makeText(this,
                                "Error checking vendor status.", Toast.LENGTH_SHORT).show());
    }

    // ------------------------------------------------------------
    // Dialog helpers
    // ------------------------------------------------------------
    private void showVendorAddressPopup() {
        new AlertDialog.Builder(this)
                .setTitle("Address Already Saved")
                .setMessage("Your address is already saved as a vendor.")
                .setPositiveButton("OK", (d, w) -> finish())
                .setCancelable(false)
                .show();
    }

    // ------------------------------------------------------------
    // Save flow
    // ------------------------------------------------------------
    private void saveAddress(String userId) {
        String street = etStreet.getText().toString().trim();
        String city   = etCity.getText().toString().trim();
        String state  = etState.getText().toString().trim();
        String zip    = etZip.getText().toString().trim();

        if (TextUtils.isEmpty(street) ||
                TextUtils.isEmpty(city)   ||
                TextUtils.isEmpty(state)  ||
                TextUtils.isEmpty(zip)) {
            Toast.makeText(this, "Please fill in all fields.", Toast.LENGTH_SHORT).show();
            return;
        }

        String fullAddress = street + ' ' + city + ' ' + state + ' ' + zip;

        firestore.collection("Users").document(userId)
                .update("address", fullAddress)
                .addOnSuccessListener(v -> {
                    Toast.makeText(this, "Address saved!", Toast.LENGTH_SHORT).show();
                    finish();
                })
                .addOnFailureListener(e ->
                        Toast.makeText(this,
                                "Failed to save address.", Toast.LENGTH_SHORT).show());
    }

    // ------------------------------------------------------------
    // Navigation helper
    // ------------------------------------------------------------
    private void navigateTo(Class<?> target) {
        startActivity(new Intent(this, target));
    }
}

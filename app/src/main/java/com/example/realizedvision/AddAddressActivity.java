package com.example.realizedvision;

import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

public class AddAddressActivity extends AppCompatActivity {

    private EditText etStreet, etCity, etState, etZip;
    private Button btnSave;
    private FirebaseFirestore firestore;
    private FirebaseAuth auth;
    private FirebaseUser currentUser;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_add_address);

        auth = FirebaseAuth.getInstance();
        firestore = FirebaseFirestore.getInstance();
        currentUser = auth.getCurrentUser();

        etStreet = findViewById(R.id.streetAddressEditText);
        etCity = findViewById(R.id.cityEditText);
        etState = findViewById(R.id.stateEditText);
        etZip = findViewById(R.id.zipCodeEditText);
        btnSave = findViewById(R.id.saveAddressButton);


        ImageButton backButton = findViewById(R.id.backButtonAddress);
        backButton.setOnClickListener(view -> navigateTo(SettingsActivity.class));

        if (currentUser != null) {
            String userId = currentUser.getUid();

            // Check if user is a vendor
            firestore.collection("Users").document(userId).get()
                    .addOnSuccessListener(snapshot -> {
                        Boolean isVendor = snapshot.getBoolean("isVendor");
                        if (isVendor != null && isVendor) {
                            showVendorAddressPopup();
                        } else {
                            btnSave.setOnClickListener(v -> saveAddress(userId));
                        }
                    })
                    .addOnFailureListener(e -> {
                        Toast.makeText(this, "Error checking vendor status.", Toast.LENGTH_SHORT).show();
                        finish();
                    });
        }
    }

    private void showVendorAddressPopup() {
        new AlertDialog.Builder(this)
                .setTitle("Address Already Saved")
                .setMessage("Your address is already saved as a vendor.")
                .setPositiveButton("OK", (dialog, which) -> finish())
                .setCancelable(false)
                .show();
    }

    private void saveAddress(String userId) {
        String street = etStreet.getText().toString().trim();
        String city = etCity.getText().toString().trim();
        String state = etState.getText().toString().trim();
        String zip = etZip.getText().toString().trim();

        if (street.isEmpty() || city.isEmpty() || state.isEmpty() || zip.isEmpty()) {
            Toast.makeText(this, "Please fill in all fields.", Toast.LENGTH_SHORT).show();
            return;
        }

        String fullAddress = street + " " + city + " " + state + " " + zip;

        firestore.collection("Users").document(userId)
                .update("address", fullAddress)
                .addOnSuccessListener(aVoid -> {
                    Toast.makeText(this, "Address saved!", Toast.LENGTH_SHORT).show();
                    finish();
                })
                .addOnFailureListener(e -> Toast.makeText(this, "Failed to save address.", Toast.LENGTH_SHORT).show());
    }


    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(AddAddressActivity.this, targetActivity);
        startActivity(intent);
    }
}


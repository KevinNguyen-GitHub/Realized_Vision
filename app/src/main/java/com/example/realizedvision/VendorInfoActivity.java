package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.HashMap;

public class VendorInfoActivity extends AppCompatActivity {

    private FirebaseUser currentUser;
    private FirebaseFirestore firestore;

    private EditText etCompanyName, etAddress, etYears;

    private TextView tvVendorStatus;
    private Button submitBtn;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendorinfo);

        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        fetchUserData();

        // Setting UI connections
        tvVendorStatus = findViewById(R.id.vendorStatus);
        etCompanyName = findViewById(R.id.companyName);
        etAddress = findViewById(R.id.address);
        etYears = findViewById(R.id.yearsInBusiness);
        submitBtn = findViewById(R.id.submitButton);

        // Back Button
        ImageButton backButton = findViewById(R.id.backButtonVendor);
        backButton.setOnClickListener(v -> finish());

        // Save button logic
        submitBtn.setOnClickListener(v -> updateUserInfo());
    }

    private void updateUserInfo(){
        String companyName = etCompanyName.getText().toString().trim();
        String address = etAddress.getText().toString().trim();
        int yearsInBusiness;

        try{
            yearsInBusiness = Integer.parseInt(etYears.getText().toString().trim());
        }
        catch (NumberFormatException e){
            Toast.makeText(this, "Please enter an integer for years.", Toast.LENGTH_SHORT).show();
            return;
        }

        if (TextUtils.isEmpty(companyName) || TextUtils.isEmpty(address)) {
            Toast.makeText(this, "Please fill in both name fields.", Toast.LENGTH_SHORT).show();
            return;
        }

        String userId = currentUser.getUid();
        firestore.collection("Users").document(userId)
                        .update("isVendor", true);

        HashMap<String, Object> userMap = new HashMap<>();
        userMap.put("companyName", companyName);
        userMap.put("address", address);
        userMap.put("years", yearsInBusiness);
        userMap.put("vendorID", userId);


        firestore.collection("Vendors").document(userId)
                        .set(userMap)
                        .addOnSuccessListener(new OnSuccessListener<Void>() {
                            @Override
                            public void onSuccess(Void unused) {
                                Log.d("Firestore", "Updated user status");
                                finish();
                            }
                        })
                        .addOnFailureListener(new OnFailureListener() {
                            @Override
                            public void onFailure(@NonNull Exception e) {
                                Log.w("Failed to update user status", e);
                            }
                        });
    }
    private void fetchUserData() {
        if (currentUser != null) {
            String userId = currentUser.getUid();

            DocumentReference userDocRef = firestore.collection("Users").document(userId);

            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot snapshot = task.getResult();

                    if (snapshot.exists() && Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))) {
                        etCompanyName.setVisibility(View.INVISIBLE);
                        etAddress.setVisibility(View.INVISIBLE);
                        etYears.setVisibility(View.INVISIBLE);

                        submitBtn.setVisibility(View.INVISIBLE);

                        tvVendorStatus.setVisibility(View.VISIBLE);
                    } else if (snapshot.exists() && Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))) {

                    } else {
                        Toast.makeText(VendorInfoActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                } else {
                    Toast.makeText(VendorInfoActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }
}

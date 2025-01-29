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

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.DataSnapshot;
import com.google.firebase.database.DatabaseError;
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;
import com.google.firebase.database.ValueEventListener;

import java.util.HashMap;

public class VendorInfoActivity extends AppCompatActivity {

    private FirebaseUser currentUser;
    private DatabaseReference userDatabaseRef;

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
            userDatabaseRef = FirebaseDatabase.getInstance().getReference("Users");
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
        userDatabaseRef.child(userId).child("isVendor").setValue(true);

        HashMap<String, Object> userMap = new HashMap<>();
        userMap.put("companyName", companyName);
        userMap.put("address", address);
        userMap.put("years", yearsInBusiness);

        userDatabaseRef.child(userId).child("companyInfo").setValue(userMap).addOnCompleteListener(updateTask -> {
            if (updateTask.isSuccessful()) {
                Toast.makeText(this, "User status updated.", Toast.LENGTH_SHORT).show();
                finish(); // Go back to previous screen
            } else {
                Toast.makeText(this, "Failed to update user status.", Toast.LENGTH_SHORT).show();
            }
        });
    }
    private void fetchUserData() {
        if (currentUser != null) {
            String userId = currentUser.getUid();

            userDatabaseRef.child(userId).addListenerForSingleValueEvent(new ValueEventListener() {
                @Override
                public void onDataChange(DataSnapshot snapshot) {

                    if (snapshot.exists() && snapshot.child("isVendor").getValue().equals(true)) {
                        etCompanyName.setVisibility(View.INVISIBLE);
                        etAddress.setVisibility(View.INVISIBLE);
                        etYears.setVisibility(View.INVISIBLE);

                        submitBtn.setVisibility(View.INVISIBLE);

                        tvVendorStatus.setVisibility(View.VISIBLE);

                    } else if (snapshot.exists() && snapshot.child("isVendor").getValue().equals(false)){

                    } else {
                        Toast.makeText(VendorInfoActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                }

                @Override
                public void onCancelled(DatabaseError error) {
                    Toast.makeText(VendorInfoActivity.this, "Error: " + error.getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }
}

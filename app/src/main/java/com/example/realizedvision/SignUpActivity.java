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

        etFirstName = findViewById(R.id.firstNameEditText);
        etLastName = findViewById(R.id.lastNameEditText);
        etEmail = findViewById(R.id.emailEditText);
        etPassword = findViewById(R.id.passwordEditText);
        etPhoneNumber = findViewById(R.id.phoneNumberEditText);
        btnSignUp = findViewById(R.id.signUpButton);
        textViewLogin = findViewById(R.id.login_link);

        auth = FirebaseAuth.getInstance();
        firestore = FirebaseFirestore.getInstance();

        textViewLogin.setOnClickListener(v -> {
            startActivity(new Intent(getApplicationContext(), LoginActivity.class));
            finish();
        });

        btnSignUp.setOnClickListener(v -> {
            String firstName = etFirstName.getText().toString().trim();
            String lastName = etLastName.getText().toString().trim();
            String email = etEmail.getText().toString().trim();
            String password = etPassword.getText().toString().trim();
            String phoneNumber = etPhoneNumber.getText().toString().trim();

            if (firstName.isEmpty() || lastName.isEmpty() || email.isEmpty() || password.isEmpty() || phoneNumber.isEmpty()) {
                Toast.makeText(SignUpActivity.this, "All fields are required.", Toast.LENGTH_SHORT).show();
                return;
            }

            if (phoneNumber.length() < 10) {
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

                            firestore.collection("Users").document(userId)
                                    .set(userMap)
                                    .addOnSuccessListener(aVoid -> {
                                        Log.d("Firestore", "User data saved successfully");
                                        Intent intent = new Intent(SignUpActivity.this, MainActivity.class);
                                        intent.putExtra("firstName", firstName);
                                        intent.putExtra("lastName", lastName);
                                        startActivity(intent);
                                        finish();
                                    })
                                    .addOnFailureListener(e -> {
                                        Log.e("Firestore", "Failed to save user data", e);
                                        Toast.makeText(SignUpActivity.this, "Failed to save user data.", Toast.LENGTH_SHORT).show();
                                    });
                        } else {
                            Toast.makeText(SignUpActivity.this, "Signup Failed: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                        }
                    });
        });
    }
}



//package com.example.realizedvision;
//
//import android.content.Intent;
//import android.net.Uri;
//import android.os.Bundle;
//import android.util.Log;
//import android.view.View;
//import android.widget.Button;
//import android.widget.EditText;
//import android.widget.TextView;
//import android.widget.Toast;
//import androidx.appcompat.app.AppCompatActivity;
//
//import com.android.volley.Request;
//import com.android.volley.RequestQueue;
//import com.android.volley.toolbox.JsonArrayRequest;
//import com.android.volley.toolbox.Volley;
//import com.google.firebase.auth.FirebaseAuth;
//import com.google.firebase.firestore.FirebaseFirestore;
//
//import org.json.JSONException;
//import org.json.JSONObject;
//
//import java.util.HashMap;
//import java.util.Map;
//
//public class SignUpActivity extends AppCompatActivity {
//
//    private EditText etFirstName, etLastName, etEmail, etPassword, etPhoneNumber;
//    private Button btnSignUp;
//    private FirebaseAuth auth;
//    private FirebaseFirestore firestore;
//    private TextView textViewLogin;
//
//
//    boolean validAddress = true;
//
//    private EditText etStreetAddress, etCity, etState, etZipCode;
//
//    @Override
//    protected void onCreate(Bundle savedInstanceState) {
//
//        super.onCreate(savedInstanceState);
//        setContentView(R.layout.activity_signup);
//
//        etFirstName = findViewById(R.id.firstNameEditText);
//        etLastName = findViewById(R.id.lastNameEditText);
//        etEmail = findViewById(R.id.emailEditText);
//        etPassword = findViewById(R.id.passwordEditText);
//        etPhoneNumber = findViewById(R.id.phoneNumberEditText);
//
//        // Address Fields
//        etStreetAddress = findViewById(R.id.streetAddressEditText);
//        etCity = findViewById(R.id.cityEditText);
//        etState = findViewById(R.id.stateEditText);
//        etZipCode = findViewById(R.id.zipCodeEditText);
//
//
//        btnSignUp = findViewById(R.id.signUpButton);
//
//        auth = FirebaseAuth.getInstance();
//        firestore = FirebaseFirestore.getInstance();
//
//        textViewLogin = findViewById(R.id.login_link);
//
//        textViewLogin.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                Intent intent = new Intent(getApplicationContext(), LoginActivity.class);
//                startActivity(intent);
//                finish();
//            }
//        });
//        btnSignUp.setOnClickListener(v -> {
//            String firstName = etFirstName.getText().toString().trim();
//            String lastName = etLastName.getText().toString().trim();
//            String email = etEmail.getText().toString().trim();
//            String password = etPassword.getText().toString().trim();
//            String phoneNumber = etPhoneNumber.getText().toString().trim();
//
//            // Get Address Inputs
//            String street = etStreetAddress.getText().toString().trim();
//            String city = etCity.getText().toString().trim();
//            String state = etState.getText().toString().trim();
//            String zipCode = etZipCode.getText().toString().trim();
//
//            // Concatenate Address
//            String fullAddress = street + " " + city + " " + state + " " + zipCode;
//
//            // Validate empty fields
//            if (firstName.isEmpty() || lastName.isEmpty() || email.isEmpty() || password.isEmpty() ||
//                    phoneNumber.isEmpty() || street.isEmpty() || city.isEmpty() || state.isEmpty() || zipCode.isEmpty()) {
//                Toast.makeText(SignUpActivity.this, "All fields are required.", Toast.LENGTH_SHORT).show();
//                return;
//            }
//
//            if (phoneNumber.length() < 10) {
//                Toast.makeText(SignUpActivity.this, "Enter a valid phone number.", Toast.LENGTH_SHORT).show();
//                return;
//            }
//
//            //Perform address validation BEFORE proceeding with Firebase
//            geocodeAddress(fullAddress, (latitude, longitude) -> {
//                if (latitude == 0 && longitude == 0) {
//                    Toast.makeText(SignUpActivity.this, "Invalid address. Please enter a valid location.", Toast.LENGTH_LONG).show();
//                    return; // Stop signup process if address is invalid
//                }
//
//                //If address is valid, proceed with Firebase authentication
//                auth.createUserWithEmailAndPassword(email, password)
//                        .addOnCompleteListener(task -> {
//                            if (task.isSuccessful() && auth.getCurrentUser() != null) {
//                                String userId = auth.getCurrentUser().getUid();
//                                Map<String, Object> userMap = new HashMap<>();
//                                userMap.put("firstName", firstName);
//                                userMap.put("lastName", lastName);
//                                userMap.put("email", email);
//                                userMap.put("phoneNumber", phoneNumber);
//                                userMap.put("isVendor", false);
//                                userMap.put("address", fullAddress);  // Store the concatenated address into database
//
//                                firestore.collection("Users")
//                                        .document(userId)
//                                        .set(userMap)
//                                        .addOnCompleteListener(task1 -> {
//                                            if (task1.isSuccessful()) {
//                                                Log.d("Firestore", "User data saved successfully");
//                                                Intent intent = new Intent(SignUpActivity.this, MainActivity.class);
//                                                intent.putExtra("firstName", firstName);
//                                                intent.putExtra("lastName", lastName);
//                                                startActivity(intent);
//                                                finish();
//                                            } else {
//                                                Log.e("Firestore", "Failed to save user data", task1.getException());
//                                                Toast.makeText(SignUpActivity.this, "Failed to save user data.", Toast.LENGTH_SHORT).show();
//                                            }
//                                        });
//                            } else {
//                                Log.e("FirebaseAuth", "Signup failed", task.getException());
//                                Toast.makeText(SignUpActivity.this, "Signup Failed: " + (task.getException() != null ? task.getException().getMessage() : ""), Toast.LENGTH_SHORT).show();
//                            }
//                        });
//            });
//        });
//
//    }
//
//
//    private void geocodeAddress(String address, MapActivity.OnGeocodeCompleteListener listener) {
//        String url = "https://nominatim.openstreetmap.org/search?q=" +
//                Uri.encode(address) + "&format=json";
//
//        RequestQueue queue = Volley.newRequestQueue(this);
//
//        JsonArrayRequest request = new JsonArrayRequest(Request.Method.GET, url, null,
//                response -> {
//                    try {
//                        if (response.length() > 0) {
//                            JSONObject place = response.getJSONObject(0);
//                            double lat = place.getDouble("lat");
//                            double lon = place.getDouble("lon");
//
//                            listener.onGeocodeSuccess(lat, lon); // Callback to return data
//
//                        } else {
//                            Toast.makeText(this, "Location not found: " + address, Toast.LENGTH_SHORT).show();
//                            validAddress = false;
//                        }
//                    } catch (JSONException e) {
//                        e.printStackTrace();
//                        Toast.makeText(this, "Error fetching location for " + address, Toast.LENGTH_SHORT).show();
//                        validAddress = false;
//                    }
//                },
//                error -> Toast.makeText(this, "Geocoding failed for " + address, Toast.LENGTH_SHORT).show());
//
//        queue.add(request);
//    }
//    interface OnGeocodeCompleteListener {
//        void onGeocodeSuccess(double latitude, double longitude);
//    }
//
//
//}

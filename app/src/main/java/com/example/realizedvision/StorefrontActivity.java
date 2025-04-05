
package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.QuerySnapshot;
import com.google.firebase.firestore.Source;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class StorefrontActivity extends AppCompatActivity {

    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private TextView profileNameTextView;
    private String vendorId;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendor_profile);

        profileNameTextView = findViewById(R.id.profile_name);
        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }
        // Initialize RecyclerView
        recyclerView = findViewById(R.id.recyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));

        // Initialize item list and adapter
        itemList = new ArrayList<>();
        itemAdapter = new ItemAdapter(this, itemList, false);
        recyclerView.setAdapter(itemAdapter);



        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        ImageView settingsIcon = findViewById(R.id.settings_icon);
        ImageView calendarIcon = findViewById(R.id.calendar_icon);

        homeIcon.setOnClickListener(view -> checkUserType());
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        calendarIcon.setOnClickListener(view -> navigateTo(ViewCalendarActivity.class));

        profileIcon.setOnClickListener(view -> navigateTo(StorefrontActivity.class));
        settingsIcon.setOnClickListener(view -> navigateTo(SettingsActivity.class));


        Intent intent  = getIntent();
        if(intent != null){
            String vendorID = intent.getStringExtra("vendorID");

            if(vendorID != null){
                vendorId = vendorID;
            }else{
                Log.e("Storefront Activity", "vendorID extra not found in intent");
            }
        }else{
            Log.e("Storefront Activity", "Intent is null");
        }

        loadStorefrontItems(vendorId);
        fetchUserData();
    }


    private void loadStorefrontItems(String userId) {
        CollectionReference itemsRef = firestore.collection("Storefront");

        itemsRef.whereEqualTo("vendorID", userId)
                .addSnapshotListener(new EventListener<QuerySnapshot>() {
                    @Override
                    public void onEvent(@Nullable QuerySnapshot snapshot, @Nullable FirebaseFirestoreException error) {
                        if (error != null) {
                            Log.e("Firestore", "Error loading items", error);
                            return;
                        }
                        if (snapshot != null) {
                            itemList.clear();

                            for (DocumentSnapshot itemDoc : snapshot.getDocuments()) {
                                Item item = itemDoc.toObject(Item.class);

                                if (item != null) {
                                    itemList.add(item);
                                }
                            }
                            itemAdapter.notifyDataSetChanged();
                        }
                    }
                });
    }
    private void fetchUserData() {

        if (currentUser != null) {
            String userId = vendorId;

            DocumentReference userDocRef = firestore.collection("Vendors").document(userId);

            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot snapshot = task.getResult();

                    if (snapshot.exists() /*&& Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))*/) {
                        String companyName = snapshot.getString("companyName");

                        // Handle null values
                        companyName = (companyName != null) ? companyName : "";

                        // Use resource string with placeholders
                        String displayCompanyName = getString(R.string.profile_name_format, companyName, "");
                        profileNameTextView.setText(displayCompanyName);
                    } else {
                        Toast.makeText(StorefrontActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                } else {
                    Toast.makeText(StorefrontActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }
    private void checkUserType() {
        if (currentUser == null) {
            Toast.makeText(this, "User not authenticated", Toast.LENGTH_SHORT).show();
            return;
        }

        String userID = currentUser.getUid();
        FirebaseFirestore.getInstance().collection("Users").document(userID).get()
                .addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        DocumentSnapshot document = task.getResult();
                        if (document.exists()) {
                            // Check if user is vendor
                            Boolean isVendor = document.getBoolean("isVendor");
                            Intent intent;

                            if (isVendor != null && isVendor) {
                                //User is vendor
                                intent = new Intent(StorefrontActivity.this, MainVendorActivity.class);
                            } else {
                                //Regular user
                                intent = new Intent(StorefrontActivity.this, MainActivity.class);
                            }
                            startActivity(intent);
                            finish();
                        } else {
                            //document doesn't exist, treat like regular user
                            startActivity(new Intent(StorefrontActivity.this, MainActivity.class));
                            finish();
                        }
                    } else {
                        Log.e("StorefrontActivity", "Error checking user type", task.getException());
                        //Treat as regular user if error
                        startActivity(new Intent(StorefrontActivity.this, MainActivity.class));
                        finish();
                    }
                });
    }

    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(StorefrontActivity.this, targetActivity);
        startActivity(intent);
    }
}












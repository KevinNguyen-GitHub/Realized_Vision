
package com.example.realizedvision;

import android.content.Context;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.PopupWindow;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.QuerySnapshot;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class StorefrontActivity extends AppCompatActivity {

    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private Button commissionRequest;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendor_profile); // Ensure correct XML file

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
        itemAdapter = new ItemAdapter(this, itemList);
        recyclerView.setAdapter(itemAdapter);

        // Check if storefront exists and populate it with 10 sample items if empty
        checkOrPopulateStorefront();

        // Initialize the commission request button
                commissionRequest = findViewById(R.id.RequestButton);
        commissionRequest.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                // Inflate the popup layout (popup_commision_form.xml)
                LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
                final View popupView = inflater.inflate(R.layout.popup_commission_form, null);

                int width = ViewGroup.LayoutParams.WRAP_CONTENT;
                int height = ViewGroup.LayoutParams.WRAP_CONTENT;
                boolean focusable = true;
                final PopupWindow popupWindow = new PopupWindow(popupView, width, height, focusable);

                // Allow the popup to be dismissed when touching outside
                popupWindow.setOutsideTouchable(true);
                popupWindow.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));

                // Show the popup at the center of the screen
                popupWindow.showAtLocation(findViewById(android.R.id.content), Gravity.CENTER, 0, 0);

                // Get references for the required fields and submit button
                final TextInputLayout nameInputLayout = popupView.findViewById(R.id.nameInputLayout);
                final TextInputLayout typeInputLayout = popupView.findViewById(R.id.typeInputLayout);
                final TextInputLayout sizeInputLayout = popupView.findViewById(R.id.sizeInputLayout);
                final TextInputLayout styleInputLayout = popupView.findViewById(R.id.styleInputLayout);

                final TextInputEditText nameEditText = popupView.findViewById(R.id.nameEditText);
                final TextInputEditText typeEditText = popupView.findViewById(R.id.typeEditText);
                final TextInputEditText sizeEditText = popupView.findViewById(R.id.sizeEditText);
                final TextInputEditText styleEditText = popupView.findViewById(R.id.styleEditText);

                Button submitButton = popupView.findViewById(R.id.submitButton);

                submitButton.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        boolean valid = true;
                        String name = nameEditText.getText().toString().trim();
                        String type = typeEditText.getText().toString().trim();
                        String size = sizeEditText.getText().toString().trim();
                        String style = styleEditText.getText().toString().trim();

                        // Validate required fields
                        if (TextUtils.isEmpty(name)) {
                            nameInputLayout.setError("Name is required");
                            valid = false;
                        } else {
                            nameInputLayout.setError(null);
                        }

                        if (TextUtils.isEmpty(type)) {
                            typeInputLayout.setError("Type is required");
                            valid = false;
                        } else {
                            typeInputLayout.setError(null);
                        }

                        if (TextUtils.isEmpty(size)) {
                            sizeInputLayout.setError("Size is required");
                            valid = false;
                        } else {
                            sizeInputLayout.setError(null);
                        }

                        if (TextUtils.isEmpty(style)) {
                            styleInputLayout.setError("Style is required");
                            valid = false;
                        } else {
                            styleInputLayout.setError(null);
                        }

                        if (valid) {
                            // All required fields are filled. Proceed with form submission.
                            Toast.makeText(StorefrontActivity.this, "Form Submitted", Toast.LENGTH_SHORT).show();
                            popupWindow.dismiss();

                        }
                    }
                });
            }
        });
    }


    // Check if the storefront contains any items
    private void checkOrPopulateStorefront() {
        if (currentUser != null) {
            String userId = currentUser.getUid();

            CollectionReference itemsRef = firestore.collection("Storefront");

            itemsRef.whereEqualTo("vendorID", userId)
                    .addSnapshotListener(new EventListener<QuerySnapshot>() {
                        @Override
                        public void onEvent(@Nullable QuerySnapshot snapshot, @Nullable FirebaseFirestoreException error) {
                            if (error != null) {
                                Log.e("Firestore", "Error loading items", error);
                                return;
                            }

                            if (snapshot == null || snapshot.isEmpty()) {
                                Log.d("Firestore", "No items found for this vendor");
                                populateStorefront(userId);
                            }

                            loadStorefrontItems(userId);
                        }
                    });
        }
    }

    private void populateStorefront(String userId) {
        CollectionReference storefrontColRef = firestore.collection("Storefront");

        String itemID;
        for (int i = 1; i <= 10; i++) {
            itemID = storefrontColRef.document().getId();

            HashMap<String, Object> item = new HashMap<>();
            item.put("name", "Sample Item " + i);
            item.put("description", "This is a sample item number " + i);
            item.put("price", (double) (5 + i * 2)); // Different prices
            item.put("imageUrl", ""); // No image for now
            item.put("itemID", itemID);
            item.put("vendorID", userId);

            storefrontColRef.document(itemID).set(item);
        }

        Toast.makeText(StorefrontActivity.this, "10 Sample Items Added to Storefront!", Toast.LENGTH_SHORT).show();

        // Load the items after populating them
        loadStorefrontItems(userId);
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
}



// -------------------------READ ME ---------------------------------READ ME--------------------------------READ ME------------------------------------------------
//COMMENTED CODE BELOW IS FOR USER INPUT VERSION. USE WHEN READY TO
//IMPLEMENT ADD FUNCTION. CODE ABOVE IS BOILER PLATE VERSION FOR VIEW
//TESTING.






//package com.example.realizedvision;
//
//import android.os.Bundle;
//import android.util.Log;
//import android.widget.Toast;
//
//import androidx.annotation.NonNull;
//import androidx.appcompat.app.AppCompatActivity;
//import androidx.recyclerview.widget.LinearLayoutManager;
//import androidx.recyclerview.widget.RecyclerView;
//
//import com.google.firebase.auth.FirebaseAuth;
//import com.google.firebase.auth.FirebaseUser;
//import com.google.firebase.database.DataSnapshot;
//import com.google.firebase.database.DatabaseError;
//import com.google.firebase.database.DatabaseReference;
//import com.google.firebase.database.FirebaseDatabase;
//import com.google.firebase.database.ValueEventListener;
//import com.example.realizedvision.ItemAdapter;
//
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.List;
//
//public class StorefrontActivity extends AppCompatActivity {
//
//    private RecyclerView recyclerView;
//    private ItemAdapter itemAdapter;
//    private List<Item> itemList;
//    private DatabaseReference userDatabaseRef;
//    private FirebaseUser currentUser;
//
//    @Override
//    protected void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);
//        setContentView(R.layout.activity_vendor_profile); // Make sure this XML file is correct!
//
//        // Initialize Firebase
//        currentUser = FirebaseAuth.getInstance().getCurrentUser();
//        userDatabaseRef = FirebaseDatabase.getInstance().getReference("Users");
//
//        // Initialize RecyclerView
//        recyclerView = findViewById(R.id.recyclerView);
//        recyclerView.setLayoutManager(new LinearLayoutManager(this));
//
//        // Initialize item list and adapter
//        itemList = new ArrayList<>();
//        itemAdapter = new ItemAdapter(this, itemList);
//        recyclerView.setAdapter(itemAdapter);
//
//        // Check if storefront exists and load items
//        checkOrCreateStorefront();
//    }
//
//    private void checkOrCreateStorefront() {
//        if (currentUser != null) {
//            String userId = currentUser.getUid();
//            DatabaseReference storefrontRef = userDatabaseRef.child(userId).child("storefront");
//
//            // Check if "storefront" exists
//            storefrontRef.addListenerForSingleValueEvent(new ValueEventListener() {
//                @Override
//                public void onDataChange(@NonNull DataSnapshot snapshot) {
//                    if (!snapshot.exists()) {
//                        // Storefront doesn't exist, create it
//                        createDefaultStorefront(userId);
//                    } else {
//                        // Load existing items
//                        loadStorefrontItems(userId);
//                    }
//                }
//
//                @Override
//                public void onCancelled(@NonNull DatabaseError error) {
//                    Toast.makeText(StorefrontActivity.this, "Error: " + error.getMessage(), Toast.LENGTH_SHORT).show();
//                }
//            });
//        }
//    }
//
//    private void createDefaultStorefront(String userId) {
//        DatabaseReference storefrontRef = userDatabaseRef.child(userId).child("storefront").child("items");
//
//        // Create a default item for testing
//        HashMap<String, Object> defaultItem = new HashMap<>();
//        defaultItem.put("name", "Sample Item");
//        defaultItem.put("description", "This is a sample item.");
//        defaultItem.put("price", 10.99);
//        defaultItem.put("imageUrl", "https://via.placeholder.com/150");
//
//        // Push default item into the storefront collection
//        storefrontRef.push().setValue(defaultItem).addOnCompleteListener(task -> {
//            if (task.isSuccessful()) {
//                Toast.makeText(StorefrontActivity.this, "Storefront created!", Toast.LENGTH_SHORT).show();
//                loadStorefrontItems(userId);
//            } else {
//                Toast.makeText(StorefrontActivity.this, "Failed to create storefront", Toast.LENGTH_SHORT).show();
//            }
//        });
//    }
//
//    private void loadStorefrontItems(String userId) {
//        DatabaseReference itemsRef = userDatabaseRef.child(userId).child("storefront").child("items");
//
//        itemsRef.addValueEventListener(new ValueEventListener() {
//            @Override
//            public void onDataChange(@NonNull DataSnapshot snapshot) {
//                itemList.clear();
//                for (DataSnapshot dataSnapshot : snapshot.getChildren()) {
//                    Item item = dataSnapshot.getValue(Item.class);
//                    if (item != null) {
//                        itemList.add(item);
//                    }
//                }
//                itemAdapter.notifyDataSetChanged();
//            }
//
//            @Override
//            public void onCancelled(@NonNull DatabaseError error) {
//                Log.e("Firebase", "Error loading items", error.toException());
//            }
//        });
//    }
//}


package com.example.realizedvision;

import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class StorefrontActivity extends AppCompatActivity {

    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;

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
    }

    // Check if the storefront contains any items
    private void checkOrPopulateStorefront() {
        if (currentUser != null) {
            String userId = currentUser.getUid();

            DocumentReference vendorDocRef = firestore.collection("Vendors").document(userId);

            vendorDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot userSnapshot = task.getResult();

                    if (userSnapshot.exists() /*&& Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))*/) {
                        CollectionReference storefrontColRef = firestore.collection("Storefront");

                        storefrontColRef.get().addOnCompleteListener(storefrontTask -> {
                            if (storefrontTask.isSuccessful()){
                                QuerySnapshot storefrontSnapshot = storefrontTask.getResult();

                                if (storefrontSnapshot.isEmpty()) {
                                    populateStorefront(userId);
                                } else {
                                    loadStorefrontItems(userId);
                                }
                            } else {
                                Toast.makeText(StorefrontActivity.this, "Error: " + storefrontTask.getException().getMessage(), Toast.LENGTH_SHORT).show();
                            }
                        });
                    } else {
                        Toast.makeText(StorefrontActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                } else {
                    Toast.makeText(StorefrontActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }

    private void populateStorefront(String userId) {

        DocumentReference vendorDocRef = firestore.collection("Vendors").document(userId);

        vendorDocRef.get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot userSnapshot = task.getResult();

                if (userSnapshot.exists() /*&& Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))*/) {
                    CollectionReference storefrontColRef = firestore.collection("Storefront");

                    String itemID;
                    DocumentReference itemRef;
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
                } else {
                    Toast.makeText(StorefrontActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                }
            } else {
                Toast.makeText(StorefrontActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
            }
        });

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

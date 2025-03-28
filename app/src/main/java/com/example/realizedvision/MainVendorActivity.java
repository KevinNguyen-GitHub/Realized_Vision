package com.example.realizedvision;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.QueryDocumentSnapshot;
import com.google.firebase.firestore.QuerySnapshot;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MainVendorActivity extends AppCompatActivity{
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private String vendorId;
    private TextView profileNameTextView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendormain);

//        Connect to db, find user instance
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();
        vendorId = currentUser.getUid();

        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);

        Button addItemButton = findViewById(R.id.add_item_button);
        Button deleteItemButton = findViewById(R.id.delete_item_button);
        Button viewOrdersButton = findViewById(R.id.view_orders_button);
        profileNameTextView = findViewById(R.id.main_vendor_name);

//        Clicking on add or delete buttons
        addItemButton.setOnClickListener(v -> addItem(vendorId));
        deleteItemButton.setOnClickListener(v -> deleteItem(vendorId));
//        viewOrdersButton.setOnClickListener(v -> navigateTo(OrderHistoryActivity.class));

//        Navigate to desired elements when clicked
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(StorefrontActivity.class));

        fetchUserData();
    }

    private void addItem(String vendorId) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("Add New Item");

        View viewInflated = LayoutInflater.from(this).inflate(R.layout.dialog_add_item, null);
        final EditText inputName = viewInflated.findViewById(R.id.input_name);
        final EditText inputDescription = viewInflated.findViewById(R.id.input_description);
        final EditText inputPrice = viewInflated.findViewById(R.id.input_price);
        final EditText inputCategory = viewInflated.findViewById(R.id.input_category);

        builder.setView(viewInflated);

        builder.setPositiveButton("Add", (dialog, which) -> {
            String name = inputName.getText().toString().trim();
            String description = inputDescription.getText().toString().trim();
            String priceString = inputPrice.getText().toString().trim();
            String category = inputCategory.getText().toString().trim();

            if (!name.isEmpty() && !description.isEmpty() && !priceString.isEmpty() && !category.isEmpty()) {
                double price = Double.parseDouble(priceString);
                String userId = currentUser.getUid();

                // Check if an item with the same name already exists for the vendor
                firestore.collection("Storefront")
                        .whereEqualTo("name", name)
                        .whereEqualTo("vendorID", userId)
                        .get()
                        .addOnSuccessListener(querySnapshot -> {
                            if (!querySnapshot.isEmpty()) {
                                // Item with the same name exists, show error message
                                Toast.makeText(this, "Item with this name already exists!", Toast.LENGTH_SHORT).show();
                            } else {
                                // No duplicate, proceed with adding the item
                                addNewItemToFirestore(name, description, price, category, userId, vendorId);
                            }
                        })
                        .addOnFailureListener(e -> {
                            Toast.makeText(this, "Error checking existing items.", Toast.LENGTH_SHORT).show();
                        });
            }
        });

        builder.setNegativeButton("Cancel", (dialog, which) -> dialog.cancel());

        AlertDialog dialog = builder.create();
        dialog.setOnShowListener(dialogInterface -> {
            dialog.getButton(AlertDialog.BUTTON_POSITIVE).setTextColor(getResources().getColor(android.R.color.black));
            dialog.getButton(AlertDialog.BUTTON_NEGATIVE).setTextColor(getResources().getColor(android.R.color.black));
        });
        dialog.show();
    }

    // Helper function to add the item and reload storefront
    private void addNewItemToFirestore(String name, String description, double price,String category, String userId, String vendorId) {
        CollectionReference storefrontColRef = firestore.collection("Storefront");
        String itemID = storefrontColRef.document().getId();
        String imageUrl = ""; // Placeholder for image URL

        HashMap<String, Object> item = new HashMap<>();
        item.put("name", name);
        item.put("description", description);
        item.put("imageUrl", imageUrl);
        item.put("price", price);
        item.put("category", category);
        item.put("itemID", itemID);
        item.put("vendorID", userId);
        item.put("quantity", 1);

        storefrontColRef.document(itemID).set(item)
                .addOnSuccessListener(aVoid -> {
                    Toast.makeText(this, "Item Added Successfully", Toast.LENGTH_SHORT).show();
                })
                .addOnFailureListener(e -> {
                    Toast.makeText(this, "Error Adding Item", Toast.LENGTH_SHORT).show();
                });
    }

    private void deleteItem(String vendorId) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("Delete Item");

        View viewInflated = LayoutInflater.from(this).inflate(R.layout.dialog_delete_item, null);
        final EditText inputItem = viewInflated.findViewById(R.id.input_item_id);

        builder.setView(viewInflated);

        builder.setPositiveButton("Delete", (dialog, which) -> {
            String inputText = inputItem.getText().toString().trim();
            if (!inputText.isEmpty()) {
                // Check if input is an item ID
                DocumentReference itemRef = firestore.collection("Storefront").document(inputText);
                itemRef.get().addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        String itemVendorId = documentSnapshot.getString("vendorID");

                        if (itemVendorId != null && itemVendorId.equals(vendorId)) {
                            // Delete the item by item ID
                            itemRef.delete().addOnSuccessListener(aVoid -> {
                                Toast.makeText(this, "Item Deleted Successfully", Toast.LENGTH_SHORT).show();
                            }).addOnFailureListener(e -> {
                                Toast.makeText(this, "Error Deleting Item", Toast.LENGTH_SHORT).show();
                            });
                        } else {
                            Toast.makeText(this, "You can only delete your own items.", Toast.LENGTH_SHORT).show();
                        }
                    } else {
                        // If not an item ID, search by name
                        searchAndDeleteByName(inputText, vendorId);
                    }
                }).addOnFailureListener(e -> {
                    Toast.makeText(this, "Error retrieving item data.", Toast.LENGTH_SHORT).show();
                });
            }
        });

        builder.setNegativeButton("Cancel", (dialog, which) -> dialog.cancel());

        AlertDialog dialog = builder.create();
        dialog.setOnShowListener(dialogInterface -> {
            dialog.getButton(AlertDialog.BUTTON_POSITIVE).setTextColor(getResources().getColor(android.R.color.black));
            dialog.getButton(AlertDialog.BUTTON_NEGATIVE).setTextColor(getResources().getColor(android.R.color.black));
        });
        dialog.show();
    }

    // Function to delete by item name
    private void searchAndDeleteByName(String itemName, String vendorId) {
        firestore.collection("Storefront")
                .whereEqualTo("name", itemName)
                .whereEqualTo("vendorID", vendorId) // Ensure only deleting current user's items
                .get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    if (!queryDocumentSnapshots.isEmpty()) {
                        for (DocumentSnapshot doc : queryDocumentSnapshots.getDocuments()) {
                            doc.getReference().delete().addOnSuccessListener(aVoid -> {
                                Toast.makeText(this, "Item Deleted Successfully", Toast.LENGTH_SHORT).show();
                            }).addOnFailureListener(e -> {
                                Toast.makeText(this, "Error Deleting Item", Toast.LENGTH_SHORT).show();
                            });
                        }
                    } else {
                        Toast.makeText(this, "Item not found.", Toast.LENGTH_SHORT).show();
                    }
                })
                .addOnFailureListener(e -> {
                    Toast.makeText(this, "Error searching for item.", Toast.LENGTH_SHORT).show();
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
                        profileNameTextView.setText("Welcome, " + displayCompanyName);
                    } else {
                        Toast.makeText(MainVendorActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                    }
                } else {
                    Toast.makeText(MainVendorActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                }
            });
        }
    }

    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainVendorActivity.this, targetActivity);
        startActivity(intent);
    }
}

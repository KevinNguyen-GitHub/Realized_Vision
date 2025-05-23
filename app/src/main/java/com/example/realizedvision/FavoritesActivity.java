package com.example.realizedvision;

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
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;
import java.util.ArrayList;
import java.util.List;

public class FavoritesActivity extends AppCompatActivity implements ItemAdapter.OnItemClickListener {
    private RecyclerView recyclerView;
    private ItemAdapter favoritesAdapter;
    private List<Item> favorites;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_favorites);

        // Initialize Firebase and RecyclerView
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();
        recyclerView = findViewById(R.id.favoritesRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));

        favorites = new ArrayList<>();
        favoritesAdapter = new ItemAdapter(this, favorites, false);
        recyclerView.setAdapter(favoritesAdapter);

        favoritesAdapter.setOnItemClickListener(this);

        // Fetch favorites from Firestore
        fetchFavoritesFromFirestore();

        // Set up navigation
        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        Button cartButton = findViewById(R.id.cart_button);

        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));
        cartButton.setOnClickListener(view -> navigateTo(ShoppingCartActivity.class));
    }

    // Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(FavoritesActivity.this, targetActivity);
        startActivity(intent);
    }

    private void fetchFavoritesFromFirestore() {
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        String userId = currentUser.getUid();

        db.collection("Users")
                .document(userId)
                .collection("Favorites")
                .get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    Log.d("Favorites Activity", "Number of favorites fetched: " + queryDocumentSnapshots.size());
                    favorites.clear(); // Clear the existing list

                    for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
                        // Map the document to an Item object
                        Item item = document.toObject(Item.class);
                        item.setFavorite(true); // Mark as favorite
                        favorites.add(item); // Add to the list
                    }
                    favoritesAdapter.notifyDataSetChanged(); // Update the RecyclerView
                })
                .addOnFailureListener(e -> {
                    Log.e("Favorites Activity", "Error fetching favorites: " + e.getMessage());
                    Toast.makeText(FavoritesActivity.this, "Failed to fetch favorites", Toast.LENGTH_SHORT).show();
                });
    }

    @Override
    public void onFavoriteClick(int position) {
        Item item = favorites.get(position);
        item.setFavorite(!item.isFavorite());
        favoritesAdapter.notifyItemChanged(position);

        String itemId = item.getItemID();
        String userID = currentUser.getUid();
        CollectionReference favoritesRef = FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userID)
                .collection("Favorites");

        favoritesRef.document(itemId).get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot document = task.getResult();
                if (document.exists()) { // Item already in favorites, remove
                    favoritesRef.document(itemId).delete()
                            .addOnSuccessListener(aVoid -> {
                                Log.d("Favorites Activity", "Removed item from favorites: " + item.getName());
                                Toast.makeText(FavoritesActivity.this, "Removed from favorites", Toast.LENGTH_SHORT).show();

                                // Remove the item from the list and update the adapter
                                favorites.remove(position);
                                favoritesAdapter.notifyItemRemoved(position);
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Favorites Activity", "Failure removing item from favorites", e);
                                Toast.makeText(FavoritesActivity.this, "Failed to remove item from favorites", Toast.LENGTH_SHORT).show();
                            });
                } else { // Add item to favorites
                    favoritesRef.document(itemId).set(item)
                            .addOnSuccessListener(aVoid -> {
                                Log.d("Favorites Activity", "Added item to favorites: " + item.getName());
                                Toast.makeText(FavoritesActivity.this, "Added to favorites", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Favorites Activity", "Failed to add item to favorites: " + item.getName(), e);
                                Toast.makeText(FavoritesActivity.this, "Failed to favorite item", Toast.LENGTH_SHORT).show();
                            });
                }
            } else {
                Log.e("Favorites Activity", "Failed to check favorites", task.getException());
                Toast.makeText(FavoritesActivity.this, "Failed to check favorites", Toast.LENGTH_SHORT).show();
            }
        });
    }

    @Override
    public void onCartClick(int position) {
        Item item = favorites.get(position);
        Log.d("Main Activity", "Cart button clicked for: " + item.getName());

        String itemId = item.getItemID();
        String userID = currentUser.getUid();
        CollectionReference cartRef = FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userID)
                .collection("Shopping Cart");

        cartRef.document(itemId).get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot document = task.getResult();
                if (document.exists()) {
                    // Item already exists in the cart, increment the quantity
                    Long currentQuantity = document.getLong("quantity");
                    if (currentQuantity != null) {
                        cartRef.document(itemId).update("quantity", currentQuantity + 1)
                                .addOnSuccessListener(aVoid -> {
                                    Log.d("Favorites Activity", "Quantity incremented for: " + item.getName());
                                    Toast.makeText(FavoritesActivity.this, "Quantity incremented", Toast.LENGTH_SHORT).show();
                                })
                                .addOnFailureListener(e -> {
                                    Log.e("Favorites Activity", "Failed to increment quantity", e);
                                    Toast.makeText(FavoritesActivity.this, "Failed to increment quantity", Toast.LENGTH_SHORT).show();
                                });
                    }
                } else {
                    // Item does not exist in the cart, add it with quantity 1
                    item.setQuantity(1); // Set initial quantity to 1
                    cartRef.document(itemId).set(item)
                            .addOnSuccessListener(aVoid -> {
                                Log.d("Favorites Activity", "Added item to cart: " + item.getName());
                                Toast.makeText(FavoritesActivity.this, "Item added to cart", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Favorites Activity", "Failed to add item to cart", e);
                                Toast.makeText(FavoritesActivity.this, "Failed to add item", Toast.LENGTH_SHORT).show();
                            });
                }
            } else {
                Log.e("Favorites Activity", "Failed to check shopping cart", task.getException());
                Toast.makeText(FavoritesActivity.this, "Failed to check cart", Toast.LENGTH_SHORT).show();
            }
        });
    }

    @Override
    public void onItemClick(int position) {
        Item item = favorites.get(position);

        LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View popupView = inflater.inflate(R.layout.expand_item, null);

        ImageView popupItemImage = popupView.findViewById(R.id.item_image);
        TextView popupItemName = popupView.findViewById(R.id.item_name);
        TextView popupItemPrice = popupView.findViewById(R.id.item_price);
        TextView popupItemDesc = popupView.findViewById(R.id.item_description);
        TextView popupVendorName = popupView.findViewById(R.id.item_vendor);
        Button popupAddtoCart = popupView.findViewById(R.id.popup_cart_button);
        Button popupAddFavorites = popupView.findViewById(R.id.popup_favorite_button);

        popupAddtoCart.setOnClickListener(view ->{onCartClick(position);});
        popupAddFavorites.setOnClickListener(view ->{onFavoriteClick(position);});

        popupVendorName.setOnClickListener(view ->{
            navigateToVendorStorefront(item.getVendorID());
        });


        popupItemName.setText(item.getName());
        popupItemPrice.setText(String.format("$%.2f", item.getPrice()));
        popupItemDesc.setText(item.getDescription());

        //Load image
        Glide.with(this)
                .load(item.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .error(R.drawable.ic_placeholder_image)
                .into(popupItemImage);

        VendorUtils.fetchVendorName(item.getVendorID(), new VendorUtils.onVendorFetchedListener() {
            @Override
            public void onVendorFetched(String vendorName) {
                if (vendorName != null) {
                    popupVendorName.setText(vendorName);
                } else {
                    popupVendorName.setText("Vendor not found");
                }
            }
        });

        DisplayMetrics displayMetrics = new DisplayMetrics();
        getWindowManager().getDefaultDisplay().getMetrics(displayMetrics);
        int screenWidth = displayMetrics.widthPixels;
        int screenHeight = displayMetrics.heightPixels;

        // Calculate popup dimensions (80% of screen width, 70% of screen height)
        int popupWidth = (int) (screenWidth * 0.8);
        int popupHeight = (int) (screenHeight * 0.7);

        PopupWindow popupWindow = new PopupWindow(
                popupView, popupWidth, popupHeight, true);

        popupWindow.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
        popupWindow.showAtLocation(recyclerView, Gravity.CENTER, 0,0);


    }

    private void navigateToVendorStorefront(String vendorID) {
        Intent intent = new Intent(FavoritesActivity.this, StorefrontActivity.class);
        intent.putExtra("vendorID", vendorID); // Pass the vendor ID to the storefront activity
        startActivity(intent);
    }
}
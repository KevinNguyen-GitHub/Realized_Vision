package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.util.ArrayList;
import java.util.List;

public class ShoppingCartActivity extends AppCompatActivity implements ShoppingCartAdapter.OnItemClickListener{
    private RecyclerView recyclerView;
    private ShoppingCartAdapter shoppingCartAdapter;
    private List<Item> cartList;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;



    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_shopping_cart);

        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();
        recyclerView = findViewById(R.id.cartRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));

        cartList = new ArrayList<>();
        shoppingCartAdapter = new ShoppingCartAdapter(this, cartList);
        recyclerView.setAdapter(shoppingCartAdapter);

        shoppingCartAdapter.setOnItemClickListener(this);

        fetchShoppingCartfromFirestore();


        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        ImageView favoritesIcon = findViewById(R.id.favorites_icon);
        Button favoritesButton = findViewById(R.id.favorites_button);
        Button checkoutButton = findViewById(R.id.checkoutButton);

//        Navigate to desired activities when clicked
        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        favoritesIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        favoritesButton.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));
        checkoutButton.setOnClickListener(view -> navigateTo(CheckoutActivity.class));

    }
    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(ShoppingCartActivity.this, targetActivity);
        startActivity(intent);
    }

    private void fetchShoppingCartfromFirestore(){
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        String userID = currentUser.getUid();

        db.collection("Users").document(userID).collection("Shopping Cart")
                .get()
                .addOnSuccessListener( queryDocumentSnapshots -> {
                    Log.d("Shopping Cart Activity", "Number of items fetched: " + queryDocumentSnapshots.size());
                    cartList.clear();

                    for(QueryDocumentSnapshot document : queryDocumentSnapshots){
                        Item item = document.toObject(Item.class);
                        Log.d("Shopping Cart Activity", "Item: " + item.getName());
                        String itemId = item.getItemID();

                        db.collection("Users")
                                .document(userID)
                                .collection("Favorites")
                                .document(itemId)
                                .get()
                                .addOnSuccessListener(favoriteDocument -> {
                                    if(favoriteDocument.exists()){
                                        item.setFavorite(true);
                                    }
                                    else{
                                        item.setFavorite(false);
                                    }
                                    cartList.add(item);
                                    shoppingCartAdapter.notifyDataSetChanged();
                                }).addOnFailureListener(e ->{
                                    Log.d("Main Activity", "Error checking favorites", e);
                                });
                    }
                }).addOnFailureListener(e ->{
                    Log.d("Cart Activity", "Faied to fetch Cart" + e.getMessage());
                    Toast.makeText(ShoppingCartActivity.this, "Failed to fetch cart items", Toast.LENGTH_SHORT).show();
                });

    }

    @Override
    public void onFavoriteClick(int position) {
        Item item = cartList.get(position);
        item.setFavorite(!item.isFavorite());
        shoppingCartAdapter.notifyItemChanged(position);

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
                                Log.d("Shopping Cart Activity", "Removed item from favorites: " + item.getName());
                                Toast.makeText(ShoppingCartActivity.this, "Removed from favorites", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Shopping Cart Activity", "Failure removing item from favorites", e);
                                Toast.makeText(ShoppingCartActivity.this, "Failed to remove item from favorites", Toast.LENGTH_SHORT).show();
                            });
                } else { // Add item to favorites
                    favoritesRef.document(itemId).set(item)
                            .addOnSuccessListener(aVoid -> {
                                Log.d("Shopping Cart Activity", "Added item to favorites: " + item.getName());
                                Toast.makeText(ShoppingCartActivity.this, "Added to favorites", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Shopping Cart Activity", "Failed to add item to favorites: " + item.getName(), e);
                                Toast.makeText(ShoppingCartActivity.this, "Failed to favorite item", Toast.LENGTH_SHORT).show();
                            });
                }
            } else {
                Log.e("Shopping Cart Activity", "Failed to check favorites", task.getException());
                Toast.makeText(ShoppingCartActivity.this, "Failed to check favorites", Toast.LENGTH_SHORT).show();
            }
        });
    }

    @Override
    public void onRemoveClick(int position) {
        Item item = cartList.get(position);
        String userID = currentUser.getUid();
        CollectionReference cartRef = firestore.collection("Users").document(userID).collection("Shopping Cart");
        String itemId = item.getItemID();
        DocumentReference itemRef = cartRef.document(itemId);

        itemRef.get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot document = task.getResult();
                if (document.exists()) {
                    Long quantity = document.getLong("quantity");
                    if (quantity != null && quantity > 1) {
                        // Decrement quantity
                        itemRef.update("quantity", quantity - 1)
                                .addOnSuccessListener(aVoid -> {
                                    item.setQuantity(quantity - 1);
                                    shoppingCartAdapter.notifyItemChanged(position);
                                    Log.d("ShoppingCartActivity", "Quantity decremented for: " + item.getName());
                                    Toast.makeText(ShoppingCartActivity.this, "Quantity decremented", Toast.LENGTH_SHORT).show();
                                })
                                .addOnFailureListener(e -> {
                                    Log.e("ShoppingCartActivity", "Failed to decrement quantity", e);
                                    Toast.makeText(ShoppingCartActivity.this, "Failed to decrement quantity", Toast.LENGTH_SHORT).show();
                                });
                    } else {
                        // Remove item
                        cartRef.document(itemId).delete()
                                .addOnSuccessListener(aVoid -> {
                                    cartList.remove(position);
                                    shoppingCartAdapter.notifyDataSetChanged();
                                    Log.d("ShoppingCartActivity", "Item removed: " + item.getName());
                                    Toast.makeText(ShoppingCartActivity.this, "Item removed from cart", Toast.LENGTH_SHORT).show();
                                })
                                .addOnFailureListener(e -> {
                                    Log.e("ShoppingCartActivity", "Failed to remove item", e);
                                    Toast.makeText(ShoppingCartActivity.this, "Failed to remove item", Toast.LENGTH_SHORT).show();
                                });
                    }
                }
            } else {
                Log.e("Shopping Cart Activity", "Failed to check shopping cart", task.getException());
                Toast.makeText(ShoppingCartActivity.this, "Failed to check cart", Toast.LENGTH_SHORT).show();
            }
        });
    }

    @Override
    public void onIncrementClick(int position) {
        Item item = cartList.get(position);
        String userID = currentUser.getUid();
        CollectionReference cartRef = firestore.collection("Users").document(userID).collection("Shopping Cart");
        String itemId = item.getItemID();
        DocumentReference itemRef = cartRef.document(itemId);

        itemRef.get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot document = task.getResult();
                if (document.exists()) {
                    Long quantity = document.getLong("quantity");
                    if (quantity != null) {
                        // Increment quantity
                        itemRef.update("quantity", quantity + 1)
                                .addOnSuccessListener(aVoid -> {
                                    item.setQuantity(quantity + 1);
                                    shoppingCartAdapter.notifyItemChanged(position);
                                    Log.d("ShoppingCartActivity", "Quantity incremented for: " + item.getName());
                                    Toast.makeText(ShoppingCartActivity.this, "Quantity incremented", Toast.LENGTH_SHORT).show();
                                })
                                .addOnFailureListener(e -> {
                                    Log.e("ShoppingCartActivity", "Failed to increment quantity", e);
                                    Toast.makeText(ShoppingCartActivity.this, "Failed to increment quantity", Toast.LENGTH_SHORT).show();
                                });
                    }
                }
            } else {
                Log.e("ShoppingCartActivity", "Failed to get item", task.getException());
                Toast.makeText(ShoppingCartActivity.this, "Failed to get item", Toast.LENGTH_SHORT).show();
            }
        });
    }
    @Override
    public void onItemClick(int position) {
        Item item = cartList.get(position);
        Intent intent = new Intent(ShoppingCartActivity.this, ExpandItem.class);
        intent.putExtra("item_id", item.getItemID()); // Pass the item ID to the details activity
        startActivity(intent);
    }

}

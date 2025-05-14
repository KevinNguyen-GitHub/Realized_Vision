package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class MainActivity extends AppCompatActivity {
    private RecyclerView recyclerView;
    private ProductAdapter productAdapter;

    private FirebaseUser currentUser;
    private FirebaseFirestore firestore;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

//        Connect to db, find user instance
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }


        recyclerView = findViewById(R.id.mainRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));

        List<Product> productList = new ArrayList<>();
        productList.add(new Product(1, 101, "Ceramic Pot", "Hand-painted pot", new BigDecimal(49.99), R.drawable.ic_placeholder_image, 1));

        productAdapter = new ProductAdapter(productList);
        recyclerView.setAdapter(productAdapter);

        ImageView favoriteIcon = findViewById(R.id.favorites_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);




        ImageView profileIcon = findViewById(R.id.profile_icon);

        profileIcon.setOnClickListener(view -> {
            if (currentUser != null) {
                String userId = currentUser.getUid();

                DocumentReference userDocRef = firestore.collection("Users").document(userId);

                userDocRef.get().addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        DocumentSnapshot snapshot = task.getResult();

                        if (snapshot.exists() && Boolean.TRUE.equals(snapshot.getBoolean("isVendor"))) {
                            navigateTo(StorefrontActivity.class);
                        } else if (snapshot.exists() && Boolean.FALSE.equals(snapshot.getBoolean("isVendor"))) {
                            navigateTo(ProfileActivity.class);
                        } else {
                            Toast.makeText(MainActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                        }
                    } else {
                        Toast.makeText(MainActivity.this, "Error: " + task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                    }
                });
            }
        });



        favoriteIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));

    }
    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainActivity.this, targetActivity);
        startActivity(intent);
    }

//    Retrieving items from database, adding them to item list to display
    private void fetchItemsfromFirestore(){
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        String userId = currentUser.getUid();

        db.collection("Storefront").get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    Log.d("MainActivity", "Number of items fetched: " + queryDocumentSnapshots.size());
                    itemList.clear();
                    for(QueryDocumentSnapshot document : queryDocumentSnapshots){
                        Item item = document.toObject(Item.class);
                        Log.d("MainActivity", "Item: " + item.getName());
                        String itemId = item.getItemID();

                        db.collection("Users")
                                .document(userId)
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
                                    itemList.add(item);
                                    itemAdapter.notifyDataSetChanged();
                                }).addOnFailureListener(e ->{
                                    Log.d("Main Activity", "Error checking favorites", e);
                                });
                    }
                }).addOnFailureListener(e ->{
                    Log.e("Main Activity", "Error fetching products: " + e.getMessage());
                });
    }

    @Override
    public void onFavoriteClick(int position){
        Item item = itemList.get(position);
        item.setFavorite(!item.isFavorite());
        itemAdapter.notifyItemChanged(position);

        String itemId = item.getItemID();
        Log.d("Main Activity", "Heart button clicked for: " + item.getName());

        String userID = currentUser.getUid();
        //Favorites subcollection under users collection
        CollectionReference favoritesRef = FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userID)
                .collection("Favorites");

        favoritesRef.document(itemId).get().addOnCompleteListener(task ->{
            if (task.isSuccessful()){
                DocumentSnapshot document  = task.getResult();
                if(document.exists()){//Item already in favorites, remove
                    favoritesRef.document(itemId).delete()
                            .addOnSuccessListener(avoid ->{
                                Log.d("Main Activity", "Removed item from favorites: "+ item.getName());
                                Toast.makeText(MainActivity.this, "Removed from favorites", Toast.LENGTH_SHORT).show();

                            }).addOnFailureListener(e ->{
                                Log.e("Main Activity", "Failure removing item from favorites", e);
                                Toast.makeText(MainActivity.this, "Failed to remove item from favorites", Toast.LENGTH_SHORT).show();
                            });
                }
                else{//add item to favorites
                    favoritesRef.document(itemId).set(item)
                            .addOnSuccessListener(avoid ->{
                                Log.d("Main Activity", "Added item to favorites: " + item.getName());
                                Toast.makeText(MainActivity.this, "Added to favorites", Toast.LENGTH_SHORT).show();
                            }).addOnFailureListener(e ->{
                                Log.e("Main Activity", "Failed to add item to favorites" + item.getName());
                                Toast.makeText(MainActivity.this, "Failed to favorite item", Toast.LENGTH_SHORT).show();
                            });


                }
            }
            else{
                Log.e("Main Activity", "Failed to check favorites", task.getException());
                Toast.makeText(MainActivity.this, "Failed to check favorites", Toast.LENGTH_SHORT).show();
            }
        });

    }

    @Override
    public void onCartClick(int position) {
        Item item = itemList.get(position);
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
                                    Log.d("Main Activity", "Quantity incremented for: " + item.getName());
                                    Toast.makeText(MainActivity.this, "Quantity incremented", Toast.LENGTH_SHORT).show();
                                })
                                .addOnFailureListener(e -> {
                                    Log.e("Main Activity", "Failed to increment quantity", e);
                                    Toast.makeText(MainActivity.this, "Failed to increment quantity", Toast.LENGTH_SHORT).show();
                                });
                    }
                } else {
                    // Item does not exist in the cart, add it with quantity 1
                    item.setQuantity(1); // Set initial quantity to 1
                    cartRef.document(itemId).set(item)
                            .addOnSuccessListener(aVoid -> {
                                Log.d("Main Activity", "Added item to cart: " + item.getName());
                                Toast.makeText(MainActivity.this, "Item added to cart", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Main Activity", "Failed to add item to cart", e);
                                Toast.makeText(MainActivity.this, "Failed to add item", Toast.LENGTH_SHORT).show();
                            });
                }
            } else {
                Log.e("Main Activity", "Failed to check shopping cart", task.getException());
                Toast.makeText(MainActivity.this, "Failed to check cart", Toast.LENGTH_SHORT).show();
            }
        });
    }

    @Override
    public void onItemClick(int position) {
        Item item = itemList.get(position);

        LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View popupView = inflater.inflate(R.layout.expand_item, null);

        ImageView popupItemImage = popupView.findViewById(R.id.item_image);
        TextView popupItemName = popupView.findViewById(R.id.item_name);
        TextView popupItemPrice = popupView.findViewById(R.id.item_price);
        TextView popupItemDesc = popupView.findViewById(R.id.item_description);
        TextView popupVendorName = popupView.findViewById(R.id.item_vendor);
        Button popupAddtoCart = popupView.findViewById(R.id.popup_cart_button);
        Button popupAddFavorites = popupView.findViewById(R.id.popup_favorite_button);
        Button popupShowReviews = popupView.findViewById(R.id.showReviewsButton);

        popupAddtoCart.setOnClickListener(view ->{onCartClick(position);});
        popupAddFavorites.setOnClickListener(view ->{onFavoriteClick(position);});


        popupShowReviews.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String itemID = item.getItemID();
                Intent intent = new Intent(MainActivity.this, ItemReviewsActivity.class);
                intent.putExtra("itemID", itemID);
                startActivity(intent);
            }
        });

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
        Intent intent = new Intent(MainActivity.this, StorefrontActivity.class);
        intent.putExtra("vendorID", vendorID); // Pass the vendor ID to the storefront activity
        startActivity(intent);
    }

}


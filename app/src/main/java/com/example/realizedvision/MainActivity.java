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

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.Query;
import com.google.firebase.firestore.QueryDocumentSnapshot;
import com.google.firebase.firestore.QuerySnapshot;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class MainActivity extends AppCompatActivity implements ItemAdapter.OnItemClickListener{
    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private List<Item> itemListFilter;

    private List<String> preferredCategories = new ArrayList<>();
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;

    @Override
    protected void onCreate(Bundle savedInstanceState){
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        new NotificationHelper(this);
        checkUserType();

//        Connect to db, find user instance
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();


//      Create recycler view to hold elements
        recyclerView = findViewById(R.id.mainRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        itemList = new ArrayList<>();
        itemListFilter = new ArrayList<>();
        itemAdapter = new ItemAdapter(this, itemList, true);
        recyclerView.setAdapter(itemAdapter);

        itemAdapter.setOnItemClickListener(this);

        fetchUserPreferencesAndItems();

        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView favoriteIcon = findViewById(R.id.favorites_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);

        //filter buttons
        Button artFilter = findViewById(R.id.filter_artwork);
        Button metalFilter = findViewById(R.id.filter_metalwork);
        Button woodFilter = findViewById(R.id.filter_woodwork);
        ImageView resetFilter = findViewById(R.id.resetFilterIcon);

        artFilter.setOnClickListener(view -> itemAdapter.getFilter().filter("Art"));
        metalFilter.setOnClickListener(view -> itemAdapter.getFilter().filter("Metalwork"));
        woodFilter.setOnClickListener(view -> itemAdapter.getFilter().filter("Woodwork"));
        resetFilter.setOnClickListener(view -> fetchItemsfromFirestore());


//        Navigate to desired elements when clicked
        favoriteIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));


    }
    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainActivity.this, targetActivity);
        startActivity(intent);
    }

    private void fetchUserPreferencesAndItems(){
        String userId = currentUser.getUid();

        //fetch favorites
        firestore.collection("Users")
                .document(userId)
                .collection("Favorites")
                .get()
                .addOnSuccessListener(favoriteQuery -> {
                    for(QueryDocumentSnapshot doc: favoriteQuery){ //add categories to preferences
                        String category = doc.getString("category");
                        if(category != null && !preferredCategories.contains(category)){
                            preferredCategories.add(category);
                        }
                    }
                    //Examine items from shopping cart
                    firestore.collection("Users")
                            .document(userId)
                            .collection("Shopping Cart")
                            .get()
                            .addOnSuccessListener(cartQuery ->{
                                for(QueryDocumentSnapshot doc:cartQuery){
                                    String category = doc.getString("category"); //add categories to preferences
                                    if(category != null && !preferredCategories.contains(category)){
                                        preferredCategories.add(category);
                                    }
                                }
                                fetchItemsfromFirestore();
                            });

                });
    }


//    Retrieving items from database, adding them to item list to display
    private void fetchItemsfromFirestore(){
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        String userId = currentUser.getUid();

        db.collection("Storefront").get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    itemList.clear();
                    itemListFilter.clear();

                    //user has no favorites or items in cart, load normally
                    if(preferredCategories.isEmpty()){
                        for(QueryDocumentSnapshot doc : queryDocumentSnapshots){
                            Item item = doc.toObject(Item.class);
                            processItem(userId, item, false);
                        }
                    }
                    else {
                        //check for items with preferred categories, process them first
                        for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
                            Item item = document.toObject(Item.class);
                            if (preferredCategories.contains(item.getCategory())) {
                                processItem(userId, item, true);
                            }
                        }
                        //non preferred items
                        for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
                            Item item = document.toObject(Item.class);
                            if (!preferredCategories.contains(item.getCategory())) {
                                processItem(userId, item, false);
                            }
                        }
                    }
                        }).addOnFailureListener(e ->{
                            Log.e("Main Activity", "Error checking user preferences: "+ e.getMessage());
                        });

    }
    private void processItem(String userID, Item item, boolean isPreferred){
        String itemID = item.getItemID();
        FirebaseFirestore db = FirebaseFirestore.getInstance();

        //Check favorites, fill hearts where needed
        db.collection("Users")
                .document(userID)
                .collection("Favorites")
                .document(itemID)
                .get()
                .addOnSuccessListener(favoriteDocument ->{
                    if(favoriteDocument.exists()){
                        item.setFavorite(true);
                    } else{
                     item.setFavorite(false);
                    }

                    //Mark preferred items
                    item.setPreferred(isPreferred);
                    itemList.add(item);
                    itemListFilter.add(item);
                    itemAdapter.notifyDataSetChanged();

                }).addOnFailureListener(e->{
                    Log.e("Main Activity", "Error checking favorites: " + e.getMessage());
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
                                intent = new Intent(MainActivity.this, MainVendorActivity.class);
                            } else {
                                //Regular user
                                intent = new Intent(MainActivity.this, MainActivity.class);
                            }
                            startActivity(intent);
                            finish();
                        } else {
                            //document doesn't exist, treat like regular user
                            startActivity(new Intent(MainActivity.this, MainActivity.class));
                            finish();
                        }
                    } else {
                        Log.e("Main Activity", "Error checking user type", task.getException());
                        //Treat as regular user if error
                        startActivity(new Intent(MainActivity.this, MainActivity.class));
                        finish();
                    }
                });
    }
    private void navigateToVendorStorefront(String vendorID) {
        Intent intent = new Intent(MainActivity.this, StorefrontActivity.class);
        intent.putExtra("vendorID", vendorID); // Pass the vendor ID to the storefront activity
        startActivity(intent);
    }

}


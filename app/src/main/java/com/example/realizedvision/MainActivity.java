package com.example.realizedvision;

import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Build;
import android.net.Uri;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.JsonArrayRequest;
import com.android.volley.toolbox.Volley;
import com.google.android.gms.maps.model.LatLng;
import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.DataSnapshot;
import com.google.firebase.database.DatabaseError;
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;
import com.google.firebase.database.ValueEventListener;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.Query;
import com.google.firebase.firestore.QueryDocumentSnapshot;
import com.google.firebase.firestore.QuerySnapshot;

import org.json.JSONException;
import org.json.JSONObject;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Map;

public class MainActivity extends AppCompatActivity implements ItemAdapter.OnItemClickListener {

    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private List<Item> itemListFilter;

    private final List<String> preferredCategories = new ArrayList<>();

    private static final String[] CATEGORIES = {
            "All",
            "Art",
            "Metalwork",
            "Woodwork",
            "Programming",
            "Flooring",
            "Graphic Design",
            "Welding",
            "Video & Animation",
            "Ceramic"
    };

    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private NotificationHelper notificationHelper;
    private static final int NOTIFICATION_PERMISSION_CODE = 100;
    private static final String POST_NOTIFICATIONS_PERMISSION =
            Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU
                    ? "android.permission.POST_NOTIFICATIONS"
                    : "";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        notificationHelper = new NotificationHelper(this);

        checkUserType();
        checkNotificationPermission();

        // Connect to db, find user instance
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();

        // Create recycler view to hold elements
        recyclerView = findViewById(R.id.mainRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        itemList = new ArrayList<>();
        itemListFilter = new ArrayList<>();
        itemAdapter = new ItemAdapter(this, itemList, true);
        recyclerView.setAdapter(itemAdapter);

        itemAdapter.setOnItemClickListener(this);

        fetchUserPreferencesAndItems();

        ImageView homeIcon     = findViewById(R.id.home_icon);
        ImageView favoriteIcon = findViewById(R.id.favorites_icon);
        ImageView messageIcon  = findViewById(R.id.messages_icon);
        ImageView profileIcon  = findViewById(R.id.profile_icon);

        // filter buttons
        Button   artFilter   = findViewById(R.id.filter_artwork);
        Button   metalFilter = findViewById(R.id.filter_metalwork);
        Button   woodFilter  = findViewById(R.id.filter_woodwork);
        ImageView addFilter  = findViewById(R.id.add_filter);
        ImageView resetFilter= findViewById(R.id.resetFilterIcon);

        artFilter.setOnClickListener(view -> itemAdapter.getFilter().filter("Art"));
        metalFilter.setOnClickListener(view -> itemAdapter.getFilter().filter("Metalwork"));
        woodFilter.setOnClickListener(view -> itemAdapter.getFilter().filter("Woodwork"));
        addFilter.setOnClickListener(view -> addFilter());
        resetFilter.setOnClickListener(view -> fetchItemsfromFirestore());

        // Navigate to desired elements when clicked
        favoriteIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));
    }

    // Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainActivity.this, targetActivity);
        startActivity(intent);
    }

    private void fetchUserPreferencesAndItems() {
        String userId = currentUser.getUid();

        // fetch favorites
        firestore.collection("Users")
                .document(userId)
                .collection("Favorites")
                .get()
                .addOnSuccessListener(favoriteQuery -> {
                    for (QueryDocumentSnapshot doc : favoriteQuery) { // add categories to preferences
                        String category = doc.getString("category");
                        if (category != null && !preferredCategories.contains(category)) {
                            preferredCategories.add(category);
                        }
                    }

                    // Examine items from shopping cart
                    firestore.collection("Users")
                            .document(userId)
                            .collection("Shopping Cart")
                            .get()
                            .addOnSuccessListener(cartQuery -> {
                                for (QueryDocumentSnapshot doc : cartQuery) {
                                    String category = doc.getString("category"); // add categories to preferences
                                    if (category != null && !preferredCategories.contains(category)) {
                                        preferredCategories.add(category);
                                    }
                                }
                                fetchItemsfromFirestore();
                            });
                });
    }

    // Retrieving items from database, adding them to item list to display
    private void fetchItemsfromFirestore() {
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        String userId = currentUser.getUid();

        db.collection("Storefront").get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    itemList.clear();
                    itemListFilter.clear();

                    // user has no favorites or items in cart, load normally
                    if (preferredCategories.isEmpty()) {
                        for (QueryDocumentSnapshot doc : queryDocumentSnapshots) {
                            Item item = doc.toObject(Item.class);
                            processItem(userId, item, false);
                        }
                    } else {
                        // check for items with preferred categories, process them first
                        for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
                            Item item = document.toObject(Item.class);
                            if (preferredCategories.contains(item.getCategory())) {
                                processItem(userId, item, true);
                            }
                        }
                        // non-preferred items
                        for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
                            Item item = document.toObject(Item.class);
                            if (!preferredCategories.contains(item.getCategory())) {
                                processItem(userId, item, false);
                            }
                        }
                    }
                })
                .addOnFailureListener(e ->
                        Log.e("Main Activity", "Error checking user preferences: " + e.getMessage()));
    }

    private void processItem(String userID, Item item, boolean isPreferred) {
        String itemID = item.getItemID();
        FirebaseFirestore db = FirebaseFirestore.getInstance();

        // Check favorites, fill hearts where needed
        db.collection("Users")
                .document(userID)
                .collection("Favorites")
                .document(itemID)
                .get()
                .addOnSuccessListener(favoriteDocument -> {
                    item.setFavorite(favoriteDocument.exists());

                    // Mark preferred items
                    item.setPreferred(isPreferred);
                    itemList.add(item);
                    itemListFilter.add(item);
                    itemAdapter.notifyDataSetChanged();
                })
                .addOnFailureListener(e ->
                        Log.e("Main Activity", "Error checking favorites: " + e.getMessage()));
    }

    @Override
    public void onFavoriteClick(int position) {
        Item item = itemList.get(position);
        item.setFavorite(!item.isFavorite());
        itemAdapter.notifyItemChanged(position);

        String itemId = item.getItemID();
        Log.d("Main Activity", "Heart button clicked for: " + item.getName());

        String userID = currentUser.getUid();
        // Favorites subcollection under users collection
        CollectionReference favoritesRef = FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userID)
                .collection("Favorites");

        favoritesRef.document(itemId).get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot document = task.getResult();
                if (document.exists()) { // Item already in favorites, remove
                    favoritesRef.document(itemId).delete()
                            .addOnSuccessListener(avoid -> {
                                Log.d("Main Activity", "Removed item from favorites: " + item.getName());
                                Toast.makeText(MainActivity.this, "Removed from favorites", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Main Activity", "Failure removing item from favorites", e);
                                Toast.makeText(MainActivity.this, "Failed to remove item from favorites", Toast.LENGTH_SHORT).show();
                            });
                } else { // add item to favorites
                    favoritesRef.document(itemId).set(item)
                            .addOnSuccessListener(avoid -> {
                                Log.d("Main Activity", "Added item to favorites: " + item.getName());
                                Toast.makeText(MainActivity.this, "Added to favorites", Toast.LENGTH_SHORT).show();
                            })
                            .addOnFailureListener(e -> {
                                Log.e("Main Activity", "Failed to add item to favorites" + item.getName());
                                Toast.makeText(MainActivity.this, "Failed to favorite item", Toast.LENGTH_SHORT).show();
                            });
                }
            } else {
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
                                .addOnSuccessListener(aVoid ->
                                        Log.d("Main Activity", "Quantity incremented for: " + item.getName()))
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

        ImageView popupItemImage   = popupView.findViewById(R.id.item_image);
        TextView  popupItemName    = popupView.findViewById(R.id.item_name);
        TextView  popupItemPrice   = popupView.findViewById(R.id.item_price);
        TextView  popupItemDesc    = popupView.findViewById(R.id.item_description);
        TextView  popupVendorName  = popupView.findViewById(R.id.item_vendor);
        Button    popupAddtoCart   = popupView.findViewById(R.id.popup_cart_button);
        Button    popupAddFavorites= popupView.findViewById(R.id.popup_favorite_button);

        popupAddtoCart.setOnClickListener(view -> onCartClick(position));
        popupAddFavorites.setOnClickListener(view -> onFavoriteClick(position));

        popupVendorName.setOnClickListener(view -> navigateToVendorStorefront(item.getVendorID()));

        popupItemName.setText(item.getName());
        popupItemPrice.setText(String.format("$%.2f", item.getPrice()));
        popupItemDesc.setText(item.getDescription());

        // Load image
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
        int screenWidth  = displayMetrics.widthPixels;
        int screenHeight = displayMetrics.heightPixels;

        // Calculate popup dimensions (80% of screen width, 70% of screen height)
        int popupWidth  = (int) (screenWidth  * 0.8);
        int popupHeight = (int) (screenHeight * 0.7);

        PopupWindow popupWindow = new PopupWindow(popupView, popupWidth, popupHeight, true);
        popupWindow.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
        popupWindow.showAtLocation(recyclerView, Gravity.CENTER, 0, 0);
    }

    private void showVendorFilterDialog() {
        // (implementation goes here)
    }

    private void checkUserType() {
        if (currentUser == null) {
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
                                // User is vendor
                                intent = new Intent(MainActivity.this, MainVendorActivity.class);
                                startActivity(intent);
                                finish();
                            }
                        } else {
                            // Document doesn't exist, treat like regular user
                            Log.e("Main Activity", "User document not found");
                        }
                    } else {
                        Log.e("Main Activity", "Error checking user type", task.getException());
                        // Treat as regular user if error
                    }
                });
    }

    private void addFilter() {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("Select Filter Category");

        LayoutInflater inflater = getLayoutInflater();
        View dialogView = inflater.inflate(R.layout.dialog_vendor_filters, null);
        Spinner filterSpinner = dialogView.findViewById(R.id.filterSpinner);
        builder.setView(dialogView);

        // Create adapter with categories
        ArrayAdapter<String> adapter = new ArrayAdapter<>(
                this,
                com.google.android.material.R.layout.support_simple_spinner_dropdown_item,
                CATEGORIES);
        adapter.setDropDownViewResource(com.google.android.material.R.layout.support_simple_spinner_dropdown_item);
        filterSpinner.setAdapter(adapter);

        Button searchButton = dialogView.findViewById(R.id.search_filters_button);
        builder.setNegativeButton("Cancel", null);

        AlertDialog dialog = builder.create();
        dialog.show();

        // Add temporary variable to track if selection is from user
        final boolean[] isUserSelection = {false};

        filterSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int position, long l) {
                if (isUserSelection[0]) {
                    String selectedCategory = CATEGORIES[position];
                    itemAdapter.getFilter().filter(selectedCategory);
                    dialog.dismiss();
                }
                isUserSelection[0] = true;
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {
                // Do nothing
            }
        });
    }

    // Add these helper methods to MainActivity
    private void showVendorResultsDialog(List<Pair<String, Double>> vendors, LatLng userLocation) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        View dialogView = LayoutInflater.from(this).inflate(R.layout.dialog_vendor_results, null);
        builder.setView(dialogView);

        RecyclerView vendorsRecyclerView = dialogView.findViewById(R.id.vendorsRecyclerView);
        vendorsRecyclerView.setLayoutManager(new LinearLayoutManager(this));

        VendorAdapter adapter = new VendorAdapter(vendors, firestore, userLocation) {
            @Override
            public void onBindViewHolder(@NonNull VendorViewHolder holder, int position) {
                super.onBindViewHolder(holder, position);
                String vendorId = vendors.get(position).first;

                firestore.collection("Vendors").document(vendorId).get()
                        .addOnSuccessListener(documentSnapshot -> {
                            if (documentSnapshot.exists()) {
                                String vendorAddress = documentSnapshot.getString("address");
                                holder.directionsButton.setOnClickListener(v -> {
                                    if (vendorAddress != null && !vendorAddress.isEmpty()) {
                                        Intent mapIntent = new Intent(MainActivity.this, MapActivity.class);
                                        mapIntent.putExtra("selectedAddress", vendorAddress);
                                        mapIntent.putExtra("hasUserLocation", userLocation != null);
                                        startActivity(mapIntent);
                                    } else {
                                        Toast.makeText(MainActivity.this, "Vendor address not available", Toast.LENGTH_SHORT).show();
                                    }
                                });
                            }
                        });
            }
        };

        vendorsRecyclerView.setAdapter(adapter);

        Button      closeButton = dialogView.findViewById(R.id.close_button);
        AlertDialog dialog      = builder.create();

        closeButton.setOnClickListener(v -> dialog.dismiss());
        dialog.show();
    }

    private void navigateToVendorStorefront(String vendorID) {
        Intent intent = new Intent(MainActivity.this, StorefrontActivity.class);
        intent.putExtra("vendorID", vendorID); // Pass the vendor ID to the storefront activity
    }

    // Copy the distance calculation from MapActivity
    private double calculateDistance(LatLng origin, LatLng destination) {
        final int EARTH_RADIUS = 3959; // Earth radius in miles

        double latDiff = Math.toRadians(destination.latitude - origin.latitude);
        double lonDiff = Math.toRadians(destination.longitude - origin.longitude);

        double lat1 = Math.toRadians(origin.latitude);
        double lat2 = Math.toRadians(destination.latitude);

        double a = Math.sin(latDiff / 2) * Math.sin(latDiff / 2) +
                Math.cos(lat1) * Math.cos(lat2) *
                        Math.sin(lonDiff / 2) * Math.sin(lonDiff / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return EARTH_RADIUS * c; // Returns distance in miles
    }

    // Add geocodeAddress interface and method (similar to MapActivity)
    interface OnGeocodeCompleteListener {
        void onGeocodeSuccess(double latitude, double longitude);
    }

    private void geocodeAddress(String address, OnGeocodeCompleteListener listener) {
        String url = "https://nominatim.openstreetmap.org/search?q=" +
                Uri.encode(address) + "&format=json";

        RequestQueue queue = Volley.newRequestQueue(this);

        JsonArrayRequest request = new JsonArrayRequest(
                Request.Method.GET,
                url,
                null,
                response -> {
                    try {
                        if (response.length() > 0) {
                            JSONObject place = response.getJSONObject(0);
                            double lat = place.getDouble("lat");
                            double lon = place.getDouble("lon");
                            listener.onGeocodeSuccess(lat, lon);
                        } else {
                            Toast.makeText(this, "Location not found: " + address, Toast.LENGTH_SHORT).show();
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                        Toast.makeText(this, "Error fetching location for " + address, Toast.LENGTH_SHORT).show();
                    }
                },
                error -> Toast.makeText(this, "Geocoding failed for " + address, Toast.LENGTH_SHORT).show()
        );

        queue.add(request);
    }

    private void checkNotificationPermission() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
            return; // No permission needed below Android 13
        }

        if (ContextCompat.checkSelfPermission(this, POST_NOTIFICATIONS_PERMISSION)
                == PackageManager.PERMISSION_GRANTED) {
            Log.d("Permission", "Notifications enabled");
            updateNotificationPreference(true); // Save to Firestore
        } else if (shouldShowRequestPermissionRationale(POST_NOTIFICATIONS_PERMISSION)) {
            showPermissionExplanationDialog(); // Explain why we need it
        } else {
            // Directly request (first time or "Don't ask again" not clicked)
            ActivityCompat.requestPermissions(
                    this,
                    new String[]{POST_NOTIFICATIONS_PERMISSION},
                    NOTIFICATION_PERMISSION_CODE
            );
        }
    }

    private void updateNotificationPreference(boolean enabled) {
        String userId = FirebaseAuth.getInstance().getCurrentUser().getUid();
        FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userId)
                .update("notificationsEnabled", enabled)
                .addOnFailureListener(e -> Log.e("MainActivity", "Failed to update notification pref", e));
    }

    private void showPermissionExplanationDialog() {
        new AlertDialog.Builder(this)
                .setTitle("Enable Notifications")
                .setMessage("This app needs notification permissions to alert you about orders and messages.")
                .setPositiveButton("Continue", (dialog, which) ->
                        ActivityCompat.requestPermissions(
                                MainActivity.this,
                                new String[]{POST_NOTIFICATIONS_PERMISSION},
                                NOTIFICATION_PERMISSION_CODE))
                .setNegativeButton("Cancel", null)
                .show();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode,
                                           @NonNull String[] permissions,
                                           @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == NOTIFICATION_PERMISSION_CODE) {
            boolean granted = grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED;
            updateNotificationPreference(granted); // Save choice to Firestore

            if (!granted) {
                Toast.makeText(this, "Notifications disabled. Enable in settings.", Toast.LENGTH_SHORT).show();
            }
        }
    }
}

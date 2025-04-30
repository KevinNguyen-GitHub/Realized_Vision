package com.example.realizedvision;

import android.app.AlertDialog;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.JsonArrayRequest;
import com.android.volley.toolbox.Volley;
import com.google.android.gms.maps.model.LatLng;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import org.json.JSONException;
import org.json.JSONObject;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class MainActivity extends AppCompatActivity {
    private RecyclerView recyclerView;
    private ProductAdapter productAdapter;

    private FirebaseUser currentUser;
    private FirebaseFirestore firestore;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

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


        TextView searchInput = findViewById(R.id.search_input);
        searchInput.setOnClickListener(view -> showVendorFilterDialog());

    }


    private void showVendorFilterDialog() {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        View dialogView = LayoutInflater.from(this).inflate(R.layout.dialog_vendor_filters, null);
        builder.setView(dialogView);

        CheckBox cbWoodworking = dialogView.findViewById(R.id.checkbox_woodworking);
        CheckBox cbWelding = dialogView.findViewById(R.id.checkbox_welding);
        CheckBox cbProgramming = dialogView.findViewById(R.id.checkbox_programming);
        CheckBox cbFlooring = dialogView.findViewById(R.id.checkbox_flooring);
        CheckBox cbArtist = dialogView.findViewById(R.id.checkbox_artist);
        CheckBox cbMechanic = dialogView.findViewById(R.id.checkbox_mechanic);
        CheckBox cbVideo = dialogView.findViewById(R.id.checkbox_video);
        CheckBox cbDesign = dialogView.findViewById(R.id.checkbox_design);

        Button searchButton = dialogView.findViewById(R.id.search_filters_button);
        AlertDialog dialog = builder.create();

        searchButton.setOnClickListener(v -> {
            List<String> selectedFilters = new ArrayList<>();
            if (cbWoodworking.isChecked()) selectedFilters.add("woodworking");
            if (cbWelding.isChecked()) selectedFilters.add("welding");
            if (cbProgramming.isChecked()) selectedFilters.add("programming");
            if (cbFlooring.isChecked()) selectedFilters.add("flooring");
            if (cbArtist.isChecked()) selectedFilters.add("artist");
            if (cbMechanic.isChecked()) selectedFilters.add("mechanic");
            if (cbVideo.isChecked()) selectedFilters.add("video_and_automation");
            if (cbDesign.isChecked()) selectedFilters.add("graphic_design");

            if (selectedFilters.isEmpty()) {
                Toast.makeText(MainActivity.this, "Please select at least one filter.", Toast.LENGTH_SHORT).show();
                return;
            }

            String userId = currentUser.getUid();
            firestore.collection("Users").document(userId).get()
                    .addOnSuccessListener(userDoc -> {
                        if (userDoc.exists()) {
                            String userAddress = userDoc.getString("address");
                            if (userAddress != null && !userAddress.isEmpty()) {
                                geocodeAddress(userAddress, (userLat, userLng) -> {
                                    LatLng userLocation = new LatLng(userLat, userLng);
                                    firestore.collection("Vendors").get()
                                            .addOnSuccessListener(querySnapshot -> {
                                                List<Pair<String, Double>> matchedVendors = new ArrayList<>();
                                                AtomicInteger totalVendorsToProcess = new AtomicInteger(0);
                                                AtomicInteger processedVendors = new AtomicInteger(0);

                                                for (QueryDocumentSnapshot doc : querySnapshot) {
                                                    HashMap<String, Object> filters = (HashMap<String, Object>) doc.get("vendorFilters");
                                                    if (filters != null) {
                                                        boolean matchesAll = true;
                                                        for (String filter : selectedFilters) {
                                                            Object value = filters.get(filter);
                                                            if (!(value instanceof Boolean) || !(Boolean) value) {
                                                                matchesAll = false;
                                                                break;
                                                            }
                                                        }

                                                        if (matchesAll) {
                                                            String vendorAddress = doc.getString("address");
                                                            if (vendorAddress != null && !vendorAddress.isEmpty()) {
                                                                totalVendorsToProcess.incrementAndGet();
                                                                geocodeAddress(vendorAddress, (vendorLat, vendorLng) -> {
                                                                    LatLng vendorLocation = new LatLng(vendorLat, vendorLng);
                                                                    double distance = calculateDistance(userLocation, vendorLocation);
                                                                    matchedVendors.add(new Pair<>(doc.getId(), distance));
                                                                    processedVendors.incrementAndGet();

                                                                    if (processedVendors.get() == totalVendorsToProcess.get()) {
                                                                        showVendorResultsDialog(matchedVendors, userLocation);
                                                                    }
                                                                });
                                                            } else {
                                                                matchedVendors.add(new Pair<>(doc.getId(), null));
                                                                processedVendors.incrementAndGet();

                                                                if (processedVendors == totalVendorsToProcess) {
                                                                    showVendorResultsDialog(matchedVendors, userLocation);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }

                                                if (totalVendorsToProcess.get() == 0) {
                                                    Toast.makeText(MainActivity.this, "No vendors match your filters", Toast.LENGTH_SHORT).show();
                                                }
                                            });
                                });
                            } else {
                                firestore.collection("Vendors").get()
                                        .addOnSuccessListener(querySnapshot -> {
                                            List<Pair<String, Double>> matchedVendors = new ArrayList<>();
                                            for (QueryDocumentSnapshot doc : querySnapshot) {
                                                HashMap<String, Object> filters = (HashMap<String, Object>) doc.get("vendorFilters");
                                                if (filters != null) {
                                                    boolean matchesAll = true;
                                                    for (String filter : selectedFilters) {
                                                        Object value = filters.get(filter);
                                                        if (!(value instanceof Boolean) || !(Boolean) value) {
                                                            matchesAll = false;
                                                            break;
                                                        }
                                                    }
                                                    if (matchesAll) {
                                                        matchedVendors.add(new Pair<>(doc.getId(), null));
                                                    }
                                                }
                                            }
                                            showVendorResultsDialog(matchedVendors, null);
                                        });
                            }
                        }
                    })
                    .addOnFailureListener(e -> {
                        firestore.collection("Vendors").get()
                                .addOnSuccessListener(querySnapshot -> {
                                    List<Pair<String, Double>> matchedVendors = new ArrayList<>();
                                    for (QueryDocumentSnapshot doc : querySnapshot) {
                                        HashMap<String, Object> filters = (HashMap<String, Object>) doc.get("vendorFilters");
                                        if (filters != null) {
                                            boolean matchesAll = true;
                                            for (String filter : selectedFilters) {
                                                Object value = filters.get(filter);
                                                if (!(value instanceof Boolean) || !(Boolean) value) {
                                                    matchesAll = false;
                                                    break;
                                                }
                                            }
                                            if (matchesAll) {
                                                matchedVendors.add(new Pair<>(doc.getId(), null));
                                            }
                                        }
                                    }
                                    showVendorResultsDialog(matchedVendors, null);
                                });
                    });

            dialog.dismiss();
        });

        dialog.show();
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
                                        Toast.makeText(MainActivity.this,
                                                "Vendor address not available",
                                                Toast.LENGTH_SHORT).show();
                                    }
                                });
                            }
                        });
            }
        };

        vendorsRecyclerView.setAdapter(adapter);

        Button closeButton = dialogView.findViewById(R.id.close_button);
        AlertDialog dialog = builder.create();

        closeButton.setOnClickListener(v -> dialog.dismiss());
        dialog.show();
    }





//    private void showVendorResultsDialog(List<Pair<String, Double>> vendors, LatLng userLocation) {
//        AlertDialog.Builder builder = new AlertDialog.Builder(this);
//        View dialogView = LayoutInflater.from(this).inflate(R.layout.dialog_vendor_results, null);
//        builder.setView(dialogView);
//
//        RecyclerView vendorsRecyclerView = dialogView.findViewById(R.id.vendorsRecyclerView);
//        vendorsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
//        VendorAdapter adapter = new VendorAdapter(vendors, firestore, userLocation);
//        vendorsRecyclerView.setAdapter(adapter);
//
//        Button closeButton = dialogView.findViewById(R.id.close_button);
//        AlertDialog dialog = builder.create();
//
//        closeButton.setOnClickListener(v -> dialog.dismiss());
//        dialog.show();
//    }



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

        JsonArrayRequest request = new JsonArrayRequest(Request.Method.GET, url, null,
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
                error -> Toast.makeText(this, "Geocoding failed for " + address, Toast.LENGTH_SHORT).show());
        queue.add(request);
    }




    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainActivity.this, targetActivity);
        startActivity(intent);
    }
}

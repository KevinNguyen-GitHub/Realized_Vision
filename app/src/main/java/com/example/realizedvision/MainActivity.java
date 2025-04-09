package com.example.realizedvision;

import android.app.AlertDialog;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
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
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
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

            // Firestore query to match filters
            firestore.collection("Vendors")
                    .get()
                    .addOnSuccessListener(querySnapshot -> {
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
                                    //for now make it so that it prints in the terminal
                                    Log.d("MatchedVendor", "Vendor ID: " + doc.getId());
                                }
                            }
                        }
                    });

            dialog.dismiss();
        });

        dialog.show();
    }



    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainActivity.this, targetActivity);
        startActivity(intent);
    }
}

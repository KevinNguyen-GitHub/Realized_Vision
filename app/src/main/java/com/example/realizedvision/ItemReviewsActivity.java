package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ItemReviewsActivity extends AppCompatActivity {
    private RecyclerView reviewsRecyclerView;
    private ReviewAdapter reviewAdapter;
    private List<Map<String, Object>> reviewsList = new ArrayList<>();
    private FirebaseFirestore db = FirebaseFirestore.getInstance();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_item_reviews);

        ImageButton back_btn = findViewById(R.id.backButtonOrderHistory);
        back_btn.setOnClickListener(view -> navigateTo(MainActivity.class));

        reviewsRecyclerView = findViewById(R.id.itemReviewsRecyclerView);
        reviewsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        reviewAdapter = new ReviewAdapter(this, reviewsList);
        reviewsRecyclerView.setAdapter(reviewAdapter);

        String itemID = getIntent().getStringExtra("itemID");
        if (itemID != null) {
            fetchReviews(itemID);
        } else {
            Log.e("ItemReviewsActivity", "No itemID received!");
            Toast.makeText(this, "Error loading reviews.", Toast.LENGTH_SHORT).show();
            finish();
        }
    }

    private void fetchReviews(String itemID) {
        db.collection("Storefront").document(itemID).collection("reviews")
                .orderBy("timestamp", com.google.firebase.firestore.Query.Direction.DESCENDING)
                .get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    reviewsList.clear();
                    for (QueryDocumentSnapshot document : queryDocumentSnapshots) {
                        reviewsList.add(document.getData());
                    }
                    reviewAdapter.notifyDataSetChanged();
                    if (reviewsList.isEmpty()) {
                        Toast.makeText(this, "No reviews yet.", Toast.LENGTH_SHORT).show();
                    }
                })
                .addOnFailureListener(e -> {
                    Log.e("ItemReviewsActivity", "Error fetching reviews: " + e.getMessage());
                    Toast.makeText(this, "Error loading reviews.", Toast.LENGTH_SHORT).show();
                });
    }

    public void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(ItemReviewsActivity.this, targetActivity);
        startActivity(intent);
    }
}
package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.Query;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Displays the list of reviews for a single store-front item.
 *
 * Expects the launching Intent to supply an <b>“itemID”</b> extra.
 * If none is supplied, the Activity finishes with a toast.
 */
public class ItemReviewsActivity extends AppCompatActivity {

    private static final String TAG = "ItemReviewsActivity";

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();

    /* ───────────────────────── UI / data ──────────────────────── */
    private ReviewAdapter           adapter;
    private final List<Map<String, Object>> reviews = new ArrayList<>();

    /* ───────────────────────── lifecycle ──────────────────────── */
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_item_reviews);

        initViews();

        String itemId = getIntent().getStringExtra("itemID");
        if (itemId == null || itemId.isEmpty()) {
            toast("Error loading reviews.");       // user-visible
            Log.e(TAG, "Missing itemID extra");    // developer-visible
            finish();
            return;
        }
        fetchReviews(itemId);
    }

    /* ─────────────────────── view wiring ─────────────────────── */
    private void initViews() {
        ImageButton back = findViewById(R.id.backButtonOrderHistory);
        back.setOnClickListener(v -> startActivity(
                new Intent(this, MainActivity.class)));

        RecyclerView rv = findViewById(R.id.itemReviewsRecyclerView);
        rv.setLayoutManager(new LinearLayoutManager(this));
        adapter = new ReviewAdapter(this, reviews);
        rv.setAdapter(adapter);
    }

    /* ─────────────────── Firestore query ─────────────────────── */
    private void fetchReviews(@NonNull String itemId) {
        db.collection("Storefront").document(itemId)
                .collection("reviews")
                .orderBy("timestamp", Query.Direction.DESCENDING)
                .get()
                .addOnSuccessListener(qs -> {
                    reviews.clear();
                    for (QueryDocumentSnapshot doc : qs) {
                        reviews.add(doc.getData());
                    }
                    adapter.notifyDataSetChanged();
                    if (reviews.isEmpty()) toast("No reviews yet.");
                })
                .addOnFailureListener(e -> {
                    Log.e(TAG, "Error fetching reviews", e);
                    toast("Error loading reviews.");
                });
    }

    /* ───────────────────── helper ─────────────────────────────── */
    private void toast(String msg) {
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show();
    }
}

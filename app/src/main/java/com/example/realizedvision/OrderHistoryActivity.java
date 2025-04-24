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

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Shows every item from the current user’s previous orders.
 *
 * Firestore data model:
 *   Order History/{doc} → { userId, timestamp, items:[{itemID,…,quantity}] }
 */
public class OrderHistoryActivity extends AppCompatActivity {

    private static final String TAG = "OrderHistory";

    private final FirebaseAuth      auth = FirebaseAuth.getInstance();
    private final FirebaseFirestore db   = FirebaseFirestore.getInstance();

    private RecyclerView        rv;
    private OrderHistoryAdapter adapter;
    private final List<Item>    items = new ArrayList<>();

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_orderhistory);

        initViews();
        fetchOrderHistory();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void initViews() {
        rv = findViewById(R.id.orderHistoryRecyclerView);
        rv.setLayoutManager(new LinearLayoutManager(this));
        adapter = new OrderHistoryAdapter(this, items);
        rv.setAdapter(adapter);

        findViewById(R.id.backButtonOrderHistory)
                .setOnClickListener(v -> startActivity(
                        new Intent(this, SettingsActivity.class)));
    }

    /* ─────────────────────── Firestore fetch ───────────────────── */
    private void fetchOrderHistory() {
        String uid = auth.getUid();
        if (uid == null) { toast("Not signed in"); return; }

        db.collection("Order History")
                .whereEqualTo("userId", uid)
                .get()
                .addOnSuccessListener(this::populateList)
                .addOnFailureListener(e -> {
                    Log.e(TAG, "Fetch failed", e);
                    toast("Error loading order history.");
                });
    }

    private void populateList(@NonNull com.google.firebase.firestore.QuerySnapshot qs) {
        items.clear();

        for (QueryDocumentSnapshot doc : qs) {
            List<Map<String, Object>> arr = (List<Map<String, Object>>) doc.get("items");
            if (arr == null) continue;

            for (Map<String, Object> m : arr) {
                try {
                    String id   = (String) m.get("itemID");
                    String vid  = (String) m.get("vendorID");
                    String name = (String) m.get("name");
                    String desc = (String) m.get("description");
                    String cat  = (String) m.get("category");
                    String img  = (String) m.get("imageUrl");
                    Double price = m.get("price") instanceof Number
                            ? ((Number) m.get("price")).doubleValue()
                            : null;
                    int qty = m.get("quantity") instanceof Number
                            ? ((Number) m.get("quantity")).intValue()
                            : 1;

                    if (id == null || vid == null || name == null || price == null) {
                        Log.w(TAG, "Skipping incomplete item in " + doc.getId());
                        continue;
                    }
                    Item it = new Item(id, vid, desc, name, cat, price, img);
                    it.setQuantity(qty);
                    items.add(it);
                } catch (Exception e) {
                    Log.e(TAG, "Parse error in " + doc.getId(), e);
                }
            }
        }

        adapter.notifyDataSetChanged();
        if (items.isEmpty()) toast("No order history found.");
    }

    /* ─────────────────────── helpers ───────────────────────────── */
    private void toast(String m) { Toast.makeText(this, m, Toast.LENGTH_SHORT).show(); }
}

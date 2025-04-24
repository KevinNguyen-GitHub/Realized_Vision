package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class OrderHistoryActivity extends AppCompatActivity {

    private RecyclerView recyclerView;
    private OrderHistoryAdapter adapter;
    private List<Item> orderItemsList;
    private FirebaseFirestore firestore;
    private FirebaseAuth auth;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_orderhistory);

        recyclerView = findViewById(R.id.orderHistoryRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        orderItemsList = new ArrayList<>();
        adapter = new OrderHistoryAdapter(this, orderItemsList);
        recyclerView.setAdapter(adapter);

        firestore = FirebaseFirestore.getInstance();
        auth = FirebaseAuth.getInstance();

        ImageButton back_btn = findViewById(R.id.backButtonOrderHistory);
        back_btn.setOnClickListener(view -> navigateTo(SettingsActivity.class));

        fetchOrderHistory();
    }

    private void fetchOrderHistory() {
        FirebaseUser currentUser = auth.getCurrentUser();
        if (currentUser != null) {
            String userId = currentUser.getUid();
            firestore.collection("Order History")
                    .whereEqualTo("userId", userId)
                    .get()
                    .addOnSuccessListener(querySnapshot -> {
                        orderItemsList.clear();
                        for (QueryDocumentSnapshot document : querySnapshot) {
                            List<Map<String, Object>> itemsInOrder = (List<Map<String, Object>>) document.get("items");
                            if (itemsInOrder != null) {
                                for (Map<String, Object> itemMap : itemsInOrder) {
                                    try {
                                        String itemId = (String) itemMap.get("itemID");
                                        String vendorID = (String) itemMap.get("vendorID");
                                        String description = (String) itemMap.get("description");
                                        String name = (String) itemMap.get("name");
                                        String category = (String) itemMap.get("category");
                                        Double price = (Double) itemMap.get("price");
                                        String imageUrl = (String) itemMap.get("imageUrl");
                                        Number quantity = (Number) itemMap.get("quantity");
                                        int qty = (quantity != null) ? quantity.intValue() : 1;

                                        if (itemId != null && vendorID != null && name != null && price != null && imageUrl != null) {
                                            Item item = new Item(itemId, vendorID, description, name, category, price, imageUrl);
                                            item.setQuantity(qty);
                                            orderItemsList.add(item);
                                        } else {
                                            Log.w("OrderHistory", "Incomplete item data in order: " + document.getId());
                                        }
                                    } catch (Exception e) {
                                        Log.e("OrderHistory", "Error parsing item: " + e.getMessage());
                                    }
                                }
                            }
                        }
                        adapter.notifyDataSetChanged();
                        if (orderItemsList.isEmpty()) {
                            Toast.makeText(this, "No order history found.", Toast.LENGTH_SHORT).show();
                        }
                    })
                    .addOnFailureListener(e -> {
                        Log.e("OrderHistory", "Error fetching order history: " + e.getMessage());
                        Toast.makeText(this, "Error loading order history.", Toast.LENGTH_SHORT).show();
                    });
        }
    }

    public void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(OrderHistoryActivity.this, targetActivity);
        startActivity(intent);
    }
}
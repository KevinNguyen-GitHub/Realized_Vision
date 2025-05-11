package com.example.realizedvision;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.HashMap;
import java.util.List;

public class OrderHistoryAdapter extends RecyclerView.Adapter<OrderHistoryAdapter.ViewHolder> {

    private Context context;
    private List<Item> orderItems;

    public OrderHistoryAdapter(Context context, List<Item> orderItems) {
        this.context = context;
        this.orderItems = orderItems;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.history_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        Item item = orderItems.get(position);
        holder.itemNameTextView.setText(item.getName());
        holder.itemPriceTextView.setText(String.format("$%.2f", item.getPrice()));
        holder.itemQuantityTextView.setText("Qty: " + item.getQuantity());
        Glide.with(context)
                .load(item.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .error(R.drawable.ic_placeholder_image)
                .into(holder.itemImageView);

        holder.buyAgainButton.setOnClickListener(v -> {
            FirebaseUser currentUser = FirebaseAuth.getInstance().getCurrentUser();
            if (currentUser != null) {
                String userId = currentUser.getUid();
                addItemToCart(userId, item, success -> {
                    if (success) {
                        if (context instanceof OrderHistoryActivity) {
                            ((OrderHistoryActivity) context).navigateTo(CheckoutActivity.class);
                        }
                    } else {
                        Toast.makeText(context, "Error adding item to cart", Toast.LENGTH_SHORT).show();
                    }
                });
            } else {
                Toast.makeText(context, "User not logged in", Toast.LENGTH_SHORT).show();
            }
        });
        holder.requestRefundButton.setOnClickListener(v -> {
            Toast.makeText(context, "Requesting refund for " + item.getName(), Toast.LENGTH_SHORT).show();
        });
    }

    @Override
    public int getItemCount() {
        return orderItems.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        ImageView itemImageView;
        TextView itemNameTextView;
        TextView itemPriceTextView;
        TextView itemQuantityTextView;
        TextView buyAgainButton;
        TextView requestRefundButton;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            itemImageView = itemView.findViewById(R.id.item_image);
            itemNameTextView = itemView.findViewById(R.id.item_name);
            itemPriceTextView = itemView.findViewById(R.id.item_price);
            itemQuantityTextView = itemView.findViewById(R.id.item_quantity);
            buyAgainButton = itemView.findViewById(R.id.buy_again);
            requestRefundButton = itemView.findViewById(R.id.request_refund);
        }
    }

    private void addItemToCart(String userId, Item item, final OnCompleteListener callback) {
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        db.collection("Users").document(userId).collection("Shopping Cart")
                .document(item.getItemID()) // Use itemId as document ID
                .set(new HashMap<String, Object>() {{
                    put("description", item.getDescription());
                    put("favorite", item.isFavorite());
                    put("imageUrl", item.getImageUrl());
                    put("itemID", item.getItemID());
                    put("name", item.getName());
                    put("price", item.getPrice());
                    put("quantity", item.getQuantity());
                    put("vendorID", item.getVendorID());
                    // Add other relevant item details
                }})
                .addOnSuccessListener(aVoid -> callback.onComplete(true))
                .addOnFailureListener(e -> callback.onComplete(false));
    }

    interface OnCompleteListener {
        void onComplete(boolean success);
    }
}
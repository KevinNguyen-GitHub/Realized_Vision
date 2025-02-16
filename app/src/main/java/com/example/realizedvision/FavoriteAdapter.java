package com.example.realizedvision;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import com.bumptech.glide.Glide;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.firestore.FirebaseFirestore;
import java.util.List;

public class FavoriteAdapter extends RecyclerView.Adapter<FavoriteAdapter.FavoriteViewHolder> {
    private List<Favorite> favoriteList;

    public FavoriteAdapter(List<Favorite> favoriteList) {
        this.favoriteList = favoriteList;
    }

    @NonNull
    @Override
    public FavoriteViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.favorite_product, parent, false);
        return new FavoriteViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull FavoriteViewHolder holder, int position) {
        Favorite favorite = favoriteList.get(position);

        // Fetch product data using product_id
        fetchProductById(favorite.getItem_id(),
                product -> {
                    // Bind product data to views
                    holder.productName.setText(product.getProductName());
                    holder.productDescription.setText(product.getDescription());
                    Glide.with(holder.itemView.getContext())
                            .load(product.getImageUrl())
                            .placeholder(R.drawable.ic_placeholder_image) // Placeholder while loading
                            .error(R.drawable.ic_placeholder_image) // Error image if loading fails
                            .into(holder.productImage);
                },
                exception -> {
                    // Handle error fetching product
                    Log.e("FavoriteAdapter", "Error fetching product: " + exception.getMessage());
                }
        );

    }

    @Override
    public int getItemCount() {
        return favoriteList.size();
    }

    public static class FavoriteViewHolder extends RecyclerView.ViewHolder {
        ImageView productImage;
        TextView productName, productVendor, productDescription;

        public FavoriteViewHolder(@NonNull View favoriteView) {
            super(favoriteView);
            productImage = favoriteView.findViewById(R.id.product_image);
            productName = favoriteView.findViewById(R.id.product_name);
            productVendor = favoriteView.findViewById(R.id.product_vendor);
            productDescription = favoriteView.findViewById(R.id.product_description);
        }
    }

    // Method to fetch product by ID
    private void fetchProductById(String productId, OnSuccessListener<Product> onSuccess, OnFailureListener onFailure) {
        FirebaseFirestore.getInstance()
                .collection("Storefront")
                .document(productId)
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        Product product = documentSnapshot.toObject(Product.class);
                        if (product != null) {
                            onSuccess.onSuccess(product);
                        } else {
                            onFailure.onFailure(new Exception("Product data is null"));
                        }
                    } else {
                        onFailure.onFailure(new Exception("Product not found"));
                    }
                })
                .addOnFailureListener(onFailure);
    }
    private void fetchVendorById(String vendorId, OnSuccessListener<Vendor> onSuccess, OnFailureListener onFailure) {
        FirebaseFirestore.getInstance()
                .collection("Vendors")
                .document(vendorId)
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        Vendor vendor = documentSnapshot.toObject(Vendor.class); // Map Firestore document to Vendor object
                        if (vendor != null) {
                            onSuccess.onSuccess(vendor);
                        } else {
                            onFailure.onFailure(new Exception("Vendor data is null"));
                        }
                    } else {
                        onFailure.onFailure(new Exception("Vendor not found"));
                    }
                })
                .addOnFailureListener(onFailure);
    }

}
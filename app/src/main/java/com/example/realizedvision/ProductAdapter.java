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

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.util.List;

public class ProductAdapter extends RecyclerView.Adapter<ProductAdapter.ProductViewHolder>{
    private List<Product> productList;

    public ProductAdapter(List<Product> productList){
        this.productList = productList;
    }

    @NonNull
    @Override
    public ProductViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.feed_product, parent, false);
        return new ProductViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ProductViewHolder holder, int position) {
        Product product = productList.get(position);

        holder.productTitle.setText(product.getProductName());
        holder.productDescription.setText(product.getDescription());

        BigDecimal price = product.getPrice();
        String formattedPrice = NumberFormat.getCurrencyInstance().format(price);
        holder.productPrice.setText(formattedPrice);

        Glide.with(holder.itemView.getContext())
                .load(product.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image) // Placeholder while loading
                .error(R.drawable.ic_placeholder_image) // Error image if loading fails
                .into(holder.productImage);

        fetchVendorById(product.getVendorId(),
                vendor -> {
                    // Bind vendor data to views (if you have a vendor field in the layout)
                    holder.productVendor.setText(vendor.getCompanyName());
                },
                exception -> {
                    // Handle error fetching vendor
                    Log.e("ProductAdapter", "Error fetching vendor: " + exception.getMessage());
                }
        );
    }

    @Override
    public int getItemCount() {
        return productList.size();
    }

    public static class ProductViewHolder extends RecyclerView.ViewHolder{
        ImageView productImage;
        TextView productTitle, productPrice, productDescription, productVendor;

        public ProductViewHolder(@NonNull View itemView){
            super(itemView);
            productImage = itemView.findViewById(R.id.product_image);
            productTitle = itemView.findViewById(R.id.product_name);
            productPrice = itemView.findViewById(R.id.product_price);
            productDescription = itemView.findViewById(R.id.product_description);
            productVendor = itemView.findViewById(R.id.product_vendor);

        }
    }
    private void fetchVendorById(String vendorId, OnSuccessListener<Vendor> onSuccess, OnFailureListener onFailure) {
        FirebaseFirestore.getInstance()
                .collection("vendors")
                .document(vendorId)
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        Vendor vendor = documentSnapshot.toObject(Vendor.class);
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

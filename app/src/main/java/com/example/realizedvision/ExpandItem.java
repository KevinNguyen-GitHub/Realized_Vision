package com.example.realizedvision;

import android.os.Bundle;
import android.util.Log;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import com.bumptech.glide.Glide;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

public class ExpandItem extends AppCompatActivity {
    private ImageView productImage;
    private TextView productName, productPrice, productDescription;
    private Button addToCartButton, favoriteButton;
    private FirebaseFirestore firestore;
    private String itemId;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.expand_item);

        // Initialize views
        productImage = findViewById(R.id.product_image);
        productName = findViewById(R.id.product_name);
        productPrice = findViewById(R.id.product_price);
        productDescription = findViewById(R.id.product_description);
        addToCartButton = findViewById(R.id.add_to_cart_button);
        favoriteButton = findViewById(R.id.favorite_button);

        // Initialize Firestore
        firestore = FirebaseFirestore.getInstance();

        // Get the item ID from the intent
        itemId = getIntent().getStringExtra("item_id");
        if (itemId == null) {
            Toast.makeText(this, "Product not found", Toast.LENGTH_SHORT).show();
            finish(); // Close the activity if no item ID is provided
            return;
        }

        // Fetch and display product details
        fetchProductDetails(itemId);

        // Set up button click listeners
        addToCartButton.setOnClickListener(v -> addToCart());
        favoriteButton.setOnClickListener(v -> toggleFavorite());
    }

    private void fetchProductDetails(String itemId) {
        firestore.collection("Storefront")
                .document(itemId)
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        // Map the document to an Item object
                        Item item = documentSnapshot.toObject(Item.class);
                        if (item != null) {
                            // Bind data to views
                            productName.setText(item.getName());
                            productPrice.setText(String.format("$%.2f", item.getPrice()));
                            productDescription.setText(item.getDescription());

                            // Load image using Glide
                            Glide.with(this)
                                    .load(item.getImageUrl())
                                    .placeholder(R.drawable.ic_placeholder_image)
                                    .into(productImage);

                            // Update favorite button text based on favorite status
                            if (item.isFavorite()) {
                                favoriteButton.setText("Remove from Favorites");
                            } else {
                                favoriteButton.setText("Add to Favorites");
                            }
                        }
                    } else {
                        Toast.makeText(this, "Product not found", Toast.LENGTH_SHORT).show();
                        finish(); // Close the activity if the product doesn't exist
                    }
                })
                .addOnFailureListener(e -> {
                    Log.e("ProductDetailsActivity", "Error fetching product details", e);
                    Toast.makeText(this, "Failed to fetch product details", Toast.LENGTH_SHORT).show();
                });
    }

    private void addToCart() {

        Toast.makeText(this, "Added to cart", Toast.LENGTH_SHORT).show();
    }

    private void toggleFavorite() {
        Toast.makeText(this, "Toggled favorite", Toast.LENGTH_SHORT).show();
    }
}
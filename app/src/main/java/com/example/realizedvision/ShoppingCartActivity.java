package com.example.realizedvision;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.*;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Shows the user’s shopping-cart items, lets them favorite, increment,
 * decrement/remove, or open a quick “inspect” popup.
 * Checkout button routes to {@link CheckoutActivity}.
 */
public class ShoppingCartActivity extends AppCompatActivity
        implements ShoppingCartAdapter.OnItemClickListener {

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db   = FirebaseFirestore.getInstance();
    private final FirebaseAuth      auth = FirebaseAuth.getInstance();

    /* ───────────────────── data / adapter / ui ─────────────────── */
    private final List<Item> cart = new ArrayList<>();
    private ShoppingCartAdapter adapter;
    private RecyclerView        rv;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override
    protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_shopping_cart);

        if (auth.getCurrentUser() == null) { finish(); return; }

        initViews();
        loadCart();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void initViews() {
        rv = findViewById(R.id.cartRecyclerView);
        rv.setLayoutManager(new LinearLayoutManager(this));
        adapter = new ShoppingCartAdapter(this, cart);
        adapter.setOnItemClickListener(this);
        rv.setAdapter(adapter);

        int[] topIds = { R.id.home_icon, R.id.favorites_icon,
                R.id.messages_icon, R.id.profile_icon };
        Class<?>[] dest = { MainActivity.class, FavoritesActivity.class,
                MessagesActivity.class, ProfileActivity.class };

        for (int i = 0; i < topIds.length; i++)
            findViewById(topIds[i]).setOnClickListener(v -> nav(dest[i]));

        findViewById(R.id.favorites_button).setOnClickListener(v -> nav(FavoritesActivity.class));
        findViewById(R.id.checkoutButton)  .setOnClickListener(v -> nav(CheckoutActivity.class));
    }

    /* ─────────────────────── Firestore fetch ───────────────────── */
    private void loadCart() {
        String uid = auth.getUid();
        db.collection("Users").document(uid).collection("Shopping Cart")
                .get()
                .addOnSuccessListener(q -> {
                    cart.clear();
                    for (QueryDocumentSnapshot d : q) {
                        Item it = d.toObject(Item.class);
                        markFavoriteAsync(uid, it);
                    }
                })
                .addOnFailureListener(e -> toast("Failed to load cart: " + e.getMessage()));
    }

    private void markFavoriteAsync(String uid, @NonNull Item it) {
        db.collection("Users").document(uid)
                .collection("Favorites").document(it.getItemID())
                .get()
                .addOnSuccessListener(fav -> {
                    it.setFavorite(fav.exists());
                    cart.add(it);
                    adapter.notifyDataSetChanged();
                });
    }

    /* ──────────────────── adapter callbacks ───────────────────── */
    @Override public void onFavoriteClick(int pos)  { toggleFavorite(cart.get(pos)); }
    @Override public void onRemoveClick  (int pos)  { changeQty(cart.get(pos), pos,-1); }
    @Override public void onIncrementClick(int pos) { changeQty(cart.get(pos), pos,+1); }
    @Override public void onItemClick(int pos)      { showPopup(cart.get(pos), pos);   }

    /* ───────────────────── fav / qty helpers ───────────────────── */
    private void toggleFavorite(Item it) {
        String uid = auth.getUid();
        DocumentReference doc = db.collection("Users").document(uid)
                .collection("Favorites").document(it.getItemID());

        doc.get().addOnSuccessListener(snap -> {
            if (snap.exists()) doc.delete().addOnSuccessListener(v-> toast("Removed from favorites"));
            else               doc.set(it   ).addOnSuccessListener(v-> toast("Added to favorites"));
            it.setFavorite(!snap.exists());
            adapter.notifyDataSetChanged();
        });
    }

    private void changeQty(Item it, int pos, int delta) {
        String uid = auth.getUid();
        DocumentReference doc = db.collection("Users").document(uid)
                .collection("Shopping Cart").document(it.getItemID());

        doc.get().addOnSuccessListener(snap -> {
            long q = snap.getLong("quantity") == null ? 1 : snap.getLong("quantity");
            long newQ = q + delta;
            if (newQ <= 0) {
                doc.delete().addOnSuccessListener(v -> {
                    cart.remove(pos);
                    adapter.notifyItemRemoved(pos);
                    toast("Item removed");
                });
            } else {
                doc.update("quantity", newQ).addOnSuccessListener(v -> {
                    it.setQuantity(newQ);
                    adapter.notifyItemChanged(pos);
                });
            }
        });
    }

    /* ─────────────────────── item popup ───────────────────────── */
    private void showPopup(Item it, int pos) {
        View p = LayoutInflater.from(this).inflate(R.layout.expand_item, null);

        ImageView img   = p.findViewById(R.id.item_image);
        TextView  name  = p.findViewById(R.id.item_name);
        TextView  price = p.findViewById(R.id.item_price);
        TextView  desc  = p.findViewById(R.id.item_description);
        TextView  vendor= p.findViewById(R.id.item_vendor);

        name .setText(it.getName());
        price.setText(String.format("$%.2f", it.getPrice()));
        desc .setText(it.getDescription());

        Glide.with(this).load(it.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .into(img);

        p.findViewById(R.id.popup_cart_button)
                .setOnClickListener(v -> changeQty(it,pos,+1));
        p.findViewById(R.id.popup_favorite_button)
                .setOnClickListener(v -> toggleFavorite(it));
        vendor.setOnClickListener(v -> {
            Intent i = new Intent(this, StorefrontActivity.class);
            i.putExtra("vendorID", it.getVendorID());
            startActivity(i);
        });

        VendorUtils.fetchVendorName(it.getVendorID(), n ->
                vendor.setText(n == null ? "Vendor not found" : n));

        DisplayMetrics dm = getResources().getDisplayMetrics();
        PopupWindow win = new PopupWindow(p,
                (int) (dm.widthPixels * .8),
                (int) (dm.heightPixels * .7),
                true);
        win.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
        win.showAtLocation(rv, Gravity.CENTER, 0, 0);
    }

    /* ─────────────────────── helpers ───────────────────────────── */
    private void nav(Class<?> c){ startActivity(new Intent(this,c)); }
    private void toast(String m){ Toast.makeText(this,m,Toast.LENGTH_SHORT).show(); }
}

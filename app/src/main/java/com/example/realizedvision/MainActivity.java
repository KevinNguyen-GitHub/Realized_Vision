package com.example.realizedvision;

import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QueryDocumentSnapshot;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Home feed: shows storefront items, lets users favorite, add to cart,
 * open reviews, or jump to a vendor storefront.
 */
public class MainActivity extends AppCompatActivity
        implements ItemAdapter.OnItemClickListener {

    private static final String TAG = "MainActivity";

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();
    private final String uid = FirebaseAuth.getInstance().getCurrentUser() != null
            ? FirebaseAuth.getInstance().getCurrentUser().getUid()
            : null;

    /* ───────────────────────── data / adapter ─────────────────── */
    private final List<Item> items = new ArrayList<>();
    private ItemAdapter adapter;

    /* ───────────────────────── UI refs ─────────────────────────── */
    private RecyclerView recycler;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        if (uid == null) { toast("Not signed in"); finish(); return; }

        initRecycler();
        initTopIcons();
        fetchItems();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void initRecycler() {
        recycler = findViewById(R.id.mainRecyclerView);
        recycler.setLayoutManager(new LinearLayoutManager(this));
        adapter  = new ItemAdapter(this, items, true);
        adapter.setOnItemClickListener(this);
        recycler.setAdapter(adapter);
    }

    private void initTopIcons() {
        findViewById(R.id.profile_icon).setOnClickListener(v -> openProfile());
        findViewById(R.id.favorites_icon).setOnClickListener(v -> nav(FavoritesActivity.class));
        findViewById(R.id.messages_icon).setOnClickListener(v -> nav(MessagesActivity.class));
    }

    private void openProfile() {
        db.collection("Users").document(uid).get()
                .addOnSuccessListener(snap ->
                        nav(Boolean.TRUE.equals(snap.getBoolean("isVendor"))
                                ? StorefrontActivity.class
                                : ProfileActivity.class))
                .addOnFailureListener(e -> toast("Failed to load profile"));
    }

    /* ───────────────────────── data fetch ─────────────────────── */
    private void fetchItems() {
        db.collection("Storefront").get()
                .addOnSuccessListener(qs -> {
                    items.clear();
                    for (QueryDocumentSnapshot d : qs) {
                        Item it = d.toObject(Item.class);
                        items.add(it);
                    }
                    adapter.notifyDataSetChanged();
                })
                .addOnFailureListener(e -> Log.e(TAG, "fetchItems", e));
    }

    /* ───────────────── ItemAdapter callbacks ───────────────────── */
    @Override public void onFavoriteClick(int pos) {
        Item it = items.get(pos);
        it.setFavorite(!it.isFavorite());
        adapter.notifyItemChanged(pos);

        CollectionReference favRef = db.collection("Users")
                .document(uid)
                .collection("Favorites");
        favRef.document(it.getItemID()).get().addOnSuccessListener(doc -> {
            (doc.exists() ? favRef.document(it.getItemID()).delete()
                    : favRef.document(it.getItemID()).set(it))
                    .addOnSuccessListener(v -> toast(it.isFavorite()
                            ? "Added to favorites" : "Removed from favorites"))
                    .addOnFailureListener(e -> toast("Favorite update failed"));
        });
    }

    @Override public void onCartClick(int pos) {
        Item it = items.get(pos);
        CollectionReference cart = db.collection("Users")
                .document(uid).collection("Shopping Cart");

        cart.document(it.getItemID()).get().addOnSuccessListener(doc -> {
            if (doc.exists()) {
                Long q = doc.getLong("quantity");
                cart.document(it.getItemID()).update("quantity", (q == null ? 1 : q + 1));
            } else {
                it.setQuantity(1);
                cart.document(it.getItemID()).set(it);
            }
            toast("Added to cart");
        }).addOnFailureListener(e -> toast("Cart update failed"));
    }

    @Override public void onItemClick(int pos) { showItemPopup(items.get(pos)); }

    /* ───────────────────────── popup helper ─────────────────────── */
    private void showItemPopup(@NonNull Item it) {
        View p = LayoutInflater.from(this).inflate(R.layout.expand_item, null);

        ((TextView) p.findViewById(R.id.item_name)).setText(it.getName());
        ((TextView) p.findViewById(R.id.item_price))
                .setText(String.format("$%.2f", it.getPrice()));
        ((TextView) p.findViewById(R.id.item_description)).setText(it.getDescription());

        Glide.with(this).load(it.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .into((ImageView) p.findViewById(R.id.item_image));

        p.findViewById(R.id.popup_cart_button).setOnClickListener(v -> onCartClick(posOf(it)));
        p.findViewById(R.id.popup_favorite_button).setOnClickListener(v -> onFavoriteClick(posOf(it)));
        p.findViewById(R.id.showReviewsButton).setOnClickListener(v -> {
            Intent i = new Intent(this, ItemReviewsActivity.class);
            i.putExtra("itemID", it.getItemID());
            startActivity(i);
        });
        p.findViewById(R.id.item_vendor).setOnClickListener(v -> navToVendor(it.getVendorID()));

        int w = (int) (getResources().getDisplayMetrics().widthPixels  * .8);
        int h = (int) (getResources().getDisplayMetrics().heightPixels * .7);
        PopupWindow win = new PopupWindow(p, w, h, true);
        win.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
        win.showAtLocation(recycler, Gravity.CENTER, 0, 0);
    }

    private int posOf(Item it) { return items.indexOf(it); }

    /* ───────────────────────── nav helpers ──────────────────────── */
    private void nav(Class<?> c) { startActivity(new Intent(this, c)); }
    private void navToVendor(String vid) {
        Intent i = new Intent(this, StorefrontActivity.class);
        i.putExtra("vendorID", vid);
        startActivity(i);
    }

    /* ───────────────────────── utils ────────────────────────────── */
    private void toast(String msg) { Toast.makeText(this, msg, Toast.LENGTH_SHORT).show(); }
}

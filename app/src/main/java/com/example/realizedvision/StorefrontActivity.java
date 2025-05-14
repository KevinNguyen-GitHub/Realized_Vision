package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.*;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Vendor-side storefront screen.
 *
 *  • Lists every item where <code>vendorID == currentUser</code><br>
 *  • Lets the vendor add / delete items in-place.<br>
 *  • Tapping the address opens {@link MapActivity}.<br>
 *  • Top-bar icons route to core areas.<br>
 */
public class StorefrontActivity extends AppCompatActivity {

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseAuth      auth = FirebaseAuth.getInstance();
    private final FirebaseFirestore db   = FirebaseFirestore.getInstance();

    /* ───────────────────────── UI / data ───────────────────────── */
    private RecyclerView        rv;
    private ItemAdapter         adapter;
    private final List<Item>    items = new ArrayList<>();
    private TextView            tvName, tvAddress;
    private String              vendorId;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_vendor_profile);

        if (auth.getCurrentUser() == null) { finish(); return; }
        vendorId = auth.getUid();

        initViews();
        listenForItems();
        loadVendorMeta();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void initViews() {
        tvName    = findViewById(R.id.profile_name);
        tvAddress = findViewById(R.id.vendor_address);

        rv = findViewById(R.id.recyclerView);
        rv.setLayoutManager(new LinearLayoutManager(this));
        adapter = new ItemAdapter(this, items, /*filterable*/ false);
        rv.setAdapter(adapter);

        // nav icons
        int[] ids   = { R.id.home_icon, R.id.favorites_icon, R.id.messages_icon,
                R.id.profile_icon, R.id.settings_icon };
        Class<?>[] dest = { MainActivity.class, FavoritesActivity.class,
                MessagesActivity.class, StorefrontActivity.class,
                SettingsActivity.class };
        for (int i = 0; i < ids.length; i++)
            findViewById(ids[i]).setOnClickListener(v -> nav(dest[i]));

        findViewById(R.id.add_button)   .setOnClickListener(v -> showAddDialog());
        findViewById(R.id.delete_button).setOnClickListener(v -> showDeleteDialog());
    }

    /* ─────────────────────── Firestore live-load ───────────────── */
    private void listenForItems() {
        db.collection("Storefront")
                .whereEqualTo("vendorID", vendorId)
                .addSnapshotListener((@Nullable QuerySnapshot snap,
                                      @Nullable FirebaseFirestoreException e) -> {
                    if (e != null) return;
                    items.clear();
                    for (DocumentSnapshot d : snap.getDocuments())
                        items.add(d.toObject(Item.class));
                    adapter.notifyDataSetChanged();
                });
    }

    /* ───────────────────── vendor meta (name/address) ──────────── */
    private void loadVendorMeta() {
        db.collection("Vendors").document(vendorId).get()
                .addOnSuccessListener(doc -> {
                    tvName.setText(doc.getString("companyName"));
                    String addr = doc.getString("address");
                    if (addr != null && !addr.isEmpty()) {
                        tvAddress.setText(addr);
                        tvAddress.setVisibility(View.VISIBLE);
                        tvAddress.setOnClickListener(v -> {
                            Intent i = new Intent(this, MapActivity.class);
                            i.putExtra("selectedAddress", addr);
                            startActivity(i);
                        });
                    }
                });
    }

    /* ───────────────────────── add item ───────────────────────── */
    private void showAddDialog() {
        View v = LayoutInflater.from(this)
                .inflate(R.layout.dialog_add_item, null);
        EditText name = v.findViewById(R.id.input_name);
        EditText desc = v.findViewById(R.id.input_description);
        EditText price= v.findViewById(R.id.input_price);

        new AlertDialog.Builder(this).setTitle("Add new item")
                .setView(v)
                .setPositiveButton("Add", (d, i) -> {
                    String n = name.getText().toString().trim();
                    String de= desc.getText().toString().trim();
                    String p = price.getText().toString().trim();
                    if (n.isEmpty()||de.isEmpty()||p.isEmpty()) { toast("All fields"); return; }

                    db.collection("Storefront")
                            .whereEqualTo("vendorID", vendorId)
                            .whereEqualTo("name", n)
                            .limit(1)
                            .get()
                            .addOnSuccessListener(q -> {
                                if (!q.isEmpty()) { toast("Name already exists"); return; }
                                Map<String,Object> m = new HashMap<>();
                                m.put("name", n);
                                m.put("description", de);
                                m.put("price", Double.parseDouble(p));
                                m.put("vendorID", vendorId);
                                m.put("itemID", db.collection("dummy").document().getId());
                                m.put("imageUrl", "");

                                db.collection("Storefront").document((String) m.get("itemID"))
                                        .set(m).addOnSuccessListener(vv -> toast("Added!"))
                                        .addOnFailureListener(er -> toast("Add failed"));
                            });
                })
                .setNegativeButton("Cancel", null)
                .show();
    }

    /* ─────────────────────── delete item ───────────────────────── */
    private void showDeleteDialog() {
        View v = LayoutInflater.from(this)
                .inflate(R.layout.dialog_delete_item, null);
        EditText input = v.findViewById(R.id.input_item_id);

        new AlertDialog.Builder(this).setTitle("Delete item")
                .setView(v)
                .setPositiveButton("Delete", (d, i) -> {
                    String txt = input.getText().toString().trim();
                    if (txt.isEmpty()) return;

                    // try by ID, else by name
                    DocumentReference ref = db.collection("Storefront").document(txt);
                    ref.get().addOnSuccessListener(doc -> {
                        if (doc.exists() && vendorId.equals(doc.getString("vendorID")))
                            ref.delete().addOnSuccessListener(vv -> toast("Deleted"));
                        else deleteByName(txt);
                    });
                })
                .setNegativeButton("Cancel", null)
                .show();
    }

    private void deleteByName(String name) {
        db.collection("Storefront")
                .whereEqualTo("vendorID", vendorId)
                .whereEqualTo("name", name)
                .get()
                .addOnSuccessListener(q -> {
                    if (q.isEmpty()) { toast("Item not found"); return; }
                    for (DocumentSnapshot d : q) d.getReference().delete();
                    toast("Deleted");
                });
    }

    /* ───────────────────── helper wrappers ────────────────────── */
    private void nav(Class<?> c){ startActivity(new Intent(this,c)); }
    private void toast(String m){ Toast.makeText(this,m,Toast.LENGTH_SHORT).show(); }
}

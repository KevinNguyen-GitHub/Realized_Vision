package com.example.realizedvision;

import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.FirebaseFirestore;

/**
 * Vendor-side home screen – lets the vendor add / delete storefront items
 * and navigate to messages or their public storefront page.
 *
 * Expects the current user to be authenticated as a vendor.
 */
public class MainVendorActivity extends AppCompatActivity {

    private static final String TAG = "MainVendorActivity";

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();
    private final FirebaseUser user    = FirebaseAuth.getInstance().getCurrentUser();

    /* ───────────────────────── UI refs ─────────────────────────── */
    private TextView tvCompanyName;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendormain);

        if (user == null) { toast("Not signed in"); finish(); return; }

        tvCompanyName = findViewById(R.id.main_vendor_name);

        findViewById(R.id.add_item_button)   .setOnClickListener(v -> showAddItemDialog());
        findViewById(R.id.delete_item_button).setOnClickListener(v -> showDeleteItemDialog());
        findViewById(R.id.messages_icon)     .setOnClickListener(v -> nav(MessagesActivity.class));
        findViewById(R.id.profile_icon)      .setOnClickListener(v -> nav(StorefrontActivity.class));

        fetchVendorName();
    }

    /* ───────────────────── vendor display name ─────────────────── */
    private void fetchVendorName() {
        db.collection("Vendors").document(user.getUid()).get()
                .addOnSuccessListener(doc -> {
                    String name = doc.getString("companyName");
                    tvCompanyName.setText(getString(
                            R.string.profile_name_format,
                            name == null ? "" : name, ""));
                })
                .addOnFailureListener(e -> toast("Failed to load profile"));
    }

    /* ───────────────────────── add item ─────────────────────────── */
    private void showAddItemDialog() {
        View v = LayoutInflater.from(this).inflate(R.layout.dialog_add_item, null);
        EditText etName  = v.findViewById(R.id.input_name);
        EditText etDesc  = v.findViewById(R.id.input_description);
        EditText etPrice = v.findViewById(R.id.input_price);
        EditText etCat   = v.findViewById(R.id.input_category);

        themedDialog("Add New Item", v, "Add", () -> {
            String n = etName.getText().toString().trim();
            String d = etDesc.getText().toString().trim();
            String p = etPrice.getText().toString().trim();
            String c = etCat.getText().toString().trim();
            if (n.isEmpty() || d.isEmpty() || p.isEmpty() || c.isEmpty()) {
                toast("All fields required"); return;
            }
            double price = Double.parseDouble(p);
            checkDuplicateAndAdd(n, d, price, c);
        });
    }

    private void checkDuplicateAndAdd(String n, String d, double price, String c) {
        db.collection("Storefront")
                .whereEqualTo("name", n)
                .whereEqualTo("vendorID", user.getUid())
                .get()
                .addOnSuccessListener(q -> {
                    if (q.isEmpty()) addItemToStorefront(n, d, price, c);
                    else toast("Item with this name already exists");
                })
                .addOnFailureListener(e -> toast("Error checking duplicates"));
    }

    private void addItemToStorefront(String n, String d, double price, String c) {
        CollectionReference ref = db.collection("Storefront");
        String id = ref.document().getId();

        ref.document(id).set(new Item.Builder(id, user.getUid())
                        .name(n).description(d).price(price)
                        .category(c).imageUrl("").build())
                .addOnSuccessListener(v -> toast("Item added"))
                .addOnFailureListener(e -> toast("Failed to add item"));
    }

    /* ─────────────────────── delete item ───────────────────────── */
    private void showDeleteItemDialog() {
        View v = LayoutInflater.from(this).inflate(R.layout.dialog_delete_item, null);
        EditText et = v.findViewById(R.id.input_item_id);

        themedDialog("Delete Item", v, "Delete", () -> {
            String key = et.getText().toString().trim();
            if (key.isEmpty()) { toast("Enter item ID or name"); return; }
            attemptDelete(key);
        });
    }

    private void attemptDelete(String key) {
        // Try by ID first
        DocumentReference doc = db.collection("Storefront").document(key);
        doc.get().addOnSuccessListener(snap -> {
            if (snap.exists()) {
                if (user.getUid().equals(snap.getString("vendorID")))
                    doc.delete().addOnSuccessListener(v -> toast("Item deleted"));
                else toast("Cannot delete someone else’s item.");
            } else deleteByName(key);
        });
    }

    private void deleteByName(String name) {
        db.collection("Storefront")
                .whereEqualTo("name", name)
                .whereEqualTo("vendorID", user.getUid())
                .get()
                .addOnSuccessListener(q -> {
                    if (q.isEmpty()) { toast("Item not found"); return; }
                    q.forEach(d -> d.getReference().delete());
                    toast("Item deleted");
                })
                .addOnFailureListener(e -> toast("Delete failed"));
    }

    /* ───────────────────── reusable dialog helper ───────────────── */
    private void themedDialog(String title, View body,
                              String positive, Runnable onPos) {
        AlertDialog d = new AlertDialog.Builder(this)
                .setTitle(title)
                .setView(body)
                .setPositiveButton(positive, (dia, w) -> onPos.run())
                .setNegativeButton("Cancel", null)
                .create();
        d.setOnShowListener(di -> {
            d.getButton(AlertDialog.BUTTON_POSITIVE).setTextColor(Color.BLACK);
            d.getButton(AlertDialog.BUTTON_NEGATIVE).setTextColor(Color.BLACK);
        });
        d.show();
    }

    /* ─────────────────────── utils / nav ───────────────────────── */
    private void nav(Class<?> c) { startActivity(new Intent(this, c)); }
    private void toast(String m)  { Toast.makeText(this, m, Toast.LENGTH_SHORT).show(); }
}

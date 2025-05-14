package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.*;

/**
 * The customer-side profile screen.
 *
 * • Shows the user’s full name
 * • Top-bar icons route to core areas
 * • “Storefront” button only opens if the user is flagged as a vendor
 */
public class ProfileActivity extends AppCompatActivity {

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db   = FirebaseFirestore.getInstance();
    private final FirebaseUser      user = FirebaseAuth.getInstance().getCurrentUser();

    /* ───────────────────────── UI refs ─────────────────────────── */
    private TextView tvName;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_profile);

        if (user == null) { finish(); return; }   // auth session expired?

        initViews();
        loadUserName();
    }

    /* ───────────────────────── view wiring ────────────────────── */
    private void initViews() {
        tvName = findViewById(R.id.profile_name);

        int[] topIcons = { R.id.home_icon, R.id.favorites_icon, R.id.messages_icon,
                R.id.profile_icon, R.id.settings_icon };
        Class<?>[] dests = { MainActivity.class, FavoritesActivity.class, MessagesActivity.class,
                ProfileActivity.class, SettingsActivity.class };

        for (int i = 0; i < topIcons.length; i++)
            findViewById(topIcons[i]).setOnClickListener(v -> nav(dests[i]));

        findViewById(R.id.storefrontButton).setOnClickListener(v -> openStorefrontIfVendor());
    }

    /* ───────────────────── data fetch helpers ─────────────────── */
    private void loadUserName() {
        db.collection("Users").document(user.getUid()).get()
                .addOnSuccessListener(snap -> {
                    if (!snap.exists()) { toast("User data not found."); return; }
                    String first = snap.getString("firstName");
                    String last  = snap.getString("lastName");
                    tvName.setText(getString(R.string.profile_name_format,
                            first == null ? "" : first,
                            last  == null ? "" : last));
                })
                .addOnFailureListener(e -> toast("Failed to load profile: " + e.getMessage()));
    }

    private void openStorefrontIfVendor() {
        db.collection("Users").document(user.getUid()).get()
                .addOnSuccessListener(snap -> {
                    if (!snap.exists()) { toast("User data not found."); return; }
                    boolean isVendor = Boolean.TRUE.equals(snap.getBoolean("isVendor"));
                    if (isVendor) nav(StorefrontActivity.class);
                    else          toast("Upgrade to vendor to use this feature.");
                })
                .addOnFailureListener(e -> toast("Failed: " + e.getMessage()));
    }

    /* ───────────────────── helper wrappers ────────────────────── */
    private void nav(Class<?> c) { startActivity(new Intent(this, c)); }
    private void toast(String m)  { Toast.makeText(this, m, Toast.LENGTH_SHORT).show(); }
}

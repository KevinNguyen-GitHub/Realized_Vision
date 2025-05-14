package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;

/**
 * Simple hub that routes the user to Home, Favorites, or their profile/storefront
 * from the top-bar icons shown on the Messages screen.
 */
public class MessagesActivity extends AppCompatActivity {

    private static final String TAG = "MessagesActivity";

    private final FirebaseFirestore db = FirebaseFirestore.getInstance();
    private FirebaseUser user;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_messages);

        user = FirebaseAuth.getInstance().getCurrentUser();
        if (user == null) {                  // session expired?
            toast("Please sign in again.");
            startActivity(new Intent(this, LoginActivity.class));
            finish();
            return;
        }

        initTopBar();
    }

    /* ───────────────────────── UI wiring ───────────────────────── */
    private void initTopBar() {
        findViewById(R.id.home_icon)      .setOnClickListener(v -> nav(MainActivity.class));
        findViewById(R.id.favorites_icon) .setOnClickListener(v -> nav(FavoritesActivity.class));
        findViewById(R.id.profile_icon)   .setOnClickListener(v -> openProfile());
    }

    private void openProfile() {
        db.collection("Users").document(user.getUid()).get()
                .addOnSuccessListener(snap -> {
                    if (!snap.exists()) { toast("User data not found"); return; }
                    boolean isVendor = Boolean.TRUE.equals(snap.getBoolean("isVendor"));
                    nav(isVendor ? StorefrontActivity.class : ProfileActivity.class);
                })
                .addOnFailureListener(e ->
                        toast("Failed to load profile: " + e.getMessage()));
    }

    /* ─────────────────────── helpers ───────────────────────────── */
    private void nav(Class<?> dest) {
        startActivity(new Intent(this, dest));
    }
    private void toast(String msg) { Toast.makeText(this, msg, Toast.LENGTH_SHORT).show(); }
}

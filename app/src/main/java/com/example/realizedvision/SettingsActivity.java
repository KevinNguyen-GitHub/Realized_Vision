package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.example.realizedvision.onboard.VendorOnboardActivity;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.*;

/**
 * Settings hub for both customers and vendors.
 *
 * • All nav buttons are wired in one place.
 * • “Add address” is blocked for vendors (they already have one).
 * • “Set vendor filters” only opens for vendor accounts.
 * • Back-arrow returns the user to the right profile/storefront.
 * • Logout clears the back-stack.
 */
public class SettingsActivity extends AppCompatActivity {

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseAuth      auth = FirebaseAuth.getInstance();
    private final FirebaseFirestore db   = FirebaseFirestore.getInstance();
    private FirebaseUser user;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override
    protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_settings);

        user = auth.getCurrentUser();
        if (user == null) { finish(); return; }

        wireTopButtons();
        wireNavButtons();
        wireLogout();
    }

    /* ─────────────────────── view wiring ──────────────────────── */
    private void wireTopButtons() {
        findViewById(R.id.backButtonChangePass).setOnClickListener(v -> {
            db.collection("Users").document(user.getUid()).get()
                    .addOnSuccessListener(snap -> {
                        boolean vendor = Boolean.TRUE.equals(snap.getBoolean("isVendor"));
                        nav(vendor ? StorefrontActivity.class : ProfileActivity.class);
                    })
                    .addOnFailureListener(e -> toast("Failed to load profile"));
        });

        findViewById(R.id.addAddressButton).setOnClickListener(v -> {
            db.collection("Users").document(user.getUid()).get()
                    .addOnSuccessListener(snap -> {
                        boolean vendor = Boolean.TRUE.equals(snap.getBoolean("isVendor"));
                        if (vendor) toast("You’ve already provided an address as a vendor.");
                        else         nav(AddAddressActivity.class);
                    })
                    .addOnFailureListener(e -> toast("Failed to check vendor status"));
        });

        findViewById(R.id.setVendorFiltersButton).setOnClickListener(v -> {
            db.collection("Users").document(user.getUid()).get()
                    .addOnSuccessListener(snap -> {
                        if (Boolean.TRUE.equals(snap.getBoolean("isVendor")))
                            nav(VendorFilterActivity.class);
                        else toast("You must be a vendor to set vendor filters.");
                    });
        });
    }

    private void wireNavButtons() {
        int[] ids   = { R.id.tcButton,   R.id.historyButton,   R.id.changeUsernameButton,
                R.id.changePasswordButton, R.id.notificationsButton, R.id.upgradeButton };
        Class<?>[] dest = { TermsAndConditionsActivity.class, OrderHistoryActivity.class,
                ChangeNameActivity.class, ChangePasswordActivity.class,
                NotificationsActivity.class, VendorOnboardActivity.class };

        for (int i = 0; i < ids.length; i++)
            findViewById(ids[i]).setOnClickListener(v -> nav(dest[i]));
    }

    private void wireLogout() {
        findViewById(R.id.btn_logout).setOnClickListener(v -> {
            auth.signOut();
            Intent i = new Intent(this, LoginActivity.class)
                    .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            startActivity(i);
            finish();
        });
    }

    /* ─────────────────────── helpers ──────────────────────────── */
    private void nav(Class<?> c) { startActivity(new Intent(this, c)); }
    private void toast(String m) { Toast.makeText(this, m, Toast.LENGTH_SHORT).show(); }
}

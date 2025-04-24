package com.example.realizedvision;

import android.os.Bundle;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.Switch;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.database.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Lets the user toggle which notifications they want, per transport.
 *
 * Stored in Realtime DB at
 *   /Users/{uid}/notificationPreferences/{key} = Boolean
 *
 * Missing keys fall back to:
 *   • true  for app_ / email_
 *   • false for sms_
 */
public class NotificationsActivity extends AppCompatActivity {

    private DatabaseReference prefsRef;
    private final Map<String, Switch> switchMap = new HashMap<>();

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override
    protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_notifications);

        String uid = FirebaseAuth.getInstance().getUid();
        if (uid == null) { finish(); return; }

        prefsRef = FirebaseDatabase.getInstance()
                .getReference("Users").child(uid).child("notificationPreferences");

        bindViews();
        loadOrInitPrefs();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void bindViews() {
        findViewById(R.id.backButtonNotification).setOnClickListener(v -> finish());

        // Bulk-bind switches to their keys
        addSwitch(R.id.switchAppPurchasNotis,      "app_purchases");
        addSwitch(R.id.switchAppMessageNotis,      "app_messages");
        addSwitch(R.id.switchAppReservationNotis,  "app_reservations");

        addSwitch(R.id.switchEmailPurchaseNotis,   "email_purchases");
        addSwitch(R.id.switchEmailMessageNotis,    "email_messages");
        addSwitch(R.id.switchEmailReservationNotis,"email_reservations");

        addSwitch(R.id.switchSMSPurchaseNotis,     "sms_purchases");
        addSwitch(R.id.switchSMSReservationNotis,  "sms_reservations");
    }

    private void addSwitch(int id, String key) {
        Switch s = findViewById(id);
        switchMap.put(key, s);
        s.setOnCheckedChangeListener((v, checked) ->
                prefsRef.child(key).setValue(checked));
    }

    /* ─────────────────────── preference load ───────────────────── */
    private void loadOrInitPrefs() {
        prefsRef.addListenerForSingleValueEvent(new ValueEventListener() {
            @Override public void onDataChange(DataSnapshot snap) {
                if (!snap.exists()) writeDefaults();          // first run
                switchMap.forEach((k, sw) -> {
                    Boolean v = snap.child(k).getValue(Boolean.class);
                    sw.setChecked(v != null ? v : !k.startsWith("sms_"));
                });
            }
            @Override public void onCancelled(DatabaseError e) { /* no-op */ }
        });
    }

    private void writeDefaults() {
        Map<String, Object> def = new HashMap<>();
        switchMap.keySet().forEach(k ->
                def.put(k, !k.startsWith("sms_")));            // true for app/email, false for sms
        prefsRef.updateChildren(def);
    }
}

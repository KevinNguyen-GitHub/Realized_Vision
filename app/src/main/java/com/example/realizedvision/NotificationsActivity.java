package com.example.realizedvision;

import android.os.Bundle;
import android.util.Log;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.Switch;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.Map;

public class NotificationsActivity extends AppCompatActivity {

    // App notification switches
    private Switch appSwitchPurchases, appSwitchMessages, appSwitchReservations;
    // Email switches
    private Switch emailSwitchPurchases, emailSwitchMessages, emailSwitchReservations;
    // System alert switches
    private Switch switchSystemAlertsApp, switchSystemAlertsEmail;

    private DocumentReference userPrefsRef;
    private FirebaseUser currentUser;
    private NotificationHelper notificationHelper;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_notifications);

        // Initialize Firebase Firestore
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        notificationHelper = new NotificationHelper(this);

        if (currentUser != null) {
            userPrefsRef = FirebaseFirestore.getInstance()
                    .collection("Users")
                    .document(currentUser.getUid());
        }

        // Back button
        ImageButton backButton = findViewById(R.id.backButtonNotification);
        backButton.setOnClickListener(v -> finish());

        // Initialize switches
        initializeSwitches();

        // Load preferences
        loadNotificationPreferences();
    }

    private void initializeSwitches() {
        // App notification switches
        appSwitchPurchases = findViewById(R.id.switchAppPurchasNotis);
        appSwitchMessages = findViewById(R.id.switchAppMessageNotis);
        appSwitchReservations = findViewById(R.id.switchAppReservationNotis);

        // Email switches
        emailSwitchPurchases = findViewById(R.id.switchEmailPurchaseNotis);
        emailSwitchMessages = findViewById(R.id.switchEmailMessageNotis);
        emailSwitchReservations = findViewById(R.id.switchEmailReservationNotis);

        // System alert switches
        switchSystemAlertsApp = findViewById(R.id.switchSystemAlertsApp);
        switchSystemAlertsEmail = findViewById(R.id.switchSystemAlertsEmail);

        // Set up listeners
        setupSwitchListeners();
    }

    private void loadNotificationPreferences() {
        if (currentUser == null) return;

        userPrefsRef.get().addOnSuccessListener(document -> {
            if (document.exists() && document.contains("notificationPreferences")) {
                Map<String, Boolean> prefs = (Map<String, Boolean>) document.get("notificationPreferences");

                // App notifications
                appSwitchPurchases.setChecked(prefs.getOrDefault("app_purchases", true));
                appSwitchMessages.setChecked(prefs.getOrDefault("app_messages", true));
                appSwitchReservations.setChecked(prefs.getOrDefault("app_reservations", true));

                // Email notifications
                emailSwitchPurchases.setChecked(prefs.getOrDefault("email_purchases", true));
                emailSwitchMessages.setChecked(prefs.getOrDefault("email_messages", true));
                emailSwitchReservations.setChecked(prefs.getOrDefault("email_reservations", true));

                // System alerts
                switchSystemAlertsApp.setChecked(prefs.getOrDefault("app_security", true));
                switchSystemAlertsEmail.setChecked(prefs.getOrDefault("email_security", true));
            }
        }).addOnFailureListener(e -> {
            Toast.makeText(this, "Failed to load preferences", Toast.LENGTH_SHORT).show();
            Log.e("NotificationsActivity", "Error loading preferences", e);
        });
    }

    private void setupSwitchListeners() {
        // App notifications
        appSwitchPurchases.setOnCheckedChangeListener(createSwitchListener("app_purchases"));
        appSwitchMessages.setOnCheckedChangeListener(createSwitchListener("app_messages"));
        appSwitchReservations.setOnCheckedChangeListener(createSwitchListener("app_reservations"));

        // Email notifications
        emailSwitchPurchases.setOnCheckedChangeListener(createSwitchListener("email_purchases"));
        emailSwitchMessages.setOnCheckedChangeListener(createSwitchListener("email_messages"));
        emailSwitchReservations.setOnCheckedChangeListener(createSwitchListener("email_reservations"));

        // System alerts
        switchSystemAlertsApp.setOnCheckedChangeListener(createSwitchListener("app_security"));
        switchSystemAlertsEmail.setOnCheckedChangeListener(createSwitchListener("email_security"));
    }

    private CompoundButton.OnCheckedChangeListener createSwitchListener(String preferenceKey) {
        return (buttonView, isChecked) -> {
            if (currentUser == null) return;

            // Use NotificationHelper to update preferences
            notificationHelper.updateNotificationPreference(preferenceKey, isChecked);

            // Optional: Show confirmation
            if (isChecked) {
                Toast.makeText(NotificationsActivity.this,
                        preferenceKey.replace("_", " ") + " enabled",
                        Toast.LENGTH_SHORT).show();
            }
        };
    }
}
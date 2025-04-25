package com.example.realizedvision;

import android.os.Bundle;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.Switch;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;
import com.google.firebase.database.DataSnapshot;
import com.google.firebase.database.DatabaseError;
import com.google.firebase.database.ValueEventListener;

import java.util.HashMap;

public class NotificationsActivity extends AppCompatActivity {

    private Switch appSwitchPurchases, appSwitchMessages, appSwitchReservations;
    private Switch emailSwitchPurchases, emailSwitchMessages, emailSwitchReservations;
    private Switch switchSystemAlertsApp, switchSystemAlertsEmail;


    private DatabaseReference databaseReference;
    private FirebaseUser currentUser;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_notifications);

        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            databaseReference = FirebaseDatabase.getInstance()
                    .getReference("Users").child(currentUser.getUid()).child("notificationPreferences");
        }

        // Back button functionality
        ImageButton backButton = findViewById(R.id.backButtonNotification);
        backButton.setOnClickListener(v -> finish());

        //App notification switches
        appSwitchPurchases = findViewById(R.id.switchAppPurchasNotis);
        appSwitchMessages = findViewById(R.id.switchAppMessageNotis);
        appSwitchReservations = findViewById(R.id.switchAppReservationNotis);
        //email switches
        emailSwitchPurchases = findViewById(R.id.switchEmailPurchaseNotis);
        emailSwitchMessages = findViewById(R.id.switchEmailMessageNotis);
        emailSwitchReservations = findViewById(R.id.switchEmailReservationNotis);
        //System alert switch
        switchSystemAlertsApp = findViewById(R.id.switchSystemAlertsApp);
        switchSystemAlertsEmail = findViewById(R.id.switchSystemAlertsEmail);



        // Fetch existing preferences
        loadNotificationPreferences();

        // Update preferences when switches are toggled
        setupSwitchListeners();
    }

    private void loadNotificationPreferences() {
        if (databaseReference != null) {
            databaseReference.addListenerForSingleValueEvent(new ValueEventListener() {
                @Override
                public void onDataChange(DataSnapshot snapshot) {
                    if (!snapshot.exists()) {
                        setDefaultPreferences(); // Writes defaults to Firebase
                        return;
                    }
                    if (snapshot.exists()) {
                        appSwitchPurchases.setChecked(snapshot.child("app_purchases").getValue(Boolean.class));
                        appSwitchMessages.setChecked(snapshot.child("app_messages").getValue(Boolean.class));
                        appSwitchReservations.setChecked(snapshot.child("app_reservations").getValue(Boolean.class));

                        emailSwitchPurchases.setChecked(snapshot.child("email_purchases").getValue(Boolean.class));
                        emailSwitchMessages.setChecked(snapshot.child("email_messages").getValue(Boolean.class));
                        emailSwitchReservations.setChecked(snapshot.child("email_reservations").getValue(Boolean.class));

                        switchSystemAlertsApp.setChecked(snapshot.child("app_security").getValue(Boolean.class));
                        switchSystemAlertsEmail.setChecked(snapshot.child("email_security").getValue(Boolean.class));

                    }
                }

                @Override
                public void onCancelled(DatabaseError error) {
                    // Handle database error
                }
            });
        }
        else{
            setDefaultPreferences();
        }
    }

    private void setSwitchState(Switch switchView, DataSnapshot snapshot, String key){
        if(snapshot.child(key).exists()){
            switchView.setChecked(snapshot.child(key).getValue(Boolean.class));
        }
        else{
            //default to enabled
            switchView.setChecked(true);
        }
    }

    private void setDefaultPreferences(){
        HashMap<String, Object> defaults = new HashMap<>();

        //App notis
        defaults.put("app_purchases",true);
        defaults.put("app_messages", true);
        defaults.put("app_reservations", true);
        //email notis
        defaults.put("email_purchases", true);
        defaults.put("email_messages", true);
        defaults.put("email_reservations", true);
        //default sms notis to false
        defaults.put("app_security", true);
        defaults.put("email_security", true);

        databaseReference.updateChildren(defaults);

        // Update UI for defaults
        appSwitchPurchases.setChecked(true);
        appSwitchMessages.setChecked(true);
        appSwitchReservations.setChecked(true);

        emailSwitchPurchases.setChecked(true);
        emailSwitchMessages.setChecked(true);
        emailSwitchReservations.setChecked(true);

        switchSystemAlertsApp.setChecked(true);
        switchSystemAlertsEmail.setChecked(true);

    }
    private void setupSwitchListeners() {
        //app noti listeners
        appSwitchPurchases.setOnCheckedChangeListener(createSwitchListener("app_purchases"));
        appSwitchMessages.setOnCheckedChangeListener(createSwitchListener("app_messages"));
        appSwitchReservations.setOnCheckedChangeListener(createSwitchListener("app_reservations"));
        //email noti listeners
        emailSwitchPurchases.setOnCheckedChangeListener(createSwitchListener("email_purchases"));
        emailSwitchMessages.setOnCheckedChangeListener(createSwitchListener("email_messages"));
        emailSwitchReservations.setOnCheckedChangeListener(createSwitchListener("email_reservations"));
        //system notis listeners
        switchSystemAlertsApp.setOnCheckedChangeListener(createSwitchListener("app_security"));
        switchSystemAlertsEmail.setOnCheckedChangeListener(createSwitchListener("email_security"));
    }

    private CompoundButton.OnCheckedChangeListener createSwitchListener(final String preferenceKey) {
        return (buttonView, isChecked) -> {
            HashMap<String, Object> update = new HashMap<>();
            update.put(preferenceKey, isChecked);
            databaseReference.updateChildren(update);
        };
    }
}





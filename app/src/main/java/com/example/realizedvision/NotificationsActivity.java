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

    private Switch switch1, switch2, switch3;
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
                    .getReference("Users").child(currentUser.getUid()).child("notifications");
        }

        // Back button functionality
        ImageButton backButton = findViewById(R.id.backButtonNotification);
        backButton.setOnClickListener(v -> finish());

        // Initialize switches
        switch1 = findViewById(R.id.switch2);
        switch2 = findViewById(R.id.switch4);
        switch3 = findViewById(R.id.switch3);

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
                    if (snapshot.exists()) {
                        switch1.setChecked(snapshot.child("preference1").getValue(Boolean.class));
                        switch2.setChecked(snapshot.child("preference2").getValue(Boolean.class));
                        switch3.setChecked(snapshot.child("preference3").getValue(Boolean.class));
                    }
                }

                @Override
                public void onCancelled(DatabaseError error) {
                    // Handle database error
                }
            });
        }
    }

    private void setupSwitchListeners() {
        CompoundButton.OnCheckedChangeListener listener = (buttonView, isChecked) -> {
            HashMap<String, Object> updates = new HashMap<>();
            updates.put("preference1", switch1.isChecked());
            updates.put("preference2", switch2.isChecked());
            updates.put("preference3", switch3.isChecked());

            databaseReference.updateChildren(updates);
        };

        switch1.setOnCheckedChangeListener(listener);
        switch2.setOnCheckedChangeListener(listener);
        switch3.setOnCheckedChangeListener(listener);
    }
}





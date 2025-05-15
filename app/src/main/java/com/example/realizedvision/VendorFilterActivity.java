package com.example.realizedvision;

import android.content.res.ColorStateList;
import android.graphics.Color;
import android.os.Bundle;
import android.widget.CompoundButton;
import android.widget.ImageButton;
import android.widget.Switch;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.SetOptions;

import java.util.HashMap;
import java.util.Map;

public class VendorFilterActivity extends AppCompatActivity {

    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private DocumentReference vendorDocRef;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_vendor_filter);

        firestore = FirebaseFirestore.getInstance();
        currentUser = FirebaseAuth.getInstance().getCurrentUser();

        if (currentUser == null) {
            Toast.makeText(this, "User not logged in", Toast.LENGTH_SHORT).show();
            finish();
            return;
        }

        String vendorId = currentUser.getUid();
        vendorDocRef = firestore.collection("Vendors").document(vendorId);

        //Back button
        ImageButton backButton = findViewById(R.id.backButtonVendorFilter);
        backButton.setOnClickListener(v -> finish());

        setupFilterSwitch(R.id.woodworking, "woodworking");
        setupFilterSwitch(R.id.welding, "welding");
        setupFilterSwitch(R.id.programming, "programming");
        setupFilterSwitch(R.id.artist, "artist");
        setupFilterSwitch(R.id.mechanic, "mechanic");
        setupFilterSwitch(R.id.flooring, "flooring");
        setupFilterSwitch(R.id.graphicDesign, "graphicDesign");
        setupFilterSwitch(R.id.videoAndAutomation, "videoAndAutomation");


        Switch woodworkingSwitch = findViewById(R.id.woodworking);
        woodworkingSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            woodworkingSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("woodworking", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });


        Switch weldingSwitch = findViewById(R.id.welding);
        weldingSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            weldingSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("welding", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });



        Switch programmingSwitch = findViewById(R.id.programming);
        programmingSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            programmingSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("programming", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });



        Switch flooringSwitch = findViewById(R.id.flooring);
        flooringSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            flooringSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("flooring", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });



        Switch artistSwitch = findViewById(R.id.artist);
        artistSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            artistSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("artist", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });



        Switch mechanicSwitch = findViewById(R.id.mechanic);
        mechanicSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            mechanicSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("mechanic", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });


        Switch videoAndAutomationSwitch = findViewById(R.id.videoAndAutomation);
        videoAndAutomationSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            videoAndAutomationSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("videoAndAutomation", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });


        Switch graphicDesignSwitch = findViewById(R.id.graphicDesign);
        graphicDesignSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // 1. Change color
            int color = isChecked ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            graphicDesignSwitch.setTrackTintList(ColorStateList.valueOf(color));

            // 2. Update Firestore
            Map<String, Object> filterUpdate = new HashMap<>();
            filterUpdate.put("graphicDesign", isChecked);

            Map<String, Object> nestedUpdate = new HashMap<>();
            nestedUpdate.put("vendorFilters", filterUpdate);

            vendorDocRef.set(nestedUpdate, SetOptions.merge())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update woodworking", Toast.LENGTH_SHORT).show());
        });


        vendorDocRef.get().addOnSuccessListener(documentSnapshot -> {
            if (documentSnapshot.exists()) {
                Map<String, Object> filters = (Map<String, Object>) documentSnapshot.get("vendorFilters");

                if (filters != null) {
                    setSwitchState(R.id.woodworking, filters, "woodworking");
                    setSwitchState(R.id.welding, filters, "welding");
                    setSwitchState(R.id.programming, filters, "programming");
                    setSwitchState(R.id.flooring, filters, "flooring");
                    setSwitchState(R.id.artist, filters, "artist");
                    setSwitchState(R.id.mechanic, filters, "mechanic");
                    setSwitchState(R.id.graphicDesign, filters, "graphicDesign");
                    setSwitchState(R.id.videoAndAutomation, filters, "videoAndAutomation");
                }
            }
        });







    }

    private void setupFilterSwitch(int switchId, String filterKey) {
        Switch filterSwitch = findViewById(switchId);
        filterSwitch.setOnCheckedChangeListener((CompoundButton buttonView, boolean isChecked) -> {
            Map<String, Object> update = new HashMap<>();
            update.put("vendorFilters." + filterKey, isChecked);

            vendorDocRef.update(update)
                    .addOnSuccessListener(aVoid ->
                            Toast.makeText(this, filterKey + " set to " + isChecked, Toast.LENGTH_SHORT).show())
                    .addOnFailureListener(e ->
                            Toast.makeText(this, "Failed to update " + filterKey, Toast.LENGTH_SHORT).show());
        });
    }

    private void setSwitchState(int switchId, Map<String, Object> filters, String key) {
        Boolean isOn = (Boolean) filters.get(key);
        if (isOn != null) {
            Switch sw = findViewById(switchId);
            sw.setChecked(isOn);
            int color = isOn ? Color.parseColor("#4CAF50") : Color.LTGRAY;
            sw.setTrackTintList(ColorStateList.valueOf(color));
        }
    }
}











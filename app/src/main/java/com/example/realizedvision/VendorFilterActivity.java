package com.example.realizedvision;

import android.content.res.ColorStateList;
import android.graphics.Color;
import android.os.Bundle;
import android.widget.ImageButton;
import android.widget.Switch;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Lets vendors toggle which skills/keywords show up in Marketplace search.
 * <p>
 * The switches are persisted in
 * <pre>Vendors/{vendorId}.vendorFilters.{filterKey}=true/false</pre>
 */
public class VendorFilterActivity extends AppCompatActivity {

    private static final int ON_COLOR  = Color.parseColor("#4CAF50");
    private static final int OFF_COLOR = Color.LTGRAY;

    /* ---------------- Firebase --------------- */
    private final FirebaseUser       user = FirebaseAuth.getInstance().getCurrentUser();
    private final FirebaseFirestore  db   = FirebaseFirestore.getInstance();
    private DocumentReference        doc;

    /* ---------------- filters --------------- */
    private final String[] keys = {
            "woodworking", "welding", "programming", "artist",
            "mechanic", "flooring", "graphicDesign", "videoAndAutomation"
    };
    private final int[] switchIds = {
            R.id.woodworking, R.id.welding, R.id.programming, R.id.artist,
            R.id.mechanic, R.id.flooring, R.id.graphicDesign, R.id.videoAndAutomation
    };

    @Override protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_vendor_filter);

        if (user == null) { finish(); return; }
        doc = db.collection("Vendors").document(user.getUid());

        findViewById(R.id.backButtonVendorFilter).setOnClickListener(v -> finish());

        // wire all switches
        for (int i = 0; i < keys.length; i++)
            wireSwitch((Switch) findViewById(switchIds[i]), keys[i]);

        // load current state once
        doc.get().addOnSuccessListener(snap -> {
            Map<String,Object> f = (Map<String, Object>) snap.get("vendorFilters");
            if (f == null) return;
            for (int i = 0; i < keys.length; i++)
                syncSwitch((Switch)findViewById(switchIds[i]), (Boolean) f.get(keys[i]));
        });
    }

    /* ------------ helpers ------------ */
    private void wireSwitch(Switch sw, String key) {
        sw.setOnCheckedChangeListener((btn,isOn) -> {
            updateColor(sw,isOn);
            doc.update("vendorFilters."+key, isOn)
                    .addOnFailureListener(e ->
                            Toast.makeText(this,"Failed to update "+key,Toast.LENGTH_SHORT).show());
        });
    }

    private void syncSwitch(Switch sw, Boolean isOn) {
        if (isOn == null) return;
        sw.setChecked(isOn);
        updateColor(sw,isOn);
    }

    private void updateColor(Switch sw, boolean isOn){
        sw.setTrackTintList(ColorStateList.valueOf(isOn ? ON_COLOR : OFF_COLOR));
    }
}

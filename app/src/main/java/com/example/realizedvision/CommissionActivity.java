package com.example.realizedvision;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.PopupWindow;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QuerySnapshot;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Shows the user’s own commission requests and the vendor’s incoming requests.
 * A “Request” button lets the user create a new commission request in-app.
 *
 * <p>Uses preset IDs for demo purposes; wire these up to real auth/session
 * before production.</p>
 */
public class CommissionActivity extends AppCompatActivity {

    /* ──────────────────────────── demo constants ─────────────────────────── */
    private static final String PRESET_USER_ID   = "presetUserId";
    private static final String PRESET_VENDOR_ID = "presetVendorId";

    /* ─────────────────────────── Firebase / data ─────────────────────────── */
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();

    private final List<CommissionRequest> userRequests   = new ArrayList<>();
    private final List<CommissionRequest> vendorRequests = new ArrayList<>();

    private CommissionUserAdapter  userAdapter;
    private CommissionVendorAdapter vendorAdapter;

    /* ───────────────────────────── lifecycle ─────────────────────────────── */
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_commissions);

        initRecyclerViews();
        loadExistingRequests();

        findViewById(R.id.RequestButton).setOnClickListener(v -> showRequestPopup());
    }

    /* ─────────────────────────── recycler-view init ───────────────────────── */
    private void initRecyclerViews() {
        userAdapter   = new CommissionUserAdapter(this, userRequests);
        vendorAdapter = new CommissionVendorAdapter(this, vendorRequests);

        userAdapter.setOnOrderCanceledListener(this::refreshAdapters);
        vendorAdapter.setOnStatusChangedListener(this::refreshAdapters);

        RecyclerView userRv   = findViewById(R.id.userRequestsRecyclerView);
        RecyclerView vendorRv = findViewById(R.id.vendorRequestsRecyclerView);

        userRv.setLayoutManager(new LinearLayoutManager(this));
        vendorRv.setLayoutManager(new LinearLayoutManager(this));

        userRv.setAdapter(userAdapter);
        vendorRv.setAdapter(vendorAdapter);
    }

    private void refreshAdapters() {
        userAdapter.notifyDataSetChanged();
        vendorAdapter.notifyDataSetChanged();
    }

    /* ────────────────────────── popup / form flow ────────────────────────── */
    private void showRequestPopup() {
        View popup = LayoutInflater.from(this)
                .inflate(R.layout.popup_commission_form, null);

        PopupWindow window = new PopupWindow(
                popup,
                dpToPx(350),
                ViewGroup.LayoutParams.WRAP_CONTENT,
                true
        );
        window.setOutsideTouchable(true);
        window.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
        window.showAtLocation(findViewById(android.R.id.content), Gravity.CENTER, 0, 0);

        TextInputLayout nameLayout  = popup.findViewById(R.id.nameInputLayout);
        TextInputLayout typeLayout  = popup.findViewById(R.id.typeInputLayout);
        TextInputLayout sizeLayout  = popup.findViewById(R.id.sizeInputLayout);
        TextInputLayout styleLayout = popup.findViewById(R.id.styleInputLayout);

        TextInputEditText nameEt  = popup.findViewById(R.id.nameEditText);
        TextInputEditText typeEt  = popup.findViewById(R.id.typeEditText);
        TextInputEditText sizeEt  = popup.findViewById(R.id.sizeEditText);
        TextInputEditText styleEt = popup.findViewById(R.id.styleEditText);

        Button submit = popup.findViewById(R.id.submitButton);
        submit.setOnClickListener(v -> {
            /* simple inline validation */
            boolean ok = true;
            ok &= checkField(nameEt,  nameLayout,  "Name is required");
            ok &= checkField(typeEt,  typeLayout,  "Type is required");
            ok &= checkField(sizeEt,  sizeLayout,  "Size is required");
            ok &= checkField(styleEt, styleLayout, "Style is required");

            if (!ok) return;

            HashMap<String, Object> payload = new HashMap<>();
            payload.put("name",   nameEt.getText().toString().trim());
            payload.put("type",   typeEt.getText().toString().trim());
            payload.put("size",   sizeEt.getText().toString().trim());
            payload.put("style",  styleEt.getText().toString().trim());
            payload.put("status", "Pending");
            payload.put("vendorId",  PRESET_VENDOR_ID);
            payload.put("userId",    PRESET_USER_ID);
            payload.put("timestamp", System.currentTimeMillis());

            db.collection("CommissionRequests").add(payload)
                    .addOnSuccessListener(ref -> {
                        toast("Commission request submitted");
                        addLocalRequest(ref.getId(), payload);
                        window.dismiss();
                    })
                    .addOnFailureListener(e -> toast("Error submitting request"));
        });
    }

    private boolean checkField(TextInputEditText et,
                               TextInputLayout layout,
                               String err) {
        String txt = et.getText() == null ? "" : et.getText().toString().trim();
        if (TextUtils.isEmpty(txt)) {
            layout.setError(err);
            return false;
        }
        layout.setError(null);
        return true;
    }

    private void addLocalRequest(String docId, HashMap<String, Object> src) {
        CommissionRequest r = new CommissionRequest(
                (String) src.get("name"),
                (String) src.get("type"),
                (String) src.get("status"),
                (String) src.get("size"),
                (String) src.get("style"),
                /* budget */ "",
                /* note   */ ""
        );
        r.setVendorId(PRESET_VENDOR_ID);
        r.setUserId(PRESET_USER_ID);
        r.setTimestamp((Long) src.get("timestamp"));
        r.setDocumentId(docId);

        userRequests.add(r);
        vendorRequests.add(r);
        refreshAdapters();
    }

    /* ─────────────────────────── Firestore load ─────────────────────────── */
    private void loadExistingRequests() {
        db.collection("CommissionRequests")
                .get()
                .addOnSuccessListener(this::populateFromSnapshot)
                .addOnFailureListener(e -> toast("Failed to load commission requests"));
    }

    private void populateFromSnapshot(QuerySnapshot snap) {
        if (snap == null || snap.isEmpty()) return;

        userRequests.clear();
        vendorRequests.clear();

        for (DocumentSnapshot doc : snap) {
            CommissionRequest r = doc.toObject(CommissionRequest.class);
            if (r != null) {
                r.setDocumentId(doc.getId());
                userRequests.add(r);
                vendorRequests.add(r);
            }
        }
        refreshAdapters();
    }

    /* ───────────────────────────── utilities ────────────────────────────── */
    private void toast(String msg) {
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show();
    }

    private int dpToPx(int dp) {
        return (int) (dp * getResources().getDisplayMetrics().density);
    }
}

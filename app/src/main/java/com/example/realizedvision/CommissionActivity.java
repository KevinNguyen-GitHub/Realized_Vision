package com.example.realizedvision;

import static androidx.core.util.TypedValueCompat.dpToPx;

import android.content.Context;
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
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
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

public class CommissionActivity extends AppCompatActivity {

    // Preset IDs for demonstration
    private static final String PRESET_USER_ID = "presetUserId";
    private static final String PRESET_VENDOR_ID = "presetVendorId";

    private FirebaseFirestore firestore;

    // Lists for commission requests
    private List<CommissionRequest> userRequests = new ArrayList<>();
    private List<CommissionRequest> vendorRequests = new ArrayList<>();

    // Adapters
    private CommissionUserAdapter userAdapter;
    private CommissionVendorAdapter vendorAdapter;

    // RecyclerViews
    private RecyclerView userRequestsRecyclerView;
    private RecyclerView vendorRequestsRecyclerView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_commissions);

        firestore = FirebaseFirestore.getInstance();

        userRequestsRecyclerView = findViewById(R.id.userRequestsRecyclerView);
        vendorRequestsRecyclerView = findViewById(R.id.vendorRequestsRecyclerView);

        userAdapter = new CommissionUserAdapter(this, userRequests);
        vendorAdapter = new CommissionVendorAdapter(this, vendorRequests);

        // When the user cancels an order, refresh both adapters
        userAdapter.setOnOrderCanceledListener(() -> {
            userAdapter.notifyDataSetChanged();
            vendorAdapter.notifyDataSetChanged();
        });

        // When the vendor accepts or rejects an order, refresh both adapters
        vendorAdapter.setOnStatusChangedListener(() -> {
            userAdapter.notifyDataSetChanged();
            vendorAdapter.notifyDataSetChanged();
        });

        userRequestsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        vendorRequestsRecyclerView.setLayoutManager(new LinearLayoutManager(this));

        userRequestsRecyclerView.setAdapter(userAdapter);
        vendorRequestsRecyclerView.setAdapter(vendorAdapter);

        // Load existing requests from Firestore
        loadExistingRequests();

        // Commission Request button
        MaterialButton requestButton = findViewById(R.id.RequestButton);
        requestButton.setOnClickListener(v -> {

            // Inflate the popup layout
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            final View popupView = inflater.inflate(R.layout.popup_commission_form, null);

            // Convert 350dp to pixels for popup width
            int widthPx = dpToPx(350);

            // Create a PopupWindow
            final PopupWindow popupWindow = new PopupWindow(
                    popupView,
                    widthPx,
                    ViewGroup.LayoutParams.WRAP_CONTENT,
                    true  // focusable
            );

            // Dismiss on outside touch
            popupWindow.setOutsideTouchable(true);
            popupWindow.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));

            // Show popup at center of screen
            popupWindow.showAtLocation(findViewById(android.R.id.content), Gravity.CENTER, 0, 0);

            final TextInputLayout nameInputLayout = popupView.findViewById(R.id.nameInputLayout);
            final TextInputLayout typeInputLayout = popupView.findViewById(R.id.typeInputLayout);
            final TextInputLayout sizeInputLayout = popupView.findViewById(R.id.sizeInputLayout);
            final TextInputLayout styleInputLayout = popupView.findViewById(R.id.styleInputLayout);

            final TextInputEditText nameEditText = popupView.findViewById(R.id.nameEditText);
            final TextInputEditText typeEditText = popupView.findViewById(R.id.typeEditText);
            final TextInputEditText sizeEditText = popupView.findViewById(R.id.sizeEditText);
            final TextInputEditText styleEditText = popupView.findViewById(R.id.styleEditText);

            Button submitButton = popupView.findViewById(R.id.submitButton);
            submitButton.setOnClickListener(view -> {
                boolean valid = true;
                String name = nameEditText.getText().toString().trim();
                String type = typeEditText.getText().toString().trim();
                String size = sizeEditText.getText().toString().trim();
                String style = styleEditText.getText().toString().trim();

                // Basic field validation
                if (TextUtils.isEmpty(name)) {
                    nameInputLayout.setError("Name is required");
                    valid = false;
                } else {
                    nameInputLayout.setError(null);
                }
                if (TextUtils.isEmpty(type)) {
                    typeInputLayout.setError("Type is required");
                    valid = false;
                } else {
                    typeInputLayout.setError(null);
                }
                if (TextUtils.isEmpty(size)) {
                    sizeInputLayout.setError("Size is required");
                    valid = false;
                } else {
                    sizeInputLayout.setError(null);
                }
                if (TextUtils.isEmpty(style)) {
                    styleInputLayout.setError("Style is required");
                    valid = false;
                } else {
                    styleInputLayout.setError(null);
                }

                if (valid) {
                    // Write all fields, including vendorId, userId, and timestamp
                    HashMap<String, Object> commissionRequest = new HashMap<>();
                    commissionRequest.put("name", name);
                    commissionRequest.put("type", type);
                    commissionRequest.put("size", size);
                    commissionRequest.put("style", style);

                    // Default status is "Pending"
                    commissionRequest.put("status", "Pending");

                    // vendorId, userId, timestamp
                    commissionRequest.put("vendorId", PRESET_VENDOR_ID);
                    commissionRequest.put("userId", PRESET_USER_ID);
                    commissionRequest.put("timestamp", System.currentTimeMillis());

                    firestore.collection("CommissionRequests")
                            .add(commissionRequest)
                            .addOnSuccessListener((DocumentReference documentReference) -> {
                                Toast.makeText(CommissionActivity.this,
                                        "Commission request submitted", Toast.LENGTH_SHORT).show();

                                // Create a new CommissionRequest object
                                CommissionRequest newRequest = new CommissionRequest(
                                        name,
                                        type,
                                        "Pending",
                                        size,
                                        style,
                                        /* budget */ "",
                                        /* additionalNote */ ""
                                );
                                // Set vendorId, userId, and timestamp
                                newRequest.setVendorId(PRESET_VENDOR_ID);
                                newRequest.setUserId(PRESET_USER_ID);
                                newRequest.setTimestamp(System.currentTimeMillis());

                                // Set doc ID
                                newRequest.setDocumentId(documentReference.getId());

                                // Add the new request to both lists
                                userRequests.add(newRequest);
                                vendorRequests.add(newRequest);

                                userAdapter.notifyDataSetChanged();
                                vendorAdapter.notifyDataSetChanged();

                                popupWindow.dismiss();
                            })
                            .addOnFailureListener(e -> Toast.makeText(
                                    CommissionActivity.this,
                                    "Error submitting request",
                                    Toast.LENGTH_SHORT).show()
                            );
                }
            });
        });
    }

    /**
     * Reads existing CommissionRequests from Firestore and loads them into
     * userRequests and vendorRequests lists.
     */
    private void loadExistingRequests() {
        firestore.collection("CommissionRequests")
                .get()
                .addOnSuccessListener((QuerySnapshot snapshots) -> {
                    if (snapshots != null && !snapshots.isEmpty()) {
                        userRequests.clear();
                        vendorRequests.clear();

                        for (DocumentSnapshot doc : snapshots) {
                            CommissionRequest req = doc.toObject(CommissionRequest.class);
                            if (req != null) {
                                req.setDocumentId(doc.getId());

                                // Add to both lists, or separate if desired
                                userRequests.add(req);
                                vendorRequests.add(req);
                            }
                        }

                        // Refresh both adapters
                        userAdapter.notifyDataSetChanged();
                        vendorAdapter.notifyDataSetChanged();
                    }
                })
                .addOnFailureListener(e -> {
                    Toast.makeText(this,
                            "Failed to load commission requests", Toast.LENGTH_SHORT).show();
                });
    }

    // Utility method to convert dp to pixels
    private int dpToPx(int dp) {
        return (int) (dp * getResources().getDisplayMetrics().density);
    }
}

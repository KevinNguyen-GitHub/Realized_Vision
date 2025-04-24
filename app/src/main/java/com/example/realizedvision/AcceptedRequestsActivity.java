package com.example.realizedvision;

import android.os.Bundle;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QuerySnapshot;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AcceptedRequestsActivity extends AppCompatActivity {

    // Hard-coded vendor ID for demonstration
    private static final String PRESET_VENDOR_ID = "presetVendorId";

    private FirebaseFirestore firestore;

    private RecyclerView acceptedRecyclerView;
    private List<CommissionRequest> acceptedList;
    private AcceptedRequestsAdapter acceptedAdapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_accepted_requests);

        firestore = FirebaseFirestore.getInstance();

        acceptedRecyclerView = findViewById(R.id.acceptedRecyclerView);
        acceptedList = new ArrayList<>();
        acceptedAdapter = new AcceptedRequestsAdapter(this, acceptedList);

        acceptedRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        acceptedRecyclerView.setAdapter(acceptedAdapter);

        // Load any of these statuses: "Accepted", "In Progress", "Completed", "Delayed"
        loadNonRejectedRequests();
    }

    /**
     * Queries Firestore for CommissionRequests where vendorId == PRESET_VENDOR_ID
     * AND status is in ["Accepted", "In Progress", "Completed"].
     * This excludes "Rejected" or any other status not in the list.
     */
    private void loadNonRejectedRequests() {
        firestore.collection("CommissionRequests")
                .whereEqualTo("vendorId", PRESET_VENDOR_ID)
                .whereIn("status", Arrays.asList("Accepted", "In Progress", "Completed"))
                .get()
                .addOnSuccessListener(new OnSuccessListener<QuerySnapshot>() {
                    @Override
                    public void onSuccess(QuerySnapshot snapshots) {
                        acceptedList.clear();
                        if (!snapshots.isEmpty()) {
                            for (DocumentSnapshot doc : snapshots.getDocuments()) {
                                CommissionRequest req = doc.toObject(CommissionRequest.class);
                                if (req != null) {
                                    req.setDocumentId(doc.getId());
                                    acceptedList.add(req);
                                }
                            }
                        }
                        acceptedAdapter.notifyDataSetChanged();
                    }
                })
                .addOnFailureListener(new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Toast.makeText(AcceptedRequestsActivity.this,
                                "Failed to load requests", Toast.LENGTH_SHORT).show();
                    }
                });
    }
}

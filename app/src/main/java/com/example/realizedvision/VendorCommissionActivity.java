package com.example.realizedvision;

import android.os.Bundle;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.ArrayList;
import java.util.List;

public class VendorCommissionActivity extends AppCompatActivity {

    private FirebaseFirestore firestore;
    private List<CommissionRequest> vendorRequests = new ArrayList<>();
    private CommissionVendorAdapter vendorAdapter;
    private RecyclerView vendorRequestsRecyclerView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_commissions_vendor);

        firestore = FirebaseFirestore.getInstance();

        vendorRequestsRecyclerView = findViewById(R.id.vendorRequestsRecyclerView);
        vendorAdapter = new CommissionVendorAdapter(this, vendorRequests);

        vendorAdapter.setOnStatusChangedListener(vendorAdapter::notifyDataSetChanged);

        vendorRequestsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        vendorRequestsRecyclerView.setAdapter(vendorAdapter);

        loadVendorRequests();
    }

    private void loadVendorRequests() {
        firestore.collection("CommissionRequests")
                .get()
                .addOnSuccessListener(snapshots -> {
                    vendorRequests.clear();
                    if (snapshots != null && !snapshots.isEmpty()) {
                        for (com.google.firebase.firestore.QueryDocumentSnapshot doc : snapshots) {
                            CommissionRequest req = doc.toObject(CommissionRequest.class);
                            if (req != null) {
                                req.setDocumentId(doc.getId());
                                vendorRequests.add(req);
                            }
                        }
                        vendorAdapter.notifyDataSetChanged();
                    }
                })
                .addOnFailureListener(e -> Toast.makeText(this, "Failed to load requests", Toast.LENGTH_SHORT).show());
    }
}

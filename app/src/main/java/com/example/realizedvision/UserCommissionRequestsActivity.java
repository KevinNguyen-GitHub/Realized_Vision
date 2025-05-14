package com.example.realizedvision;

import android.os.Bundle;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import com.google.android.material.button.MaterialButton;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.QuerySnapshot;

import java.util.ArrayList;
import java.util.List;

public class UserCommissionRequestsActivity extends AppCompatActivity {

    private static final String PRESET_USER_ID = "user_123";
    private static final String PRESET_VENDOR_ID = "vendor_456";

    private FirebaseFirestore firestore;
    private List<CommissionRequest> userRequests = new ArrayList<>();
    private CommissionUserAdapter userAdapter;
    private RecyclerView userRequestsRecyclerView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_commissions_user);

        firestore = FirebaseFirestore.getInstance();

        userRequestsRecyclerView = findViewById(R.id.userRequestsRecyclerView);
        userAdapter = new CommissionUserAdapter(this, userRequests);

        userAdapter.setOnOrderCanceledListener(userAdapter::notifyDataSetChanged);

        userRequestsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        userRequestsRecyclerView.setAdapter(userAdapter);

        loadUserRequests();

    }

    private void loadUserRequests() {
        firestore.collection("CommissionRequests")
                .get()
                .addOnSuccessListener(snapshots -> {
                    userRequests.clear();
                    if (snapshots != null && !snapshots.isEmpty()) {
                        for (com.google.firebase.firestore.QueryDocumentSnapshot doc : snapshots) {
                            CommissionRequest req = doc.toObject(CommissionRequest.class);
                            if (req != null) {
                                req.setDocumentId(doc.getId());
                                userRequests.add(req);
                            }
                        }
                        userAdapter.notifyDataSetChanged();
                    }
                })
                .addOnFailureListener(e -> Toast.makeText(this, "Failed to load requests", Toast.LENGTH_SHORT).show());
    }
}

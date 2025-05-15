package com.example.realizedvision;

import android.os.Bundle;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.firestore.DocumentChange;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.ListenerRegistration;
import com.google.firebase.firestore.MetadataChanges;
import com.google.firebase.firestore.Query;
import com.google.firebase.firestore.QuerySnapshot;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.annotation.Nullable;

public class AcceptedRequestsActivity extends AppCompatActivity {

    public static final String EXTRA_VENDOR_ID = "vendorId";

    private static final List<String> VISIBLE_STATUSES =
            Arrays.asList("Accepted", "In Progress", "Completed", "Delayed");

    private FirebaseFirestore db;
    private ListenerRegistration registration;

    private RecyclerView recycler;
    private ProgressBar progress;
    private final List<CommissionRequest> requests = new ArrayList<>();
    private AcceptedRequestsAdapter adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_accepted_requests);

        db = FirebaseFirestore.getInstance();

        recycler  = findViewById(R.id.acceptedRecyclerView);

        adapter   = new AcceptedRequestsAdapter(this, requests);
        recycler.setLayoutManager(new LinearLayoutManager(this));
        recycler.setAdapter(adapter);
    }

    @Override
    protected void onStart() {
        super.onStart();
        String vendorId = getIntent().getStringExtra(EXTRA_VENDOR_ID);

        if (vendorId == null || vendorId.isEmpty()) {
            Toast.makeText(this, "Vendor ID missing.", Toast.LENGTH_LONG).show();
            finish();
            return;
        }
        observeRequests(vendorId);
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (registration != null) {
            registration.remove();
            registration = null;
        }
    }

    /** Live Firestore listener – keeps UI in sync without manual refresh. */
    private void observeRequests(String vendorId) {
        if (progress != null) progress.setVisibility(View.VISIBLE);

        Query query = db.collection("CommissionRequests")
                .whereEqualTo("vendorId", vendorId)
                .whereIn("status", VISIBLE_STATUSES);

        registration = query.addSnapshotListener(
                MetadataChanges.EXCLUDE,
                new EventListener<QuerySnapshot>() {
                    @Override
                    public void onEvent(@Nullable QuerySnapshot snapshots,
                                        @Nullable FirebaseFirestoreException e) {

                        if (progress != null) progress.setVisibility(View.GONE);

                        if (e != null) {
                            Toast.makeText(AcceptedRequestsActivity.this,
                                    "Error loading requests.", Toast.LENGTH_SHORT).show();
                            return;
                        }
                        if (snapshots == null) return;

                        for (DocumentChange dc : snapshots.getDocumentChanges()) {
                            CommissionRequest req =
                                    dc.getDocument().toObject(CommissionRequest.class);
                            if (req == null) continue;

                            req.setDocumentId(dc.getDocument().getId());

                            switch (dc.getType()) {
                                case ADDED:
                                    // avoid duplicates when Firestore sends a cached doc as ADDED
                                    if (indexOf(req.getDocumentId()) == -1) {
                                        requests.add(req);
                                    }
                                    break;

                                case MODIFIED:
                                    int iMod = indexOf(req.getDocumentId());
                                    if (iMod != -1) requests.set(iMod, req);
                                    break;

                                case REMOVED:
                                    int iRem = indexOf(req.getDocumentId());
                                    if (iRem != -1) requests.remove(iRem);
                                    break;
                            }
                        }
                        adapter.notifyDataSetChanged();
                    }
                });
    }

    /** Find local index by Firestore document id; −1 if not present. */
    private int indexOf(String docId) {
        for (int i = 0; i < requests.size(); i++) {
            if (docId.equals(requests.get(i).getDocumentId())) return i;
        }
        return -1;
    }
}

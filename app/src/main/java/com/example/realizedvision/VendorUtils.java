package com.example.realizedvision;

import android.util.Log;
import android.widget.TextView;
import com.google.firebase.firestore.FirebaseFirestore;

public class VendorUtils {

    public interface onVendorFetchedListener{
        void onVendorFetched(String vendorName);
    }
    public static void fetchVendorName(String vendorID, onVendorFetchedListener listener) {
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        db.collection("Vendors").document(vendorID).get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        String vendorName = documentSnapshot.getString("companyName");
                        listener.onVendorFetched(vendorName);
                    } else {
                        listener.onVendorFetched(null);
                    }
                })
                .addOnFailureListener(e -> {
                    Log.e("VendorUtils", "Failed to fetch vendor name", e);
                    listener.onVendorFetched(null);
                });
    }
}
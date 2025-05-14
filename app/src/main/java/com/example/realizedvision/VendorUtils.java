package com.example.realizedvision.util;

import android.util.Log;
import androidx.annotation.Nullable;
import com.google.firebase.firestore.FirebaseFirestore;

/**
 * Lightweight helper for looking up a vendor’s company name by id.
 *
 * You call:
 * <pre>
 * VendorUtils.fetchVendorName(vendorId) { name ->
 *     // name is null if unknown / error
 * }
 * </pre>
 */
public final class VendorUtils {

    private static final String TAG = "VendorUtils";
    private static final FirebaseFirestore db = FirebaseFirestore.getInstance();

    /** functional-style callback */
    public interface VendorCallback {
        /** @param name companyName or <code>null</code> if not found / error */
        void onResult(@Nullable String name);
    }

    private VendorUtils() {} // no instances

    public static void fetchVendorName(String vendorId, VendorCallback cb) {
        if (vendorId == null || vendorId.isEmpty()) {
            cb.onResult(null);
            return;
        }

        db.collection("Vendors").document(vendorId).get()
                .addOnSuccessListener(doc ->
                        cb.onResult(doc.exists() ? doc.getString("companyName") : null))
                .addOnFailureListener(e -> {
                    Log.e(TAG, "fetchVendorName failed", e);
                    cb.onResult(null);
                });
    }
}

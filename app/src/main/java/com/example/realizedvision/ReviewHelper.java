package com.example.realizedvision;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.*;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * Utility for reading / writing reviews under:
 *   Storefront/{itemID}/reviews/{reviewID}
 *
 * All public APIs take lightweight callback interfaces so callers stay
 * platform‐thread agnostic.
 */
public class ReviewHelper {

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();
    private final FirebaseAuth      auth = FirebaseAuth.getInstance();

    /* ───────────────────────── callbacks ───────────────────────── */
    public interface ExistingReviewCallback {
        void onFound(DocumentSnapshot doc);
        void onNotFound();
        void onError(String msg);
    }
    public interface ReviewSubmitCallback {
        void onSuccess();
        void onFailure(String msg);
    }

    /* ─────────────────────── public helpers ───────────────────── */
    public void findExistingReview(String itemId,
                                   ExistingReviewCallback cb) {
        FirebaseUser u = auth.getCurrentUser();
        if (u == null) { cb.onError("User not logged in"); return; }

        db.collection("Storefront").document(itemId)
                .collection("reviews")
                .whereEqualTo("userID", u.getUid())
                .limit(1)
                .get()
                .addOnSuccessListener(q -> {
                    if (q.isEmpty()) cb.onNotFound();
                    else             cb.onFound(q.getDocuments().get(0));
                })
                .addOnFailureListener(e -> cb.onError(e.getMessage()));
    }

    public void submitReview(String itemId,
                             float rating,
                             String text,
                             ReviewSubmitCallback cb) {
        FirebaseUser u = auth.getCurrentUser();
        if (u == null) { cb.onFailure("User not logged in"); return; }
        String uid = u.getUid();

        db.collection("Users").document(uid).get()
                .addOnSuccessListener(userDoc -> {
                    String display = userDoc.getString("firstName");
                    Map<String,Object> m = new HashMap<>();
                    m.put("userID",      uid);
                    m.put("displayName", display == null ? "Anonymous" : display);
                    m.put("rating",      rating);
                    m.put("text",        text);
                    m.put("timestamp",   new Date());

                    db.collection("Storefront").document(itemId)
                            .collection("reviews")
                            .add(m)
                            .addOnSuccessListener(r -> cb.onSuccess())
                            .addOnFailureListener(e -> cb.onFailure(e.getMessage()));
                })
                .addOnFailureListener(e -> cb.onFailure(e.getMessage()));
    }

    public void updateReview(String itemId,
                             String reviewId,
                             float rating,
                             String text,
                             ReviewSubmitCallback cb) {

        db.collection("Storefront").document(itemId)
                .collection("reviews").document(reviewId)
                .update(
                        "rating",    rating,
                        "text",      text,
                        "timestamp", new Date()
                )
                .addOnSuccessListener(r -> cb.onSuccess())
                .addOnFailureListener(e -> cb.onFailure(e.getMessage()));
    }
}

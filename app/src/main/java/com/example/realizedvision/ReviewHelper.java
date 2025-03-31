package com.example.realizedvision;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.DocumentSnapshot;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class ReviewHelper {
    private FirebaseFirestore firestore = FirebaseFirestore.getInstance();

    public interface ExistingReviewCallback {
        void onExistingReviewFound(DocumentSnapshot reviewDoc);

        void onNoExistingReview();

        void onError(String errorMessage);
    }

    public interface ReviewSubmitCallback {
        void onSuccess();

        void onFailure(String errorMessage);
    }

    public void checkForExistingReview(String itemId, final ReviewHelper.ExistingReviewCallback callback) {
        FirebaseUser user = FirebaseAuth.getInstance().getCurrentUser();
        if (user != null) {
            String userId = user.getUid();
            firestore.collection("Storefront")
                    .document(itemId)
                    .collection("reviews")
                    .whereEqualTo("userId", userId)
                    .get()
                    .addOnSuccessListener(queryDocumentSnapshots -> {
                        if (!queryDocumentSnapshots.isEmpty()) {
                            DocumentSnapshot reviewDoc = queryDocumentSnapshots.getDocuments().get(0);
                            callback.onExistingReviewFound(reviewDoc);
                        } else {
                            callback.onNoExistingReview();
                        }
                    })
                    .addOnFailureListener(e -> {
                        callback.onError("Error checking for existing review: " + e.getMessage());
                    });
        } else {
            callback.onError("User not logged in");
        }
    }

    public void submitReview(String itemId, float rating, String text, ReviewSubmitCallback callback) {
        FirebaseUser user = FirebaseAuth.getInstance().getCurrentUser();

        if (user != null) {
            String userId = user.getUid();

            Map<String, Object> review = new HashMap<>();
            review.put("userId", userId);
            review.put("rating", rating);
            review.put("text", text);
            review.put("timestamp", new Date());

            firestore.collection("Storefront").document(itemId).collection("reviews")
                    .add(review)
                    .addOnSuccessListener(documentReference -> {
                        callback.onSuccess();
                    })
                    .addOnFailureListener(e -> {
                        callback.onFailure("Error submitting review: " + e.getMessage());
                    });
        } else {
            callback.onFailure("User not logged in");
        }
    }

    public void updateReview(String itemId, String reviewId, float rating, String text, final ReviewSubmitCallback callback) {

        firestore.collection("Storefront")
                .document(itemId)
                .collection("reviews")
                .document(reviewId)
                .update(
                        "rating", rating,
                        "text", text,
                        "timestamp", new Date()
                )
                .addOnSuccessListener(aVoid -> {
                    callback.onSuccess();
                })
                .addOnFailureListener(e -> {
                    callback.onFailure("Error updating review: " + e.getMessage());
                });
    }
}

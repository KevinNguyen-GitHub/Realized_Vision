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

    public interface GetFirstNameCallback {
        void onFirstNameFetched(String firstName);

        void onFirstNameFetchFailed(String errorMessage);
    }

    public void checkForExistingReview(String itemID, final ReviewHelper.ExistingReviewCallback callback) {
        FirebaseUser user = FirebaseAuth.getInstance().getCurrentUser();
        if (user != null) {
            String userID = user.getUid();
            firestore.collection("Storefront")
                    .document(itemID)
                    .collection("reviews")
                    .whereEqualTo("userID", userID)
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

    public void getFirstName(String userID, final GetFirstNameCallback callback) {
        firestore.collection("Users").document(userID)
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        String firstName = documentSnapshot.getString("firstName");

                        if (firstName != null) {
                            callback.onFirstNameFetched(firstName);
                        } else {
                            callback.onFirstNameFetchFailed("First name field not found");
                        }
                    } else {
                        callback.onFirstNameFetchFailed("User document not found");
                    }
                })
                .addOnFailureListener(e -> {
                    callback.onFirstNameFetchFailed("Error fetching user data: " + e.getMessage());
                });
    }

    public void submitReview(String itemID, float rating, String text, final ReviewSubmitCallback callback) {
        FirebaseUser user = FirebaseAuth.getInstance().getCurrentUser();

        if (user != null) {
            String userID = user.getUid();

            getFirstName(userID, new GetFirstNameCallback() {
                @Override
                public void onFirstNameFetched(String firstName) {
                    String displayName = firstName != null ? firstName : "Anonymous";

                    Map<String, Object> review = new HashMap<>();
                    review.put("userID", userID);
                    review.put("displayName", displayName);
                    review.put("rating", rating);
                    review.put("text", text);
                    review.put("timestamp", new Date());

                    firestore.collection("Storefront").document(itemID).collection("reviews")
                            .add(review)
                            .addOnSuccessListener(documentReference -> {
                                callback.onSuccess();
                            })
                            .addOnFailureListener(e -> {
                                callback.onFailure("Error submitting review: " + e.getMessage());
                            });
                }

                @Override
                public void onFirstNameFetchFailed(String errorMessage) {
                    callback.onFailure(errorMessage);
                }
            });

        } else {
            callback.onFailure("User not logged in");
        }
    }

    public void updateReview(String itemID, String reviewID, float rating, String text, final ReviewSubmitCallback callback) {

        firestore.collection("Storefront")
                .document(itemID)
                .collection("reviews")
                .document(reviewID)
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
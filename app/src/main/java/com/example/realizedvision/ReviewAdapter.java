package com.example.realizedvision;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import java.util.List;
import java.util.Map;

public class ReviewAdapter extends RecyclerView.Adapter<ReviewAdapter.ReviewViewHolder> {

    private Context context;
    private List<Map<String, Object>> reviews;

    public ReviewAdapter(Context context, List<Map<String, Object>> reviews) {
        this.context = context;
        this.reviews = reviews;
    }

    @NonNull
    @Override
    public ReviewViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.item_review, parent, false);
        return new ReviewViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ReviewViewHolder holder, int position) {
        Map<String, Object> review = reviews.get(position);
        String userId = (String) review.get("userId");
        String displayName = (String) review.get("displayName");
        float rating = ((Number) review.get("rating")).floatValue();
        String text = (String) review.get("text");

        holder.reviewerNameTextView.setText("Review by: " + displayName);
        holder.reviewRatingTextView.setText("Rating: " + rating);
        holder.reviewDescriptionTextView.setText(text != null ? text : "");
    }

    @Override
    public int getItemCount() {
        return reviews.size();
    }

    public static class ReviewViewHolder extends RecyclerView.ViewHolder {
        TextView reviewerNameTextView;
        TextView reviewRatingTextView;
        TextView reviewDescriptionTextView;

        public ReviewViewHolder(@NonNull View itemView) {
            super(itemView);
            reviewerNameTextView = itemView.findViewById(R.id.reviewer_name);
            reviewRatingTextView = itemView.findViewById(R.id.review_rating);
            reviewDescriptionTextView = itemView.findViewById(R.id.review_description);
        }
    }
}

package com.example.realizedvision;

import android.content.Intent;
import android.util.Log;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.gms.maps.model.LatLng;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.List;

public class VendorAdapter extends RecyclerView.Adapter<VendorAdapter.VendorViewHolder> {
    private List<Pair<String, Double>> vendors;
    private FirebaseFirestore firestore;
    private LatLng userLocation;

    public VendorAdapter(List<Pair<String, Double>> vendors, FirebaseFirestore firestore, LatLng userLocation) {
        this.vendors = vendors;
        this.firestore = firestore;
        this.userLocation = userLocation;
    }

    @NonNull
    @Override
    public VendorViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_vendor, parent, false);
        return new VendorViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull VendorViewHolder holder, int position) {
        String vendorId = vendors.get(position).first;
        Double distance = vendors.get(position).second;

        firestore.collection("Vendors").document(vendorId).get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        String companyName = documentSnapshot.getString("companyName");
                        holder.vendorNameTextView.setText(companyName != null ? companyName : "Vendor");

                        if (distance != null) {
                            holder.vendorDistanceTextView.setText(String.format("%.2f miles away", distance));
                            holder.vendorDistanceTextView.setTextColor(ContextCompat.getColor(holder.itemView.getContext(), R.color.maroon));
                        } else {
                            holder.vendorDistanceTextView.setText("Distance unavailable");
                            holder.vendorDistanceTextView.setTextColor(ContextCompat.getColor(holder.itemView.getContext(), R.color.gray));
                        }
                    }
                })
                .addOnFailureListener(e -> {
                    holder.vendorNameTextView.setText("Vendor");
                    holder.vendorDistanceTextView.setText("Distance unavailable");
                    holder.vendorDistanceTextView.setTextColor(ContextCompat.getColor(holder.itemView.getContext(), R.color.gray));
                    holder.directionsButton.setEnabled(false);
                });
    }

    @Override
    public int getItemCount() {
        return vendors.size();
    }

    static class VendorViewHolder extends RecyclerView.ViewHolder {
        TextView vendorNameTextView;
        TextView vendorDistanceTextView;
        Button directionsButton;

        public VendorViewHolder(@NonNull View itemView) {
            super(itemView);
            vendorNameTextView = itemView.findViewById(R.id.vendorNameTextView);
            vendorDistanceTextView = itemView.findViewById(R.id.vendorDistanceTextView);
            directionsButton = itemView.findViewById(R.id.directionsButton);
        }
    }
}
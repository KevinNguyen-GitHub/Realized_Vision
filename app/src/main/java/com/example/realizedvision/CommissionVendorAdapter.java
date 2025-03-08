package com.example.realizedvision;

import android.content.Context;
import android.graphics.drawable.ColorDrawable;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.android.material.button.MaterialButton;

import java.util.List;

public class CommissionVendorAdapter extends RecyclerView.Adapter<CommissionVendorAdapter.ViewHolder> {

    private Context context;
    private List<CommissionRequest> requests;

    // Listener to notify when a status update occurs
    private OnStatusChangedListener statusChangedListener;

    public interface OnStatusChangedListener {
        void onStatusChanged();
    }

    public void setOnStatusChangedListener(OnStatusChangedListener listener) {
        this.statusChangedListener = listener;
    }

    public CommissionVendorAdapter(Context context, List<CommissionRequest> requests) {
        this.context = context;
        this.requests = requests;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        // Inflate the vendor item layout (e.g., item_request.xml)
        View view = LayoutInflater.from(context).inflate(R.layout.item_request, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        CommissionRequest request = requests.get(position);
        holder.orderTitle.setText(request.getName());
        holder.orderDetails.setText(request.getType() + " | " + request.getStatus());

        // Accept Order button click listener
        holder.checkmarkButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                if (request.getDocumentId() != null) {
                    FirebaseFirestore.getInstance().collection("CommissionRequests")
                            .document(request.getDocumentId())
                            .update("status", "Accepted")
                            .addOnSuccessListener(new OnSuccessListener<Void>() {
                                @Override
                                public void onSuccess(Void aVoid) {
                                    request.setStatus("Accepted");
                                    notifyItemChanged(position);
                                    Toast.makeText(context, "Order accepted", Toast.LENGTH_SHORT).show();
                                    if (statusChangedListener != null) {
                                        statusChangedListener.onStatusChanged();
                                    }
                                }
                            })
                            .addOnFailureListener(new OnFailureListener() {
                                @Override
                                public void onFailure(@NonNull Exception e) {
                                    Toast.makeText(context, "Error accepting order", Toast.LENGTH_SHORT).show();
                                }
                            });
                }
            }
        });

        // Reject Order button click listener
        holder.closeButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                if (request.getDocumentId() != null) {
                    FirebaseFirestore.getInstance().collection("CommissionRequests")
                            .document(request.getDocumentId())
                            .update("status", "Rejected")
                            .addOnSuccessListener(new OnSuccessListener<Void>() {
                                @Override
                                public void onSuccess(Void aVoid) {
                                    request.setStatus("Rejected");
                                    notifyItemChanged(position);
                                    Toast.makeText(context, "Order rejected", Toast.LENGTH_SHORT).show();
                                    if (statusChangedListener != null) {
                                        statusChangedListener.onStatusChanged();
                                    }
                                }
                            })
                            .addOnFailureListener(new OnFailureListener() {
                                @Override
                                public void onFailure(@NonNull Exception e) {
                                    Toast.makeText(context, "Error rejecting order", Toast.LENGTH_SHORT).show();
                                }
                            });
                }
            }
        });

        // Set an OnClickListener on the entire item view to show order details
        holder.itemView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                // Inflate the details popup layout
                LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
                View popupView = inflater.inflate(R.layout.popup_request_details, null);

                // Create a PopupWindow (focusable so clicking outside dismisses it)
                PopupWindow detailsPopup = new PopupWindow(
                        popupView,
                        ViewGroup.LayoutParams.WRAP_CONTENT,
                        ViewGroup.LayoutParams.WRAP_CONTENT,
                        true
                );
                detailsPopup.setOutsideTouchable(true);
                detailsPopup.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));
                detailsPopup.showAtLocation(v, Gravity.CENTER, 0, 0);

                // Populate the details popup with order information
                TextView tvName = popupView.findViewById(R.id.tvName);
                TextView tvType = popupView.findViewById(R.id.tvType);
                TextView tvSize = popupView.findViewById(R.id.tvSize);
                TextView tvStyle = popupView.findViewById(R.id.tvStyle);
                TextView tvBudget = popupView.findViewById(R.id.tvBudget);
                TextView tvAdditionalNote = popupView.findViewById(R.id.tvAdditionalNote);

                tvName.setText("Name: " + request.getName());
                tvType.setText("Type: " + request.getType());
                tvSize.setText("Size: " + (request.getSize() != null ? request.getSize() : "N/A"));
                tvStyle.setText("Style: " + (request.getStyle() != null ? request.getStyle() : "N/A"));
                tvBudget.setText("Budget: " + (request.getBudget() != null ? request.getBudget() : "N/A"));
                tvAdditionalNote.setText("Additional Note: " + (request.getAdditionalNote() != null ? request.getAdditionalNote() : "N/A"));
            }
        });
    }

    @Override
    public int getItemCount() {
        return requests.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        TextView orderTitle, orderDetails;
        ImageButton checkmarkButton, closeButton;

        public ViewHolder(View itemView) {
            super(itemView);
            orderTitle = itemView.findViewById(R.id.order_title);
            orderDetails = itemView.findViewById(R.id.order_details);
            checkmarkButton = itemView.findViewById(R.id.checkmark_button);
            closeButton = itemView.findViewById(R.id.close_button);
        }
    }
}

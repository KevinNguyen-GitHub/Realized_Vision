package com.example.realizedvision;

import android.content.Context;
import android.graphics.drawable.ColorDrawable;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.button.MaterialButton;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.List;

public class AcceptedRequestsAdapter extends RecyclerView.Adapter<AcceptedRequestsAdapter.ViewHolder> {

    private Context context;
    private List<CommissionRequest> requests;

    public AcceptedRequestsAdapter(Context context, List<CommissionRequest> requests) {
        this.context = context;
        this.requests = requests;
    }

    @NonNull
    @Override
    public AcceptedRequestsAdapter.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.item_status, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull AcceptedRequestsAdapter.ViewHolder holder, int position) {
        CommissionRequest request = requests.get(position);

        // Bind the name & status
        holder.nameTextView.setText(request.getName());
        holder.statusTextView.setText("Status: " + request.getStatus());

        // Show a popup for multiple status choices
        holder.updateButton.setOnClickListener(v -> showUpdatePopup(request, position, holder.updateButton));

        // Show a details popup on item click
        holder.itemView.setOnClickListener(v -> showDetailsPopup(request, v));
    }

    @Override
    public int getItemCount() {
        return requests.size();
    }

    /**
     * 1) The multi-option status popup: In Progress, Completed, Cancelled.
     */
    private void showUpdatePopup(CommissionRequest request, int position, View anchorView) {
        LayoutInflater inflater = LayoutInflater.from(context);
        View popupView = inflater.inflate(R.layout.popup_update_options, null);

        final PopupWindow updatePopup = new PopupWindow(
                popupView,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                true
        );
        updatePopup.setOutsideTouchable(true);
        updatePopup.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));

        // Appear near the "Update" button
        updatePopup.showAsDropDown(anchorView, 0, 0);

        MaterialButton btnInProgress = popupView.findViewById(R.id.btnInProgress);
        MaterialButton btnCompleted  = popupView.findViewById(R.id.btnCompleted);
        MaterialButton btnCancelled  = popupView.findViewById(R.id.btnCancelled);
        MaterialButton btnDelayed    = popupView.findViewById(R.id.btnDelayed);

        // Set "In Progress"
        btnInProgress.setOnClickListener(v -> {
            updateStatus(request, position, "In Progress");
            updatePopup.dismiss();
        });

        // Set "Completed"
        btnCompleted.setOnClickListener(v -> {
            updateStatus(request, position, "Completed");
            updatePopup.dismiss();
        });

        // Set "Cancelled"
        btnCancelled.setOnClickListener(v -> {
            updateStatus(request, position, "Cancelled");
            updatePopup.dismiss();
        });

        // NEW: Set "Delayed"
        btnDelayed.setOnClickListener(v -> {
            updateStatus(request, position, "Delayed");
            updatePopup.dismiss();
        });
    }

    /**
     * 2) The details popup for showing size, style, budget, etc.
     */
    private void showDetailsPopup(CommissionRequest request, View anchorView) {
        LayoutInflater inflater = LayoutInflater.from(context);
        View popupView = inflater.inflate(R.layout.popup_request_details, null);

        PopupWindow detailsPopup = new PopupWindow(
                popupView,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                true
        );
        detailsPopup.setOutsideTouchable(true);
        detailsPopup.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));
        detailsPopup.showAtLocation(anchorView, Gravity.CENTER, 0, 0);

        // Populate detail fields
        TextView tvName           = popupView.findViewById(R.id.tvName);
        TextView tvType           = popupView.findViewById(R.id.tvType);
        TextView tvSize           = popupView.findViewById(R.id.tvSize);
        TextView tvStyle          = popupView.findViewById(R.id.tvStyle);
        TextView tvBudget         = popupView.findViewById(R.id.tvBudget);
        TextView tvAdditionalNote = popupView.findViewById(R.id.tvAdditionalNote);

        tvName.setText("Name: " + request.getName());
        tvType.setText("Type: " + request.getType());
        tvSize.setText("Size: " + (request.getSize() != null ? request.getSize() : "N/A"));
        tvStyle.setText("Style: " + (request.getStyle() != null ? request.getStyle() : "N/A"));
        tvBudget.setText("Budget: " + (request.getBudget() != null ? request.getBudget() : "N/A"));
        tvAdditionalNote.setText("Additional Note: "
                + (request.getAdditionalNote() != null ? request.getAdditionalNote() : "N/A"));

        MaterialButton btnAccept = popupView.findViewById(R.id.btnAccept);
        MaterialButton btnReject = popupView.findViewById(R.id.btnReject);
        btnAccept.setVisibility(View.GONE);
        btnReject.setVisibility(View.GONE);
    }

    /**
     * 3) Updating Firestore's status
     */
    private void updateStatus(CommissionRequest request, int position, String newStatus) {
        if (request.getDocumentId() == null) return;

        FirebaseFirestore.getInstance()
                .collection("CommissionRequests")
                .document(request.getDocumentId())
                .update("status", newStatus)
                .addOnSuccessListener(aVoid -> {
                    if ("Rejected".equals(newStatus)) {
                        // Remove from the list
                        requests.remove(position);
                        notifyItemRemoved(position);
                        Toast.makeText(context,
                                "Order rejected & removed from the list",
                                Toast.LENGTH_SHORT).show();
                    } else {
                        // Just update the item in place
                        request.setStatus(newStatus);
                        notifyItemChanged(position);
                        Toast.makeText(context,
                                "Status updated to " + newStatus,
                                Toast.LENGTH_SHORT).show();
                    }
                })
                .addOnFailureListener(e ->
                        Toast.makeText(context, "Failed to update status", Toast.LENGTH_SHORT).show()
                );
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        TextView nameTextView, statusTextView;
        MaterialButton updateButton;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            nameTextView   = itemView.findViewById(R.id.name);
            statusTextView = itemView.findViewById(R.id.status);
            updateButton   = itemView.findViewById(R.id.update_order_button);
        }
    }
}

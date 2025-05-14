package com.example.realizedvision;

import android.content.Context;
import android.graphics.drawable.ColorDrawable;
import android.view.*;
import android.widget.*;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import com.google.firebase.firestore.FirebaseFirestore;
import java.util.*;

public class CommissionVendorAdapter extends RecyclerView.Adapter<CommissionVendorAdapter.ViewHolder> {

    private static final String PRESET_USER_ID = "user_123";
    private static final String PRESET_VENDOR_ID = "vendor_456";

    private Context context;
    private List<CommissionRequest> requests;
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
        View view = LayoutInflater.from(context).inflate(R.layout.item_request, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        CommissionRequest request = requests.get(position);
        holder.orderTitle.setText(request.getName());
        holder.orderDetails.setText(request.getType() + " | " + request.getStatus());

        holder.checkmarkButton.setOnClickListener(v -> {
            if (request.getDocumentId() != null) {
                FirebaseFirestore firestore = FirebaseFirestore.getInstance();
                firestore.collection("CommissionRequests")
                        .document(request.getDocumentId())
                        .update("status", "Accepted")
                        .addOnSuccessListener(aVoid -> {
                            request.setStatus("Accepted");
                            notifyItemChanged(position);
                            Toast.makeText(context, "Order accepted", Toast.LENGTH_SHORT).show();
                            if (statusChangedListener != null) statusChangedListener.onStatusChanged();
                            createChatRoom(request);
                        })
                        .addOnFailureListener(e ->
                                Toast.makeText(context, "Error accepting order", Toast.LENGTH_SHORT).show()
                        );
            }
        });

        holder.closeButton.setOnClickListener(v -> {
            if (request.getDocumentId() != null) {
                FirebaseFirestore.getInstance().collection("CommissionRequests")
                        .document(request.getDocumentId())
                        .update("status", "Rejected")
                        .addOnSuccessListener(aVoid -> {
                            request.setStatus("Rejected");
                            notifyItemChanged(position);
                            Toast.makeText(context, "Order rejected", Toast.LENGTH_SHORT).show();
                            if (statusChangedListener != null) statusChangedListener.onStatusChanged();
                        })
                        .addOnFailureListener(e ->
                                Toast.makeText(context, "Error rejecting order", Toast.LENGTH_SHORT).show()
                        );
            }
        });

        holder.itemView.setOnClickListener(v -> showDetailsPopup(v, request));
    }

    @Override
    public int getItemCount() {
        return requests.size();
    }

    private void createChatRoom(CommissionRequest request) {
        FirebaseFirestore firestore = FirebaseFirestore.getInstance();

        HashMap<String, Object> chatRoomData = new HashMap<>();
        chatRoomData.put("participants", Arrays.asList(PRESET_VENDOR_ID, PRESET_USER_ID));
        chatRoomData.put("commissionRequestId", request.getDocumentId());
        chatRoomData.put("createdAt", System.currentTimeMillis());

        firestore.collection("ChatRooms")
                .add(chatRoomData)
                .addOnSuccessListener(documentReference ->
                        Toast.makeText(context, "Chat room created", Toast.LENGTH_SHORT).show()
                )
                .addOnFailureListener(e ->
                        Toast.makeText(context, "Failed to create chat room", Toast.LENGTH_SHORT).show()
                );
    }

    private void showDetailsPopup(View anchorView, CommissionRequest request) {
        LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View popupView = inflater.inflate(R.layout.popup_request_details, null);

        PopupWindow detailsPopup = new PopupWindow(popupView, ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT, true);
        detailsPopup.setOutsideTouchable(true);
        detailsPopup.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));
        detailsPopup.showAtLocation(anchorView, Gravity.CENTER, 0, 0);

        ((TextView) popupView.findViewById(R.id.tvName)).setText("Name: " + request.getName());
        ((TextView) popupView.findViewById(R.id.tvType)).setText("Type: " + request.getType());
        ((TextView) popupView.findViewById(R.id.tvSize)).setText("Size: " + (request.getSize() != null ? request.getSize() : "N/A"));
        ((TextView) popupView.findViewById(R.id.tvStyle)).setText("Style: " + (request.getStyle() != null ? request.getStyle() : "N/A"));
        ((TextView) popupView.findViewById(R.id.tvBudget)).setText("Budget: " + (request.getBudget() != null ? request.getBudget() : "N/A"));
        ((TextView) popupView.findViewById(R.id.tvAdditionalNote)).setText("Additional Note: " + (request.getAdditionalNote() != null ? request.getAdditionalNote() : "N/A"));
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

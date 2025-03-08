package com.example.realizedvision;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.android.material.button.MaterialButton;
import java.util.List;
import android.widget.TextView;

public class CommissionUserAdapter extends RecyclerView.Adapter<CommissionUserAdapter.ViewHolder> {

    private Context context;
    private List<CommissionRequest> requests;
    private OnOrderCanceledListener onOrderCanceledListener;

    // Interface for cancellation callback
    public interface OnOrderCanceledListener {
        void onOrderCanceled();
    }

    public void setOnOrderCanceledListener(OnOrderCanceledListener listener) {
        this.onOrderCanceledListener = listener;
    }

    public CommissionUserAdapter(Context context, List<CommissionRequest> requests) {
        this.context = context;
        this.requests = requests;
    }

    @NonNull
    @Override
    public CommissionUserAdapter.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        // Inflate the user item layout
        View view = LayoutInflater.from(context).inflate(R.layout.item_commissions, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull CommissionUserAdapter.ViewHolder holder, int position) {
        CommissionRequest request = requests.get(position);
        holder.vendorName.setText(request.getName());
        holder.status.setText("Status: " + request.getStatus());

        // Set up the Cancel Order button listener
        holder.cancelOrderButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                int pos = holder.getAdapterPosition();
                if (pos != RecyclerView.NO_POSITION) {
                    CommissionRequest req = requests.get(pos);
                    if (req.getDocumentId() != null) {
                        FirebaseFirestore.getInstance().collection("CommissionRequests")
                                .document(req.getDocumentId())
                                .update("status", "Cancelled")
                                .addOnSuccessListener(new OnSuccessListener<Void>() {
                                    @Override
                                    public void onSuccess(Void aVoid) {
                                        req.setStatus("Cancelled");
                                        notifyItemChanged(pos);
                                        Toast.makeText(context, "Order cancelled", Toast.LENGTH_SHORT).show();
                                        // Notify the listener so that other views can be updated as well
                                        if (onOrderCanceledListener != null) {
                                            onOrderCanceledListener.onOrderCanceled();
                                        }
                                    }
                                })
                                .addOnFailureListener(new OnFailureListener() {
                                    @Override
                                    public void onFailure(@NonNull Exception e) {
                                        Toast.makeText(context, "Error cancelling order", Toast.LENGTH_SHORT).show();
                                    }
                                });
                    }
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return requests.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        TextView vendorName, status;
        MaterialButton cancelOrderButton;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            vendorName = itemView.findViewById(R.id.vendorName);
            status = itemView.findViewById(R.id.status);
            cancelOrderButton = itemView.findViewById(R.id.cancel_order_button);
        }
    }
}

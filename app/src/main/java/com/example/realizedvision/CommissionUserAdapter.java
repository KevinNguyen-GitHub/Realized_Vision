package com.example.realizedvision;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.firestore.FirebaseFirestore;
import com.google.android.material.button.MaterialButton;

import java.util.List;

/** Shows the customer-side view of a commission request list. */
public class CommissionUserAdapter
        extends RecyclerView.Adapter<CommissionUserAdapter.VH> {

    /* ───────────────────────── constants & fields ─────────────────────── */
    private final LayoutInflater      inflater;
    private final List<CommissionRequest> data;
    private final FirebaseFirestore   db   = FirebaseFirestore.getInstance();

    private OnOrderCanceledListener canceledListener;

    /* ───────────────────────── public callback API ─────────────────────── */
    public interface OnOrderCanceledListener { void onOrderCanceled(); }
    public void setOnOrderCanceledListener(OnOrderCanceledListener l) {
        this.canceledListener = l;
    }

    /* ───────────────────────── constructor ─────────────────────────────── */
    public CommissionUserAdapter(Context ctx, List<CommissionRequest> requests) {
        this.inflater = LayoutInflater.from(ctx);
        this.data     = requests;
        setHasStableIds(true);                     // safer RecyclerView animations
    }

    /* ───────────────────────── ViewHolder ──────────────────────────────── */
    final class VH extends RecyclerView.ViewHolder {
        final TextView vendor, status;
        final MaterialButton cancelBtn;

        VH(View v) {
            super(v);
            vendor    = v.findViewById(R.id.vendorName);
            status    = v.findViewById(R.id.status);
            cancelBtn = v.findViewById(R.id.cancel_order_button);
        }

        /** Binds a single item and its click logic. */
        void bind(CommissionRequest req,
                  FirebaseFirestore db,
                  List<CommissionRequest> list,
                  OnOrderCanceledListener cb) {

            vendor.setText(req.getName());
            status.setText("Status: " + req.getStatus());

            cancelBtn.setOnClickListener(view -> {
                int pos = getBindingAdapterPosition();
                if (pos == RecyclerView.NO_POSITION) return;

                CommissionRequest item = list.get(pos);
                if (item.getDocumentId() == null) return;

                cancelBtn.setEnabled(false);          // debounce tap
                db.collection("CommissionRequests")
                        .document(item.getDocumentId())
                        .update("status", "Cancelled")
                        .addOnSuccessListener(v -> {
                            item.setStatus("Cancelled");
                            list.set(pos, item);            // keep local list in sync
                            notifyItemChanged(pos);
                            Toast.makeText(view.getContext(),
                                    "Order cancelled", Toast.LENGTH_SHORT).show();
                            cancelBtn.setEnabled(true);
                            if (cb != null) cb.onOrderCanceled();
                        })
                        .addOnFailureListener(e -> {
                            Toast.makeText(view.getContext(),
                                    "Error cancelling order", Toast.LENGTH_SHORT).show();
                            cancelBtn.setEnabled(true);
                        });
            });
        }
    }

    /* ───────────────────── RecyclerView overrides ─────────────────────── */
    @NonNull @Override
    public VH onCreateViewHolder(@NonNull ViewGroup p, int vType) {
        return new VH(inflater.inflate(R.layout.item_commissions, p, false));
    }

    @Override
    public void onBindViewHolder(@NonNull VH h, int pos) {
        h.bind(data.get(pos), db, data, canceledListener);
    }

    @Override public int getItemCount()               { return data.size(); }
    @Override public long getItemId(int position)     { return data.get(position).hashCode(); }
}

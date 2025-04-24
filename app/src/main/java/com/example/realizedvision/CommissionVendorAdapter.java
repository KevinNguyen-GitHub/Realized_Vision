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

import com.google.firebase.firestore.FirebaseFirestore;

import java.util.List;

public class CommissionVendorAdapter
        extends RecyclerView.Adapter<CommissionVendorAdapter.VH> {

    /* ───────────────── constants & fields ───────────────── */
    private final LayoutInflater inflater;
    private final List<CommissionRequest> data;
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();

    private OnStatusChangedListener statusCb;

    public interface OnStatusChangedListener { void onStatusChanged(); }
    public void setOnStatusChangedListener(OnStatusChangedListener l) { statusCb = l; }

    public CommissionVendorAdapter(Context ctx, List<CommissionRequest> requests) {
        this.inflater = LayoutInflater.from(ctx);
        this.data     = requests;
        setHasStableIds(true);                 // smoother list changes
    }

    /* ───────────────── ViewHolder ───────────────── */
    static final class VH extends RecyclerView.ViewHolder {
        final TextView title, details;
        final ImageButton acceptBtn, rejectBtn;
        VH(View v) {
            super(v);
            title     = v.findViewById(R.id.order_title);
            details   = v.findViewById(R.id.order_details);
            acceptBtn = v.findViewById(R.id.checkmark_button);
            rejectBtn = v.findViewById(R.id.close_button);
        }
    }

    /* ─────────────── RV overrides ─────────────── */
    @NonNull
    @Override public VH onCreateViewHolder(@NonNull ViewGroup p, int t) {
        return new VH(inflater.inflate(R.layout.item_request, p, false));
    }

    @Override public void onBindViewHolder(@NonNull VH h, int pos) {
        CommissionRequest r = data.get(pos);
        h.title.setText(r.getName());
        h.details.setText(r.getType() + " | " + r.getStatus());

        View.OnClickListener statusClick = v -> {
            int p = h.getBindingAdapterPosition();
            if (p == RecyclerView.NO_POSITION) return;

            String newStatus = (v == h.acceptBtn) ? "Accepted" : "Rejected";
            updateStatus(v, data.get(p), p, newStatus);
        };

        h.acceptBtn.setOnClickListener(statusClick);
        h.rejectBtn.setOnClickListener(statusClick);
        h.itemView.setOnClickListener(v -> showDetailsPopup(v, r));
    }

    @Override public int  getItemCount()          { return data.size(); }
    @Override public long getItemId(int position) { return data.get(position).hashCode(); }

    /* ─────────────── status update ─────────────── */
    private void updateStatus(View trigger, CommissionRequest req,
                              int pos, String status) {
        if (req.getDocumentId() == null) return;

        trigger.setEnabled(false);               // debounce
        db.collection("CommissionRequests")
                .document(req.getDocumentId())
                .update("status", status)
                .addOnSuccessListener(a -> {
                    req.setStatus(status);
                    notifyItemChanged(pos);
                    Toast.makeText(trigger.getContext(),
                            "Order " + status.toLowerCase(), Toast.LENGTH_SHORT).show();
                    if (statusCb != null) statusCb.onStatusChanged();
                    trigger.setEnabled(true);
                })
                .addOnFailureListener(e -> {
                    Toast.makeText(trigger.getContext(),
                            "Error updating order", Toast.LENGTH_SHORT).show();
                    trigger.setEnabled(true);
                });
    }

    /* ─────────────── popup helper ─────────────── */
    private void showDetailsPopup(View anchor, CommissionRequest r) {
        View pop = inflater.inflate(R.layout.popup_request_details, null);
        PopupWindow win = new PopupWindow(pop,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                true);
        win.setOutsideTouchable(true);
        win.setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));
        win.showAtLocation(anchor, Gravity.CENTER, 0, 0);

        ((TextView) pop.findViewById(R.id.tvName)) .setText("Name: "  + r.getName());
        ((TextView) pop.findViewById(R.id.tvType)) .setText("Type: "  + r.getType());
        ((TextView) pop.findViewById(R.id.tvSize)) .setText("Size: "  + na(r.getSize()));
        ((TextView) pop.findViewById(R.id.tvStyle)).setText("Style: " + na(r.getStyle()));
        ((TextView) pop.findViewById(R.id.tvBudget))
                .setText("Budget: " + na(r.getBudget()));
        ((TextView) pop.findViewById(R.id.tvAdditionalNote))
                .setText("Additional Note: " + na(r.getAdditionalNote()));
    }

    private static String na(String s) { return (s == null || s.isEmpty()) ? "N/A" : s; }
}

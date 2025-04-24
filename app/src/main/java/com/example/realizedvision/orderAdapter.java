package com.example.realizedvision;

import android.app.AlertDialog;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

/** Simple adapter that lists orders and lets the user cancel one. */
public class orderAdapter extends RecyclerView.Adapter<orderAdapter.VH> {

    private final Context        ctx;
    private final List<String>   orders;
    private final LayoutInflater inflater;

    public OrderAdapter(Context c, List<String> list) {
        this.ctx       = c;
        this.orders    = list;
        this.inflater  = LayoutInflater.from(c);
        setHasStableIds(true);          // smoother RecyclerView animations
    }

    /* ───────────────────────── ViewHolder ───────────────────────── */
    static final class VH extends RecyclerView.ViewHolder {
        final ImageView img;
        final TextView  title, details;
        final Button    cancel;

        VH(View v) {
            super(v);
            img     = v.findViewById(R.id.item_image);
            title   = v.findViewById(R.id.order_title);
            details = v.findViewById(R.id.order_details);
            cancel  = v.findViewById(R.id.cancel_order_button);
        }
    }

    /* ───────────────────── RecyclerView hooks ──────────────────── */
    @NonNull @Override
    public VH onCreateViewHolder(@NonNull ViewGroup p, int t) {
        return new VH(inflater.inflate(R.layout.item_order, p, false));
    }

    @Override
    public void onBindViewHolder(@NonNull VH h, int pos) {
        String order = orders.get(pos);
        h.title.setText(order);
        h.details.setText("Additional details for " + order);

        h.cancel.setOnClickListener(v -> showConfirmDialog(h.getBindingAdapterPosition()));
    }

    @Override public int  getItemCount()       { return orders.size(); }
    @Override public long getItemId(int pos)   { return orders.get(pos).hashCode(); }

    /* ───────────────────────── dialog helper ───────────────────── */
    private void showConfirmDialog(int position) {
        View dView = inflater.inflate(R.layout.popup_confirmation, null);
        AlertDialog d = new AlertDialog.Builder(ctx).setView(dView).create();

        dView.findViewById(R.id.btn_yes).setOnClickListener(v -> {
            orders.remove(position);
            notifyItemRemoved(position);
            Toast.makeText(ctx, "Order cancelled", Toast.LENGTH_SHORT).show();
            d.dismiss();
        });

        dView.findViewById(R.id.btn_no).setOnClickListener(v -> d.dismiss());
        d.show();
    }
}

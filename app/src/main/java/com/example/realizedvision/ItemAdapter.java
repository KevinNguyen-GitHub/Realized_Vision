package com.example.realizedvision;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.Filterable;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;

import java.util.ArrayList;
import java.util.List;

/** Grid / list adapter for displaying shop items with optional filtering. */
public class ItemAdapter
        extends RecyclerView.Adapter<ItemAdapter.VH>
        implements Filterable {

    /* ─────────────────────────── constants ─────────────────────────── */
    private static final String TAG = "ItemAdapter";

    /* ─────────────────────── immutable fields ──────────────────────── */
    private final LayoutInflater inflater;
    private final boolean        filterable;

    /* ───────────────────────── mutable data ─────────────────────────── */
    private final List<Item>      master;   // full list (never mutated)
    private final List<Item>      visible;  // what RecyclerView shows
    private OnItemClickListener   listener;

    /* ───────────────────────── public API ───────────────────────────── */
    public interface OnItemClickListener {
        void onFavoriteClick(int position);
        void onCartClick(int position);
        void onItemClick(int position);
    }
    public void setOnItemClickListener(OnItemClickListener l) { listener = l; }

    /* ───────────────────────── constructor ─────────────────────────── */
    public ItemAdapter(Context ctx, List<Item> items, boolean isFilterable) {
        this.inflater   = LayoutInflater.from(ctx);
        this.master     = new ArrayList<>(items); // defensive copy
        this.visible    = new ArrayList<>(items);
        this.filterable = isFilterable;
        setHasStableIds(true);                    // better animations
    }

    /* ───────────────────── RecyclerView plumbing ───────────────────── */
    @NonNull
    @Override public VH onCreateViewHolder(@NonNull ViewGroup p, int t) {
        return new VH(inflater.inflate(R.layout.item, p, false));
    }

    @Override public void onBindViewHolder(@NonNull VH h, int pos) {
        Item it = visible.get(pos);

        h.name.setText(it.getName());
        h.price.setText(String.format("$%.2f", it.getPrice()));
        h.desc.setText(it.getDescription());

        Glide.with(h.image.getContext())
                .load(it.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .into(h.image);

        h.fav.setImageResource(it.isFavorite() ?
                R.drawable.ic_red_heart : R.drawable.ic_favorite);

        h.bindClicks(listener);
    }

    @Override public int  getItemCount()          { return visible.size(); }
    @Override public long getItemId(int position) {
        String id = visible.get(position).getItemID();
        return id != null ? id.hashCode() : position;
    }

    /* ───────────────────────── filtering ───────────────────────────── */
    @Override public Filter getFilter() { return filterable ? filter : null; }

    private final Filter filter = new Filter() {
        @Override protected FilterResults performFiltering(CharSequence q) {
            List<Item> out = new ArrayList<>();
            if (q == null || q.length() == 0) {
                out.addAll(master);
            } else {
                String pat = q.toString().trim().toLowerCase();
                for (Item i : master) {
                    String cat = i.getCategory();
                    if (cat != null && cat.toLowerCase().contains(pat)) out.add(i);
                }
            }
            FilterResults r = new FilterResults();
            r.values = out;
            r.count  = out.size();
            return r;
        }
        @SuppressWarnings("unchecked")
        @Override protected void publishResults(CharSequence q, FilterResults r) {
            if (r.values == null) { Log.d(TAG, "Filter results null"); return; }

            visible.clear();
            visible.addAll((List<Item>) r.values);
            notifyDataSetChanged();
        }
    };

    /* ───────────────────────── ViewHolder ──────────────────────────── */
    static final class VH extends RecyclerView.ViewHolder {
        final TextView  name, price, desc;
        final ImageView image, fav, cart;

        VH(View v) {
            super(v);
            name  = v.findViewById(R.id.item_name);
            price = v.findViewById(R.id.item_price);
            desc  = v.findViewById(R.id.item_description);
            image = v.findViewById(R.id.item_image);
            fav   = v.findViewById(R.id.favorite_icon);
            cart  = v.findViewById(R.id.cart_icon);
        }

        void bindClicks(OnItemClickListener l) {
            if (l == null) return;

            fav .setOnClickListener(view -> {
                int p = getBindingAdapterPosition();
                if (p != RecyclerView.NO_POSITION) {
                    animate(view);
                    l.onFavoriteClick(p);
                }
            });
            cart.setOnClickListener(view -> {
                int p = getBindingAdapterPosition();
                if (p != RecyclerView.NO_POSITION) {
                    animate(view);
                    l.onCartClick(p);
                }
            });
            itemView.setOnClickListener(v -> {
                int p = getBindingAdapterPosition();
                if (p != RecyclerView.NO_POSITION) l.onItemClick(p);
            });
        }

        private void animate(View v) {
            v.animate().scaleX(1.2f).scaleY(1.2f).setDuration(200)
                    .withEndAction(() -> v.animate().scaleX(1f).scaleY(1f).setDuration(200).start())
                    .start();
        }
    }
}

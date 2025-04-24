package com.example.realizedvision;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;

import java.util.List;

public class ShoppingCartAdapter
        extends RecyclerView.Adapter<ShoppingCartAdapter.ViewHolder> {

    public interface OnItemClickListener {
        void onFavoriteClick(int pos);
        void onRemoveClick(int pos);
        void onIncrementClick(int pos);
        void onItemClick(int pos);
    }

    private final List<Item> data;
    private final LayoutInflater inflater;
    private OnItemClickListener cb;

    public ShoppingCartAdapter(@NonNull android.content.Context ctx,
                               @NonNull List<Item> items) {
        this.data      = items;
        this.inflater  = LayoutInflater.from(ctx);
        setHasStableIds(true);
    }

    public void setOnItemClickListener(OnItemClickListener l) { this.cb = l; }

    /* ─────────────────── RecyclerView hooks ──────────────────── */
    @NonNull @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup p,int t){
        return new ViewHolder(inflater.inflate(R.layout.cart_item, p, false));
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder h,int pos){
        Item it = data.get(pos);

        h.name .setText(it.getName());
        h.price.setText(String.format("$%.2f", it.getPrice()));
        h.qty  .setText("Qty: " + it.getQuantity());
        h.fav  .setImageResource(it.isFavorite()
                ? R.drawable.ic_red_heart
                : R.drawable.ic_favorite);

        Glide.with(h.img).load(it.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .into(h.img);
    }

    @Override public int  getItemCount()          { return data.size(); }
    @Override public long getItemId(int pos)      { return data.get(pos).getItemID().hashCode(); }

    /* ────────────────────── ViewHolder ────────────────────────── */
    final class ViewHolder extends RecyclerView.ViewHolder {
        final TextView name, price, qty;
        final ImageView img, fav, remove, inc;

        ViewHolder(View v) {
            super(v);
            img    = v.findViewById(R.id.item_image);
            name   = v.findViewById(R.id.item_name);
            price  = v.findViewById(R.id.item_price);
            qty    = v.findViewById(R.id.item_quantity);
            fav    = v.findViewById(R.id.favorite_icon);
            remove = v.findViewById(R.id.remove_icon);
            inc    = v.findViewById(R.id.increment_icon);

            fav   .setOnClickListener(c -> clickAnim(fav,   () -> cb.onFavoriteClick(getPos())));
            inc   .setOnClickListener(c -> clickAnim(inc,   () -> cb.onIncrementClick(getPos())));
            remove.setOnClickListener(c -> clickAnim(remove,() -> cb.onRemoveClick(getPos())));
            v.setOnClickListener(c -> { if (cb!=null && getPos()!=-1) cb.onItemClick(getPos()); });
        }

        private int getPos(){ return getBindingAdapterPosition(); }

        /** Re-usable zoom animation wrapper. */
        private void clickAnim(ImageView iv, Runnable after){
            if (cb==null || getPos()==RecyclerView.NO_POSITION) return;
            iv.animate().scaleX(1.2f).scaleY(1.2f).setDuration(120)
                    .withEndAction(() -> iv.animate().scaleX(1f).scaleY(1f)
                            .setDuration(120).withEndAction(after).start())
                    .start();
        }
    }
}

package com.example.realizedvision;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.bumptech.glide.Glide;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.FirebaseFirestore;

import java.util.HashMap;
import java.util.List;

/** Lists the items in the user’s past orders and lets them “Buy again” or leave / edit a review. */
public class OrderHistoryAdapter extends RecyclerView.Adapter<OrderHistoryAdapter.VH> {

    private final Context      ctx;
    private final List<Item>   data;
    private final LayoutInflater inflater = LayoutInflater.from(ctx = null);

    public OrderHistoryAdapter(Context c, List<Item> items) {
        this.ctx  = c;
        this.data = items;
        setHasStableIds(true);                // smoother diff-animations
    }

    /* ───────────────────────── ViewHolder ───────────────────────── */
    static final class VH extends RecyclerView.ViewHolder {
        final ImageView img;
        final TextView  name, price, qty, buy, review;
        VH(View v) {
            super(v);
            img    = v.findViewById(R.id.item_image);
            name   = v.findViewById(R.id.item_name);
            price  = v.findViewById(R.id.item_price);
            qty    = v.findViewById(R.id.item_quantity);
            buy    = v.findViewById(R.id.buy_again);
            review = v.findViewById(R.id.leave_review);
        }
    }

    /* ───────────────────── RecyclerView hooks ──────────────────── */
    @NonNull @Override public VH onCreateViewHolder(@NonNull ViewGroup p,int t){
        return new VH(LayoutInflater.from(ctx).inflate(R.layout.history_item,p,false));
    }

    @Override public void onBindViewHolder(@NonNull VH h,int pos){
        Item it=data.get(pos);

        h.name .setText(it.getName());
        h.price.setText(String.format("$%.2f",it.getPrice()));
        h.qty  .setText("Qty: "+it.getQuantity());

        Glide.with(ctx).load(it.getImageUrl())
                .placeholder(R.drawable.ic_placeholder_image)
                .error(R.drawable.ic_placeholder_image)
                .into(h.img);

        h.buy.setOnClickListener(v-> addToCart(it));
        h.review.setOnClickListener(v-> new ReviewDialog(ctx,it).show());
    }

    @Override public int  getItemCount(){return data.size();}
    @Override public long getItemId(int p){return data.get(p).getItemID().hashCode();}

    /* ───────────────────────── cart helper ─────────────────────── */
    private void addToCart(Item it){
        String uid = FirebaseAuth.getInstance().getUid();
        if(uid==null){toast("Login first");return;}

        FirebaseFirestore.getInstance()
                .collection("Users").document(uid)
                .collection("Shopping Cart").document(it.getItemID())
                .set(new HashMap<String,Object>(){{
                    put("description",it.getDescription());
                    put("favorite",it.isFavorite());
                    put("imageUrl",it.getImageUrl());
                    put("itemID",it.getItemID());
                    put("name",it.getName());
                    put("price",it.getPrice());
                    put("quantity",it.getQuantity());
                    put("vendorID",it.getVendorID());
                }})
                .addOnSuccessListener(a-> toast("Added to cart"))
                .addOnFailureListener(e-> toast("Failed to add: "+e.getMessage()));
    }

    private void toast(String m){ Toast.makeText(ctx,m,Toast.LENGTH_SHORT).show(); }
}

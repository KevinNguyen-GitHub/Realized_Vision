package com.example.realizedvision;

import android.content.ClipData;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

public class ItemAdapter extends RecyclerView.Adapter<ItemViewHolder> {
    private List<ClipData.Item> itemList;
    private Context context;

    public ItemAdapter(Context context, List<ClipData.Item> itemList) {
        this.context = context;
        this.itemList = itemList;
    }

    @Override
    public ItemViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.item, parent, false);
        return new ItemViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(ItemViewHolder holder, int position) {
        ClipData.Item item = itemList.get(position);
        holder.itemName.setText(item.getName());
        holder.itemPrice.setText("$" + String.valueOf(item.getPrice()));
        // Look into glide or picasso library
        // using placeholder image:
        holder.itemImage.setImageResource(R.drawable.ic_launcher_background);
    }

    @Override
    public int getItemCount() {
        return itemList.size();
    }
}
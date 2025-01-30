package com.example.realizedvision;

import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;

public class ItemViewHolder extends RecyclerView.ViewHolder {
    public ImageView itemImage;
    public TextView itemName;
    public TextView itemPrice;

    public ItemViewHolder(View itemView) {
        super(itemView);
        itemImage = itemView.findViewById(R.id.item_image);
        itemName = itemView.findViewById(R.id.item_name);
        itemPrice = itemView.findViewById(R.id.item_price);
    }
}
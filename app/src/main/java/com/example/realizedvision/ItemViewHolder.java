
package com.example.realizedvision;

import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.recyclerview.widget.RecyclerView;

public class ItemViewHolder extends RecyclerView.ViewHolder {
    public TextView itemName, itemPrice, itemDescription;
    public ImageView itemImage;

    public ItemViewHolder(View itemView) {
        super(itemView);
        itemName = itemView.findViewById(R.id.item_name);
        itemPrice = itemView.findViewById(R.id.item_price);
        itemDescription = itemView.findViewById(R.id.item_description);
        itemImage = itemView.findViewById(R.id.item_image);
    }
}

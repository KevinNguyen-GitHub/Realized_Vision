
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
public class ItemAdapter extends RecyclerView.Adapter<ItemAdapter.ItemViewHolder> implements Filterable{
    private List<Item> itemList;
    private List<Item> itemListFilter;
    private Context context;
    private OnItemClickListener listener;
    private final boolean isFilterable;

    public interface OnItemClickListener{
        void onFavoriteClick(int position);
        void onCartClick(int position);

        void onItemClick(int position);
    }

    public ItemAdapter(Context context, List<Item> itemList, boolean isFilterable) {
        this.context = context;
        this.itemList = itemList;
        this.itemListFilter = itemList;
        this.isFilterable = isFilterable;
        setHasStableIds(true); // Optimization for RecyclerView
    }

    public void setOnItemClickListener(OnItemClickListener listener){
        this.listener = listener;
    }

    @NonNull
    @Override
    public ItemViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.item, parent, false);
        return new ItemViewHolder(itemView, listener);
    }


    @Override
    public void onBindViewHolder(@NonNull ItemViewHolder holder, int position) {
        Item item = itemList.get(position);
        holder.itemName.setText(item.getName());
        holder.itemPrice.setText(String.format("$%.2f", item.getPrice()));
        holder.itemDescription.setText(item.getDescription());

        Glide.with(context)
                .load(item.getImageUrl()) // Ensure Item class has getImageUrl()
                .placeholder(R.drawable.ic_placeholder_image)
                .into(holder.itemImage);

        if (item.isFavorite()){
            holder.favoriteIcon.setImageResource(R.drawable.ic_red_heart);
        }
        else{
            holder.favoriteIcon.setImageResource(R.drawable.ic_favorite);
        }
    }

    @Override
    public int getItemCount() {
        return itemList.size();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public Filter getFilter(){
        if(!isFilterable){
            return null;
        }
        return itemFilter;
    }

    private Filter itemFilter = new Filter() {
        @Override
        protected FilterResults performFiltering(CharSequence constraint) {
            FilterResults results = new FilterResults();
            List<Item> filteredList = new ArrayList<>();
            if(constraint == null || constraint.length() == 0){
                filteredList.addAll(itemListFilter);
            }else{
                String filterPattern = constraint.toString().trim().toLowerCase();
                for(Item item : itemListFilter){
                    if(item.getCategory() != null) {
                        String itemCategory = item.getCategory().toLowerCase();
                        if (itemCategory.contains(filterPattern)) {
                            filteredList.add(item);
                        }
                    }
                }
            }
            results.values = filteredList;
            results.count = filteredList.size();
            return results;
        }

        @Override
        protected void publishResults(CharSequence constraint, FilterResults results) {
            if(results.values != null && results != null) {
                itemList.clear();
                itemList.addAll((List<Item>) results.values);
                notifyDataSetChanged();
            }
            else{
                Log.d("Filter Debug", "Filter results are null");
            }
        }
    };
    public static class ItemViewHolder extends RecyclerView.ViewHolder {
        public TextView itemName, itemPrice, itemDescription;
        public ImageView itemImage, favoriteIcon, cartIcon;

        public ItemViewHolder(View itemView, OnItemClickListener listener) {
            super(itemView);
            itemName = itemView.findViewById(R.id.item_name);
            itemPrice = itemView.findViewById(R.id.item_price);
            itemDescription = itemView.findViewById(R.id.item_description);
            itemImage = itemView.findViewById(R.id.item_image);
            favoriteIcon = itemView.findViewById(R.id.favorite_icon);
            cartIcon = itemView.findViewById(R.id.cart_icon);

            favoriteIcon.setOnClickListener(z -> {
                if(listener != null){
                    int position = getAbsoluteAdapterPosition();
                    if(position != RecyclerView.NO_POSITION){
                        favoriteIcon.animate()
                                .scaleX(1.2f)
                                .scaleY(1.2f)
                                .setDuration(200)
                                .withEndAction(() -> favoriteIcon.animate()
                                        .scaleX(1f)
                                        .scaleY(1f)
                                        .setDuration(200)
                                        .start())
                                .start();
                        listener.onFavoriteClick(position);
                    }
                }
            });
            cartIcon.setOnClickListener(z -> {
                if(listener!= null){
                    int position = getAbsoluteAdapterPosition();
                    if(position != RecyclerView.NO_POSITION){
                        cartIcon.animate()
                                .scaleX(1.2f)
                                .scaleY(1.2f)
                                .setDuration(200)
                                .withEndAction(() -> cartIcon.animate()
                                        .scaleX(1f)
                                        .scaleY(1f)
                                        .setDuration(200)
                                        .start())
                                .start();
                        listener.onCartClick(position);
                    }
                }
            });
            itemView.setOnClickListener(v -> {
                if (listener != null) {
                    int position = getAbsoluteAdapterPosition();
                    if (position != RecyclerView.NO_POSITION) {
                        listener.onItemClick(position);
                    }
                }
            });
        }
    }

}





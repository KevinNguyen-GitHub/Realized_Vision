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
import androidx.recyclerview.widget.RecyclerView;
import java.util.List;

public class orderAdapter extends RecyclerView.Adapter<orderAdapter.OrderViewHolder> {

    private List<String> orderList;
    private Context context;

    public orderAdapter(Context context, List<String> orderList) {
        this.context = context;
        this.orderList = orderList;
    }

    public static class OrderViewHolder extends RecyclerView.ViewHolder {
        ImageView itemImage;
        TextView orderTitle;
        TextView orderDetails;
        Button cancelOrderButton;

        public OrderViewHolder(View itemView) {
            super(itemView);
            itemImage = itemView.findViewById(R.id.item_image);
            orderTitle = itemView.findViewById(R.id.order_title);
            orderDetails = itemView.findViewById(R.id.order_details);
            cancelOrderButton = itemView.findViewById(R.id.cancel_order_button);
        }
    }

    @Override
    public OrderViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.item_order, parent, false);
        return new OrderViewHolder(view);
    }

    @Override
    public void onBindViewHolder(OrderViewHolder holder, int position) {
        String order = orderList.get(position);

        holder.orderTitle.setText(order);
        holder.orderDetails.setText("Additional details for " + order);


        holder.cancelOrderButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                // Inflate the custom confirmation dialog layout
                LayoutInflater inflater = LayoutInflater.from(context);
                View dialogView = inflater.inflate(R.layout.popup_confirmation, null);

                AlertDialog dialog = new AlertDialog.Builder(context)
                        .setView(dialogView)
                        .create();

                // Reference the dialog's buttons
                Button btnYes = dialogView.findViewById(R.id.btn_yes);
                Button btnNo = dialogView.findViewById(R.id.btn_no);

                // Set click listeners for the buttons
                btnYes.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        // Remove the order from the list
                        int currentPosition = holder.getAdapterPosition();
                        orderList.remove(currentPosition);
                        notifyItemRemoved(currentPosition);
                        notifyItemRangeChanged(currentPosition, orderList.size());
                        Toast.makeText(context, "Order cancelled", Toast.LENGTH_SHORT).show();
                        dialog.dismiss();
                    }
                });

                btnNo.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        dialog.dismiss();
                    }
                });

                dialog.show();
            }
        });
    }

    @Override
    public int getItemCount() {
        return orderList.size();
    }
}

package com.example.realizedvision;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
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
        TextView orderDetails;
        Button cancelOrderButton;

        public OrderViewHolder(View itemView) {
            super(itemView);
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
        holder.orderDetails.setText(order);

        holder.cancelOrderButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                // Create a confirmation dialog
                new AlertDialog.Builder(context)
                        .setTitle("Confirm Cancellation")
                        .setMessage("Are you sure you want to cancel this order?")
                        .setPositiveButton("Yes", new DialogInterface.OnClickListener() {
                            public void onClick(DialogInterface dialog, int which) {
                                // Remove the order from the list
                                int currentPosition = holder.getAdapterPosition();
                                orderList.remove(currentPosition);
                                notifyItemRemoved(currentPosition);
                                notifyItemRangeChanged(currentPosition, orderList.size());
                                Toast.makeText(context, "Order cancelled", Toast.LENGTH_SHORT).show();
                            }
                        })
                        .setNegativeButton("No", null)
                        .setIcon(android.R.drawable.ic_dialog_alert)
                        .show();
            }
        });
    }

    @Override
    public int getItemCount() {
        return orderList.size();
    }
}

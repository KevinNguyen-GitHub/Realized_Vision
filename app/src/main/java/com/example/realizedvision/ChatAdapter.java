package com.example.realizedvision;

import android.text.format.DateFormat;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;
import java.util.Date;

public class ChatAdapter extends RecyclerView.Adapter<ChatAdapter.MessageViewHolder> {

    private List<ChatMessage> messageList;
    private String currentUserId;

    public ChatAdapter(List<ChatMessage> messageList, String currentUserId) {
        this.messageList = messageList;
        this.currentUserId = currentUserId;
    }

    @NonNull
    @Override
    public MessageViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.chat_message_item, parent, false);
        return new MessageViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull MessageViewHolder holder, int position) {
        ChatMessage message = messageList.get(position);
        boolean isSentByCurrentUser = message.getSenderId().equals(currentUserId);
        holder.bind(message, isSentByCurrentUser);
    }

    @Override
    public int getItemCount() {
        return messageList.size();
    }

    static class MessageViewHolder extends RecyclerView.ViewHolder {
        LinearLayout messageContainer;
        TextView messageTextView;
        TextView messageTimeView;

        MessageViewHolder(@NonNull View itemView) {
            super(itemView);
            messageContainer = itemView.findViewById(R.id.messageContainer);
            messageTextView = itemView.findViewById(R.id.messageText);
            messageTimeView = itemView.findViewById(R.id.messageTime);
        }

        void bind(ChatMessage message, boolean isSentByCurrentUser) {
            messageTextView.setText(message.getText());
            messageContainer.setGravity(isSentByCurrentUser ? Gravity.END : Gravity.START);

            // Format timestamp to readable time
            long timestampMillis = message.getTimestamp() != null
                    ? message.getTimestamp().toDate().getTime()
                    : System.currentTimeMillis();
            String formattedTime = DateFormat.format("h:mm a", new Date(timestampMillis)).toString();
            messageTimeView.setText(formattedTime);
        }
    }
}

package com.example.realizedvision;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import java.util.List;

public class ChatListAdapter extends RecyclerView.Adapter<ChatListAdapter.ViewHolder> {

    public interface OnChatClickListener {
        void onChatClick(String userId);
    }

    private List<UserChat> chatList;
    private OnChatClickListener listener;

    public ChatListAdapter(List<UserChat> chatList, OnChatClickListener listener) {
        this.chatList = chatList;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.chat_list_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        UserChat chat = chatList.get(position);
        holder.userIdText.setText(chat.getUserId());
        holder.lastMessageText.setText(chat.getLastMessage());
        holder.itemView.setOnClickListener(v -> listener.onChatClick(chat.getUserId()));
    }

    @Override
    public int getItemCount() {
        return chatList.size();
    }

    static class ViewHolder extends RecyclerView.ViewHolder {
        TextView userIdText, lastMessageText;

        ViewHolder(View view) {
            super(view);
            userIdText = view.findViewById(R.id.userIdText);
            lastMessageText = view.findViewById(R.id.lastMessageText);
        }
    }
}

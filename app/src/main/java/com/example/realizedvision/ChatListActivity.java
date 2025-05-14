package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import com.google.firebase.firestore.*;
import java.util.ArrayList;
import java.util.List;

public class ChatListActivity extends AppCompatActivity {

    private static final String PRESET_USER_ID = "user_123";
    private static final String PRESET_VENDOR_ID = "vendor_456";

    private RecyclerView chatListRecyclerView;
    private ChatListAdapter chatListAdapter;
    private List<UserChat> chatList = new ArrayList<>();
    private FirebaseFirestore db = FirebaseFirestore.getInstance();
    private String currentUserId = PRESET_USER_ID;  // Or PRESET_VENDOR_ID if you’re testing as vendor

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat_list);

        chatListRecyclerView = findViewById(R.id.chatListRecyclerView);
        chatListAdapter = new ChatListAdapter(chatList, this::openChatRoom);
        chatListRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        chatListRecyclerView.setAdapter(chatListAdapter);

        loadUserChatRooms();
    }

    private void loadUserChatRooms() {
        db.collection("ChatRooms")
                .whereArrayContains("participants", currentUserId)
                .addSnapshotListener((snapshots, e) -> {
                    if (e != null || snapshots == null) return;

                    chatList.clear();
                    for (DocumentSnapshot doc : snapshots) {
                        String roomId = doc.getId();
                        List<String> participants = (List<String>) doc.get("participants");

                        if (participants != null) {
                            String otherUserId = "";
                            for (String participant : participants) {
                                if (!participant.equals(currentUserId)) {
                                    otherUserId = participant;
                                    break;
                                }
                            }

                            chatList.add(new UserChat(otherUserId, "Chat Room: " + roomId));
                        }
                    }
                    chatListAdapter.notifyDataSetChanged();
                });
    }

    private void openChatRoom(String otherUserId) {
        Intent intent = new Intent(this, ChatActivity.class);
        intent.putExtra("ROOM_ID", otherUserId);
        startActivity(intent);
    }
}

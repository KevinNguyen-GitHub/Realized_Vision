package com.example.realizedvision;

import android.os.Bundle;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.Timestamp;
import com.google.firebase.firestore.*;

import java.util.ArrayList;
import java.util.List;

public class ChatActivity extends AppCompatActivity {

    private static final String PRESET_USER_ID = "user_123";
    private static final String PRESET_VENDOR_ID = "vendor_456";

    private FirebaseFirestore db = FirebaseFirestore.getInstance();
    private RecyclerView chatRecyclerView;
    private ChatAdapter chatAdapter;
    private List<ChatMessage> chatMessages = new ArrayList<>();
    private EditText messageInput;
    private Button sendButton;

    private String currentUserId = PRESET_USER_ID; // or PRESET_VENDOR_ID if you test as vendor
    private String roomId;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat);

        roomId = getIntent().getStringExtra("ROOM_ID");

        if (roomId == null || roomId.isEmpty()) {
            Toast.makeText(this, "Chat Room ID is missing.", Toast.LENGTH_SHORT).show();
            finish();
            return;
        }

        chatRecyclerView = findViewById(R.id.chatRecyclerView);
        messageInput = findViewById(R.id.messageInput);
        sendButton = findViewById(R.id.sendButton);

        chatAdapter = new ChatAdapter(chatMessages, currentUserId);
        chatRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        chatRecyclerView.setAdapter(chatAdapter);

        TextView chatHeader = findViewById(R.id.chatHeader);
        loadChatRoomParticipants(chatHeader);

        listenForMessages();

        sendButton.setOnClickListener(v -> sendMessage());
    }

    /**
     * Loads the participants and updates the chat header with the other user's ID.
     */
    private void loadChatRoomParticipants(TextView chatHeader) {
        db.collection("ChatRooms")
                .document(roomId)
                .get()
                .addOnSuccessListener(doc -> {
                    List<String> participants = (List<String>) doc.get("participants");
                    if (participants != null) {
                        for (String participant : participants) {
                            if (!participant.equals(currentUserId)) {
                                chatHeader.setText(participant);
                                return;
                            }
                        }
                    }
                    chatHeader.setText(roomId);  // fallback
                })
                .addOnFailureListener(e -> chatHeader.setText("Chat Room: " + roomId));
    }

    /**
     * Listens for incoming messages in this chat room.
     */
    private void listenForMessages() {
        db.collection("ChatRooms")
                .document(roomId)
                .collection("messages")
                .orderBy("timestamp", Query.Direction.ASCENDING)
                .addSnapshotListener((snapshots, e) -> {
                    if (e != null || snapshots == null) return;

                    chatMessages.clear();
                    for (DocumentSnapshot doc : snapshots) {
                        ChatMessage message = doc.toObject(ChatMessage.class);
                        if (message != null) {
                            chatMessages.add(message);
                        }
                    }
                    chatAdapter.notifyDataSetChanged();
                    chatRecyclerView.scrollToPosition(chatMessages.size() - 1);
                });
    }

    /**
     * Sends a message to this chat room.
     */
    private void sendMessage() {
        String text = messageInput.getText().toString().trim();
        if (text.isEmpty()) return;

        ChatMessage message = new ChatMessage(text, currentUserId, Timestamp.now());

        db.collection("ChatRooms")
                .document(roomId)
                .collection("messages")
                .add(message);

        messageInput.setText("");
    }
}

package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.*;

import java.util.ArrayList;
import java.util.List;

public class MessagesActivity extends AppCompatActivity {

    private static final String PRESET_USER_ID = "user_123";     // Replace with FirebaseAuth if needed
    private static final String PRESET_VENDOR_ID = "vendor_456"; // Replace with FirebaseAuth if needed

    private FirebaseUser currentUser;
    private FirebaseFirestore firestore;
    private String currentUserId = PRESET_USER_ID;  // Change to PRESET_VENDOR_ID to test as vendor

    private RecyclerView chatListRecyclerView;
    private ChatListAdapter chatListAdapter;
    private List<UserChat> chatList = new ArrayList<>();
    private FirebaseFirestore db = FirebaseFirestore.getInstance();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat_list);

        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        setupNavigation();
        setupChatList();
    }

    private void setupNavigation() {
        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView favoritesIcon = findViewById(R.id.favorites_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);

        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        favoritesIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        profileIcon.setOnClickListener(view -> handleProfileNavigation());
    }

    private void setupChatList() {
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

    private void handleProfileNavigation() {
        if (currentUser != null) {
            String userId = currentUser.getUid();
            DocumentReference userDocRef = firestore.collection("Users").document(userId);

            userDocRef.get().addOnCompleteListener(task -> {
                if (task.isSuccessful()) {
                    DocumentSnapshot snapshot = task.getResult();
                    if (snapshot.exists()) {
                        Boolean isVendor = snapshot.getBoolean("isVendor");
                        if (Boolean.TRUE.equals(isVendor)) {
                            navigateTo(StorefrontActivity.class);
                        } else if (Boolean.FALSE.equals(isVendor)) {
                            navigateTo(ProfileActivity.class);
                        } else {
                            showToast("User data not found.");
                        }
                    } else {
                        showToast("User data not found.");
                    }
                } else {
                    showToast("Error: " + task.getException().getMessage());
                }
            });
        }
    }

    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(this, targetActivity);
        startActivity(intent);
    }

    private void showToast(String message) {
        Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
    }
}

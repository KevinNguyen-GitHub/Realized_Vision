package com.example.realizedvision;

public class UserChat {
    private String userId;
    private String lastMessage;

    public UserChat() {}  // Required by Firestore

    public UserChat(String userId, String lastMessage) {
        this.userId = userId;
        this.lastMessage = lastMessage;
    }

    public String getUserId() { return userId; }
    public String getLastMessage() { return lastMessage; }
}

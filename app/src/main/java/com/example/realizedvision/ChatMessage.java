package com.example.realizedvision;

import com.google.firebase.Timestamp;

public class ChatMessage {
    private String text;
    private String senderId;
    private Timestamp timestamp;

    public ChatMessage() {}  // Required by Firestore

    public ChatMessage(String text, String senderId, Timestamp timestamp) {
        this.text = text;
        this.senderId = senderId;
        this.timestamp = timestamp;
    }

    public String getText() { return text; }
    public String getSenderId() { return senderId; }
    public Timestamp getTimestamp() { return timestamp; }
}

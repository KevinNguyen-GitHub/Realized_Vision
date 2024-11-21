package com.example.realizedvision;

import android.os.Bundle;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;

public class MainActivity extends AppCompatActivity {

    private FirebaseFirestore db;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        db = FirebaseFirestore.getInstance();
        TextView messageView = findViewById(R.id.textView2);

        // Assuming you have the user's document ID stored somewhere
        String userId = "your_user_id"; // Replace with the actual user ID

        DocumentReference docRef = db.collection("users").document(userId);
        docRef.get().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                DocumentSnapshot document = task.getResult();
                if (document.exists()) {
                    String firstName = document.getString("firstName");
                    String lastName = document.getString("lastName");
                    String welcomeMessage = "Welcome, " + firstName + " " + lastName + "!";
                    messageView.setText(welcomeMessage);
                } else {
                    // Handle case where document doesn't exist
                    messageView.setText("User not found.");
                }
            } else {
                // Handle errors
                messageView.setText("Error retrieving user data.");
            }
        });
    }
}
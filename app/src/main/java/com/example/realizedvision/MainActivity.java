package com.example.realizedvision;

import android.os.Bundle;
import android.widget.TextView;
import androidx.appcompat.app.AppCompatActivity;

public class MainActivity extends AppCompatActivity {

    private TextView tvWelcomeMessage;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Initialize views
        tvWelcomeMessage = findViewById(R.id.textView2);

        // Get data from intent
        String firstName = getIntent().getStringExtra("firstName");
        String lastName = getIntent().getStringExtra("lastName");

        // Set welcome message
        if (firstName != null && lastName != null) {
            tvWelcomeMessage.setText("Welcome, " + firstName + " " + lastName + "!");
        } else {
            tvWelcomeMessage.setText("Welcome!");
        }
    }
}

package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.ImageView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;


public class MessagesActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_messages);

        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView favoritesIcon = findViewById(R.id.favorites_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);

        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        favoritesIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));

    }

    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MessagesActivity.this, targetActivity);
        startActivity(intent);
    }



}

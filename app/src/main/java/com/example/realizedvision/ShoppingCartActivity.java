package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;
import android.widget.ImageView;

import androidx.appcompat.app.AppCompatActivity;

public class ShoppingCartActivity extends AppCompatActivity{

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_shopping_cart);

        ImageView homeIcon = findViewById(R.id.home_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
        ImageView favoritesIcon = findViewById(R.id.favorites_icon);
        Button favoritesButton = findViewById(R.id.Favorites_button);

        homeIcon.setOnClickListener(view -> navigateTo(MainActivity.class));
        favoritesIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        favoritesButton.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));

    }
    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(ShoppingCartActivity.this, targetActivity);
        startActivity(intent);
    }
}

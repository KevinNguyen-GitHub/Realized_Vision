package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.CollectionReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.EventListener;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.FirebaseFirestoreException;
import com.google.firebase.firestore.QueryDocumentSnapshot;
import com.google.firebase.firestore.QuerySnapshot;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class MainActivity extends AppCompatActivity implements ItemAdapter.OnItemClickListener{
    private RecyclerView recyclerView;
    private ItemAdapter itemAdapter;
    private List<Item> itemList;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

//        Connect to db, find user instance
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        firestore = FirebaseFirestore.getInstance();

//      Create recycler view to hold elements
        recyclerView = findViewById(R.id.mainRecyclerView);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        itemList = new ArrayList<>();
        itemAdapter = new ItemAdapter(this, itemList);
        recyclerView.setAdapter(itemAdapter);

        itemAdapter.setOnItemClickListener(this);


        fetchItemsfromFirestore();

        ImageView favoriteIcon = findViewById(R.id.favorites_icon);
        ImageView messageIcon = findViewById(R.id.messages_icon);
        ImageView profileIcon = findViewById(R.id.profile_icon);
//        Navigate to desired elements when clicked
        favoriteIcon.setOnClickListener(view -> navigateTo(FavoritesActivity.class));
        messageIcon.setOnClickListener(view -> navigateTo(MessagesActivity.class));
        profileIcon.setOnClickListener(view -> navigateTo(ProfileActivity.class));


    }
    //   Helper function for activity navigation
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MainActivity.this, targetActivity);
        startActivity(intent);
    }

//    Retrieving items from database, adding them to item list to display
    private void fetchItemsfromFirestore(){
        FirebaseFirestore db = FirebaseFirestore.getInstance();
        String userId = currentUser.getUid();

        db.collection("Storefront").get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    Log.d("MainActivity", "Number of items fetched: " + queryDocumentSnapshots.size());
                    itemList.clear();
                    for(QueryDocumentSnapshot document : queryDocumentSnapshots){
                        Item item = document.toObject(Item.class);
                        Log.d("MainActivity", "Item: " + item.getName());
                        String itemId = item.getItemID();

                        db.collection("Users")
                                .document(userId)
                                .collection("Favorites")
                                .document(itemId)
                                .get()
                                .addOnSuccessListener(favoriteDocument -> {
                                    if(favoriteDocument.exists()){
                                        item.setFavorite(true);
                                    }
                                    else{
                                        item.setFavorite(false);
                                    }
                                    itemList.add(item);
                                    itemAdapter.notifyDataSetChanged();
                                }).addOnFailureListener(e ->{
                                    Log.d("Main Activity", "Error checking favorites", e);
                                });
                    }
                }).addOnFailureListener(e ->{
                    Log.e("Main Activity", "Error fetching products: " + e.getMessage());
                });
    }

    @Override
    public void onFavoriteClick(int position){
        Item item = itemList.get(position);
        item.setFavorite(!item.isFavorite());
        itemAdapter.notifyItemChanged(position);

        String itemId = item.getItemID();
        Log.d("Main Activity", "Heart button clicked for: " + item.getName());

        String userID = currentUser.getUid();
        //Favorites subcollection under users collection
        CollectionReference favoritesRef = FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userID)
                .collection("Favorites");

        favoritesRef.document(itemId).get().addOnCompleteListener(task ->{
            if (task.isSuccessful()){
                DocumentSnapshot document  = task.getResult();
                if(document.exists()){//Item already in favorites, remove
                    favoritesRef.document(itemId).delete()
                            .addOnSuccessListener(avoid ->{
                                Log.d("Main Activity", "Removed item from favorites: "+ item.getName());
                                Toast.makeText(MainActivity.this, "Removed from favorites", Toast.LENGTH_SHORT).show();

                            }).addOnFailureListener(e ->{
                                Log.e("Main Activity", "Failure removing item from favorites", e);
                                Toast.makeText(MainActivity.this, "Failed to remove item from favorites", Toast.LENGTH_SHORT).show();
                            });
                }
                else{//add item to favorites
                    favoritesRef.document(itemId).set(item)
                            .addOnSuccessListener(avoid ->{
                                Log.d("Main Activity", "Added item to favorites: " + item.getName());
                                Toast.makeText(MainActivity.this, "Added to favorites", Toast.LENGTH_SHORT).show();
                            }).addOnFailureListener(e ->{
                                Log.e("Main Activity", "Failed to add item to favorites" + item.getName());
                                Toast.makeText(MainActivity.this, "Failed to favorite item", Toast.LENGTH_SHORT).show();
                            });


                }
            }
            else{
                Log.e("Main Activity", "Failed to check favorites", task.getException());
                Toast.makeText(MainActivity.this, "Failed to check favorites", Toast.LENGTH_SHORT).show();
            }
        });

    }

    @Override
    public void onCartClick(int position){
        Item item = itemList.get(position);
        Log.d("Main Activity", "Cart button clicked for: " + item.getName());

        String itemId = item.getItemID();
        String userID = currentUser.getUid();
        CollectionReference cartRef = FirebaseFirestore.getInstance()
                .collection("Users")
                .document(userID)
                .collection("Shopping Cart");

        cartRef.document(itemId).get().addOnCompleteListener(task -> {
            if (task.isSuccessful()){
                DocumentSnapshot document = task.getResult();
                if(document.exists()){
                    Toast.makeText(MainActivity.this, "Item already in shopping cart", Toast.LENGTH_SHORT).show();
                }
                else{
                    cartRef.document(itemId).set(item)
                            .addOnSuccessListener(avoid -> {
                                Log.d("Main Activity", "Added item to cart" + item.getName());
                                Toast.makeText(MainActivity.this, "Item added to cart", Toast.LENGTH_SHORT).show();
                            }).addOnFailureListener(e ->{
                                Log.e("Main Activity", "Failed to add item to cart" + item.getName());
                                Toast.makeText(MainActivity.this, "Failed to add item", Toast.LENGTH_SHORT).show();

;                            });
                }
            }
            else{
                Log.d("Main Activity", "Failed to check shopping cart");
                Toast.makeText(MainActivity.this, "Failed to check Cart", Toast.LENGTH_SHORT).show();
            }
        });
    }

    @Override
    public void onItemClick(int position) {
        Item item = itemList.get(position);
        Intent intent = new Intent(MainActivity.this, ExpandItem.class);
        intent.putExtra("item_id", item.getItemID()); // Pass the item ID to the details activity
        startActivity(intent);
    }


}


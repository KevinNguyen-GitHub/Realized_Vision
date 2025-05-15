//package com.example.realizedvision;
//
//import android.os.Bundle;
//import androidx.appcompat.app.AppCompatActivity;
//import androidx.recyclerview.widget.LinearLayoutManager;
//import androidx.recyclerview.widget.RecyclerView;
//import java.util.ArrayList;
//import java.util.List;
//
//public class cancel_order extends AppCompatActivity {
//
//    private RecyclerView recyclerView;
//    private orderAdapter adapter;
//    private List<String> orderList;
//
//    @Override
//    protected void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);
//        setContentView(R.layout.tester);
//
//        // Initialize a sample list of orders
//        orderList = new ArrayList<>();
//        orderList.add("Order #1001 - chair");
//        orderList.add("Order #1002 - doll");
//        orderList.add("Order #1003 - picture");
//
//        // Set up the RecyclerView
//        recyclerView = findViewById(R.id.recycler_view);
//        recyclerView.setLayoutManager(new LinearLayoutManager(this));
//        adapter = new orderAdapter(this, orderList);
//        recyclerView.setAdapter(adapter);
//    }
//}

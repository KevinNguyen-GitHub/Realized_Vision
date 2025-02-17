package com.example.realizedvision

import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.ImageView
import android.widget.TextView
import androidx.appcompat.app.AppCompatActivity
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView

class ViewClassActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_class) // Set the layout with RecyclerView

        val selectedDate = intent.getStringExtra("SELECTED_DATE")
        val selectedDateTextView = findViewById<TextView>(R.id.selectedDate)
        selectedDateTextView.text = "Scheduele for\n $selectedDate"

        val calendarIcon = findViewById<ImageView>(R.id.calendar_icon)
        calendarIcon.setOnClickListener { view: View? -> navigateTo(ViewCalendarActivity::class.java) }


        // Sample data
        val classList = listOf(
            ClassInfo("Test1", "Test1 class", "9:00 AM", "10:00 AM"),
            ClassInfo("Test2", "Test2 class", "10:30 AM","11:30 AM"),
            ClassInfo("Test3", "Test3 class", "12:00 PM"," 1:00 PM"),
            ClassInfo("Test4", "Test4 class", "1:30 PM", "2:30 PM"),
            ClassInfo("Test5", "Test5 class", "3:00 PM ","4:00 PM")
        )

        // Get the RecyclerView
        val recyclerView: RecyclerView = findViewById(R.id.viewClassRecycler)


        // Set the layout manager
        recyclerView.layoutManager = LinearLayoutManager(this)

        // Create and set the adapter
        val adapter = ClassAdapter(classList)
        recyclerView.adapter = adapter
    }
    private fun navigateTo(activityClass: Class<*>) {
        val intent = Intent(this, activityClass)
        startActivity(intent)
    }
}
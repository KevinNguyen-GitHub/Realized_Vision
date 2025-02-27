package com.example.realizedvision

import android.app.TimePickerDialog
import android.content.Intent
import android.icu.util.Calendar
import android.os.Bundle
import android.util.Log
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import android.widget.Toast
import androidx.appcompat.app.AlertDialog
import androidx.appcompat.app.AppCompatActivity
import androidx.appcompat.view.ContextThemeWrapper
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.google.firebase.Timestamp
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FirebaseFirestore
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale

class ViewClassActivity : AppCompatActivity() {

    private lateinit var firestore: FirebaseFirestore
    private lateinit var auth: FirebaseAuth
    private lateinit var adapter: ClassAdapter
    private lateinit var recyclerView: RecyclerView
    private var selectedDate: String? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_class)

        firestore = FirebaseFirestore.getInstance()
        auth = FirebaseAuth.getInstance()

        selectedDate = intent.getStringExtra("selectedDate")
        val selectedDateTextView = findViewById<TextView>(R.id.selectedDate)
        selectedDateTextView.text = "Schedule for\n$selectedDate"

        recyclerView = findViewById(R.id.viewClassRecycler)
        recyclerView.layoutManager = LinearLayoutManager(this)
        adapter = ClassAdapter()
        recyclerView.adapter = adapter

        loadClasses()

        val addClassButton = findViewById<Button>(R.id.add_class_button)
        addClassButton.setOnClickListener {showAddClassDialog() }

    }

    private fun loadClasses() {
        val currentUser = auth.currentUser
        if (currentUser != null) {
            val vendorId = currentUser.uid
            firestore.collection("Classes")
                .whereEqualTo("vendorID", vendorId)
                .get()
                .addOnSuccessListener { documents ->
                    val classes = documents.mapNotNull { document ->
                        try {
                            val classID = document.getString("classID") ?: return@mapNotNull null
                            val vendorID = document.getString("vendorID") ?: return@mapNotNull null
                            val title = document.getString("title") ?: return@mapNotNull null
                            val description = document.getString("description") ?: return@mapNotNull null
                            val startTimeTimestamp = document.getTimestamp("startTime") ?: return@mapNotNull null
                            val endTimeTimestamp = document.getTimestamp("endTime") ?: return@mapNotNull null
                            val startTime = formatTimestampTo12Hour(startTimeTimestamp)
                            val endTime = formatTimestampTo12Hour(endTimeTimestamp)

                            ClassInfo(classID, vendorID, title, description, startTime, endTime)
                        } catch (e: Exception) {
                            Log.e("ViewClassActivity", "Error parsing document: ${document.id}", e)
                            null
                        }
                    }
                    adapter.submitList(classes)
                }
                .addOnFailureListener { exception ->
                    Log.w("ViewClassActivity", "Error getting classes", exception)
                    Toast.makeText(this, "Error getting classes", Toast.LENGTH_SHORT).show()
                }
        }
    }

    private fun formatTimestampTo12Hour(timestamp: Timestamp): String {
        val date = timestamp.toDate()
        val sdf = SimpleDateFormat("hh:mm a", Locale.getDefault())
        return sdf.format(date)
    }

    private fun showAddClassDialog() {
        val dialogView = layoutInflater.inflate(R.layout.dialog_add_class, null)
        val titleEdit = dialogView.findViewById<EditText>(R.id.classTitle)
        val descriptionEdit = dialogView.findViewById<EditText>(R.id.classDescription)
        val startEdit = dialogView.findViewById<EditText>(R.id.classStartTime)
        val endEdit = dialogView.findViewById<EditText>(R.id.classEndTime)

        startEdit.setOnClickListener { showTimePickerDialog(startEdit) }
        endEdit.setOnClickListener { showTimePickerDialog(endEdit) }

        val dialog = AlertDialog.Builder(ContextThemeWrapper(this, R.style.CustomAlertDialog))
            .setView(dialogView)
            .setTitle("Add Class")
            .setPositiveButton("Add") { dialog, _ ->
                val title = titleEdit.text.toString()
                val description = descriptionEdit.text.toString()
                val startTime = startEdit.text.toString()
                val endTime = endEdit.text.toString()

                if (title.isNotEmpty() && description.isNotEmpty() && startTime.isNotEmpty() && endTime.isNotEmpty()) {
                    addClassToFirestore(title, description, startTime, endTime)
                } else {
                    Toast.makeText(this, "Please fill in all fields.", Toast.LENGTH_SHORT).show()
                }
                dialog.dismiss()
            }
            .setNegativeButton("Cancel") { dialog, _ -> dialog.dismiss() }
            .create()

        dialog.show()
    }

    private fun showTimePickerDialog(editText: EditText) {
        val calendar = Calendar.getInstance()
        val hour = calendar.get(Calendar.HOUR_OF_DAY)
        val minute = calendar.get(Calendar.MINUTE)

        val timePickerDialog = TimePickerDialog(
            this,
            R.style.CustomTimePickerDialog,
            { _, selectedHour, selectedMinute ->
                val formattedTime: String = if (selectedHour == 0) {
                    String.format(Locale.getDefault(), "%02d:%02d AM", 12, selectedMinute)
                } else if (selectedHour < 12) {
                    String.format(Locale.getDefault(), "%02d:%02d AM", selectedHour, selectedMinute)
                } else if (selectedHour == 12) {
                    String.format(Locale.getDefault(), "%02d:%02d PM", selectedHour, selectedMinute)
                } else {
                    String.format(Locale.getDefault(), "%02d:%02d PM", selectedHour - 12, selectedMinute)
                }
                editText.setText(formattedTime)
            },
            hour,
            minute,
            false
        )

        timePickerDialog.show()
    }

    private fun addClassToFirestore(title: String, description: String, startTime: String, endTime: String) {
        val currentUser = auth.currentUser
        if (currentUser != null) {
            val vendorId = currentUser.uid
            val startTimestamp = combineDateAndTime(selectedDate!!, startTime)
            val endTimestamp = combineDateAndTime(selectedDate!!, endTime)

            val classData = hashMapOf(
                "vendorID" to vendorId,
                "title" to title,
                "description" to description,
                "startTime" to startTimestamp,
                "endTime" to endTimestamp
            )

            firestore.collection("Classes")
                .add(classData)
                .addOnSuccessListener { documentReference ->
                    documentReference.update("classID", documentReference.id)
                    Toast.makeText(this, "Class added successfully.", Toast.LENGTH_SHORT).show()
                    loadClasses() // Refresh the list
                }
                .addOnFailureListener { e ->
                    Log.w("ViewClassActivity", "Error adding document", e)
                    Toast.makeText(this, "Failed to add class.", Toast.LENGTH_SHORT).show()
                }
        }
    }

    private fun combineDateAndTime(date: String, time: String): Timestamp {
        val formatter = SimpleDateFormat("MMMM dd, yyyy hh:mm a", Locale.getDefault())
        val dateTimeString = "$date $time"
        val dateObject = formatter.parse(dateTimeString) ?: Date()
        return Timestamp(dateObject)
    }

    private fun navigateTo(activityClass: Class<*>) {
        val intent = Intent(this, activityClass)
        startActivity(intent)
    }
}
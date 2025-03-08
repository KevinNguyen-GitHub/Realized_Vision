package com.example.realizedvision

import android.app.TimePickerDialog
import android.content.Intent
import android.icu.util.Calendar
import android.os.Bundle
import android.util.Log
import android.view.View
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
import com.google.firebase.firestore.FieldValue
import com.google.firebase.firestore.FirebaseFirestore
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale

class ViewClassActivity : AppCompatActivity(), ClassAdapter.OnItemClickListener {

    private lateinit var firestore: FirebaseFirestore
    private lateinit var auth: FirebaseAuth
    private lateinit var adapter: ClassAdapter
    private lateinit var recyclerView: RecyclerView
    private var selectedDate: String? = null
    private var isRemoveMode = false
    private var viewingUserId: String? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_class)

        firestore = FirebaseFirestore.getInstance()
        auth = FirebaseAuth.getInstance()

        viewingUserId = intent.getStringExtra("viewingUserId") ?: auth.currentUser?.uid

        selectedDate = intent.getStringExtra("selectedDate")
        val selectedDateTextView = findViewById<TextView>(R.id.selectedDate)
        selectedDateTextView.text = "Schedule for\n$selectedDate"

        recyclerView = findViewById(R.id.viewClassRecycler)
        recyclerView.layoutManager = LinearLayoutManager(this)
        adapter = ClassAdapter(viewingUserId != auth.currentUser?.uid)
        adapter.setOnItemClickListener(this)
        recyclerView.adapter = adapter

        loadClasses()

        val addClassButton = findViewById<Button>(R.id.add_class_button)
        val removeClassButton = findViewById<Button>(R.id.remove_class_button)

        if(viewingUserId != auth.currentUser?.uid) {
            addClassButton.visibility = View.GONE
            removeClassButton.visibility = View.GONE
        }

        addClassButton.setOnClickListener {showAddClassDialog() }
        removeClassButton.setOnClickListener {
            isRemoveMode = !isRemoveMode
            if (isRemoveMode) {
                removeClassButton.text = "Cancel"
                Toast.makeText(this, "Select a class to remove", Toast.LENGTH_SHORT).show()
            } else {
                removeClassButton.text = "Remove"
            }
        }

    }

    override fun onItemClick(classInfo: ClassInfo) {
        if (isRemoveMode) {
            showRemoveConfirmationDialog(classInfo)
        }
    }

    override fun onReserveClick(classInfo: ClassInfo) {
        showReserveSeatDialog(classInfo)
    }

    private fun showReserveSeatDialog(classInfo: ClassInfo) {
        val dialog = AlertDialog.Builder(ContextThemeWrapper(this, R.style.CustomAlertDialog))
            .setTitle("Reserve Class")
            .setMessage(
                "Are you sure you want to reserve:\n" +
                        "\nTitle: ${classInfo.title}" +
                        "\nDescription: ${classInfo.description}" +
                        "\nStart Time: ${classInfo.startTime}" +
                        "\nEnd Time: ${classInfo.endTime}"
            )
            .setPositiveButton("Yes") { _, _ ->
                reserveSeat(classInfo)
            }
            .setNegativeButton("No") { dialog, _ ->
                dialog.dismiss()

            }
            .create()
        dialog.show()
    }

//    private fun navigateToViewSeatsActivity(classInfo: ClassInfo){
//        val intent = Intent(this, ViewSeatsActivity::class.java)
//        intent.putExtra("classId", classInfo.classID)
//        startActivity(intent)
//    }

    private fun reserveSeat(classInfo: ClassInfo) {
        val currentUser = auth.currentUser ?: return
        val userRef = firestore.collection("users").document(currentUser.uid)
        userRef.get().addOnSuccessListener { documentSnapshot ->
            if(documentSnapshot.exists()){
                val firstName = documentSnapshot.getString("firstName") ?: ""
                val lastName = documentSnapshot.getString("lastName") ?: ""

                val classRef = firestore.collection("Classes").document(classInfo.classID)
                firestore.runTransaction { transaction ->
                    val classSnapshot = transaction.get(classRef)
                    val currentSeats = classSnapshot.getLong("currentSeats") ?: 0
                    val sizeLimit = classSnapshot.getLong("sizeLimit") ?: 0

                    if (currentSeats < sizeLimit) {
                        transaction.update(classRef, "currentSeats", FieldValue.increment(1))

                        val seatData = hashMapOf(
                            "userId" to currentUser.uid,
                            "firstName" to firstName,
                            "lastName" to lastName,
                            "email" to currentUser.email
                        )
                        val seatsRef = classRef.collection("Seats").document(currentUser.uid)
                        transaction.set(seatsRef, seatData)
                        true
                    } else {
                        false
                    }
                }.addOnSuccessListener { result ->
                    if(result){
                        Toast.makeText(this, "Seat Reserved!", Toast.LENGTH_SHORT).show()
                        loadClasses()
                    } else{
                        Toast.makeText(this, "Class Full!", Toast.LENGTH_SHORT).show()
                    }
                }.addOnFailureListener { e ->
                    Log.w("ViewClassActivity", "Error reserving seat", e)
                    Toast.makeText(this, "Reservation Failed", Toast.LENGTH_SHORT).show()
                }

            }
        }
    }

    private fun showRemoveConfirmationDialog(classInfo: ClassInfo) {
        val dialog = AlertDialog.Builder(ContextThemeWrapper(this, R.style.CustomAlertDialog))
            .setTitle("Remove Class")
            .setMessage("Are you sure you want to remove:\n" +
                    "\nTitle: ${classInfo.title}" +
                    "\nDescription: ${classInfo.description}" +
                    "\nStart Time: ${classInfo.startTime}" +
                    "\nEnd Time: ${classInfo.endTime}")
            .setPositiveButton("Yes") { _, _ ->
                removeClassFromFirestore(classInfo)
            }
            .setNegativeButton("No") { dialog, _ ->
                dialog.dismiss()

            }
            .create()
        dialog.show()
    }

    private fun removeClassFromFirestore(classInfo: ClassInfo) {
        firestore.collection("Classes")
            .document(classInfo.classID)
            .delete()
            .addOnSuccessListener {
                Toast.makeText(this, "Class removed successfully.", Toast.LENGTH_SHORT).show()
                loadClasses()
            }
            .addOnFailureListener { e ->
                Log.w("ViewClassActivity", "Error removing document", e)
                Toast.makeText(this, "Failed to remove class.", Toast.LENGTH_SHORT).show()
            }
    }

    private fun loadClasses() {
        val currentUser = auth.currentUser
        if (currentUser != null) {
            val vendorId = currentUser.uid
            val startOfDayTimestamp = getStartOfDayTimestamp(selectedDate!!)
            val endOfDayTimestamp = getEndOfDayTimestamp(selectedDate!!)
            firestore.collection("Classes")
                .whereEqualTo("vendorID", vendorId)
                .whereGreaterThanOrEqualTo("startTime", startOfDayTimestamp)
                .whereLessThanOrEqualTo("startTime", endOfDayTimestamp)
                .orderBy("startTime")
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
                            val sizeLimit = document.getLong("sizeLimit") ?: 0
                            val currentSeats = document.getLong("currentSeats") ?: 0

                            ClassInfo(classID, vendorID, title, description, startTime, endTime, currentSeats, sizeLimit)
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

    //Helper functions that get the start and end time
    //of a given day for database queries
    private fun getStartOfDayTimestamp(dateString: String): Timestamp {
        val formatter = SimpleDateFormat("MMMM dd, yyyy", Locale.getDefault())
        val date = formatter.parse(dateString) ?: Date()
        val calendar = Calendar.getInstance()
        calendar.time = date
        calendar.set(Calendar.HOUR_OF_DAY, 0)
        calendar.set(Calendar.MINUTE, 0)
        calendar.set(Calendar.SECOND, 0)
        calendar.set(Calendar.MILLISECOND, 0)
        return Timestamp(calendar.time)
    }

    private fun getEndOfDayTimestamp(dateString: String): Timestamp {
        val formatter = SimpleDateFormat("MMMM dd, yyyy", Locale.getDefault())
        val date = formatter.parse(dateString) ?: Date()
        val calendar = Calendar.getInstance()
        calendar.time = date
        calendar.set(Calendar.HOUR_OF_DAY, 23)
        calendar.set(Calendar.MINUTE, 59)
        calendar.set(Calendar.SECOND, 59)
        calendar.set(Calendar.MILLISECOND, 999)
        return Timestamp(calendar.time)
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
        val sizeLimitEdit = dialogView.findViewById<EditText>(R.id.editClassSizeLimit)

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
                val sizeLimitString = sizeLimitEdit.text.toString()

                if (title.isNotEmpty() && description.isNotEmpty() && startTime.isNotEmpty() && endTime.isNotEmpty() && sizeLimitString.isNotEmpty()) {
                    val sizeLimit = sizeLimitString.toLongOrNull()
                    if (sizeLimit != null){
                        addClassToFirestore(title, description, startTime, endTime, sizeLimit)
                    }
                    else{
                        Toast.makeText(this, "Invalid size limit.", Toast.LENGTH_SHORT).show()
                    }
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

    private fun addClassToFirestore(title: String, description: String, startTime: String, endTime: String, sizeLimit: Long) {
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
                "endTime" to endTimestamp,
                "sizeLimit" to sizeLimit,
                "currentSeats" to 0L
            )

            firestore.collection("Classes")
                .add(classData)
                .addOnSuccessListener { documentReference ->
                    Log.d("ViewClassActivity", "Document added with ID: ${documentReference.id}")
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
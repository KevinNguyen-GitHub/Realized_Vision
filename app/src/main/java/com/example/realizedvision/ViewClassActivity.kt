package com.example.realizedvision

import android.app.TimePickerDialog
import android.content.Intent
import android.icu.util.Calendar
import android.os.Bundle
import android.util.Log
import android.view.View
import android.widget.*
import androidx.appcompat.app.AlertDialog
import androidx.appcompat.app.AppCompatActivity
import androidx.appcompat.view.ContextThemeWrapper
import androidx.lifecycle.lifecycleScope
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.google.firebase.Timestamp
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FieldValue
import com.google.firebase.firestore.FirebaseFirestore
import kotlinx.coroutines.launch
import kotlinx.coroutines.tasks.await
import java.text.SimpleDateFormat
import java.util.*
import java.util.Locale.*

class ViewClassActivity : AppCompatActivity(), ClassAdapter.OnItemClickListener {

    /* ───────────── Firebase – lazily fetched only once ───────────── */
    private val db   by lazy { FirebaseFirestore.getInstance() }
    private val auth by lazy { FirebaseAuth.getInstance() }

    /* ───────────── views ───────────── */
    private val selectedDateTv by lazy { findViewById<TextView>(R.id.selectedDate)      }
    private val list            by lazy { findViewById<RecyclerView>(R.id.viewClassRecycler) }
    private val btnAdd          by lazy { findViewById<Button>(R.id.add_class_button)   }
    private val btnRemove       by lazy { findViewById<Button>(R.id.remove_class_button)}

    /* ───────────── state ───────────── */
    private val adapter         = ClassAdapter(true).also { it.setOnItemClickListener(this) }
    private var removeMode      = false
    private val dateStr         by lazy { intent.getStringExtra("selectedDate") ?: "" }
    private val profileId       by lazy {
        intent.getStringExtra("selectedProfileId") ?: auth.currentUser?.uid
    }

    /* ══════════════════ life-cycle ══════════════════ */
    override fun onCreate(b: Bundle?) {
        super.onCreate(b)
        setContentView(R.layout.activity_view_class)

        selectedDateTv.text = getString(R.string.class_header, dateStr)

        list.layoutManager = LinearLayoutManager(this)
        list.adapter       = adapter

        // Hide vendor-only buttons for other users
        val myUid = auth.currentUser?.uid
        if (profileId != myUid) {
            btnAdd.visibility = View.GONE
            btnRemove.visibility = View.GONE
        }

        btnAdd.setOnClickListener { showAddDialog() }
        btnRemove.setOnClickListener { toggleRemoveMode() }

        loadClasses()
    }

    override fun onRestart() {
        super.onRestart()
        loadClasses()
    }

    /* ───────────── adapter callbacks ───────────── */
    override fun onItemClick(info: ClassInfo) {
        if (removeMode) confirmDelete(info)
    }

    override fun onReserveClick(info: ClassInfo) = confirmReserve(info)

    /* ═══════════════════ CRUD ═════════════════════ */
    private fun loadClasses() = lifecycleScope.launch {
        try {
            val start = dateStr.toStartOfDay()
            val end   = dateStr.toEndOfDay()

            val qs = db.collection("Classes")
                .whereEqualTo("vendorID", profileId)
                .whereGreaterThanOrEqualTo("startTime", start)
                .whereLessThanOrEqualTo("startTime", end)
                .orderBy("startTime")
                .get()
                .await()

            val list = qs.mapNotNull { it.toClassInfoOrNull() }
            adapter.submitList(list)
        } catch (e: Exception) {
            toast("Failed to load classes")
            Log.e(TAG, "loadClasses", e)
        }
    }

    private fun reserveSeat(info: ClassInfo) = lifecycleScope.launch {
        val user = auth.currentUser ?: return@launch
        val seatRef  = db.classDoc(info.classID).collection("Seats").document(user.uid)
        val classRef = db.classDoc(info.classID)

        if (seatRef.get().await().exists()) {
            toast("Seat already reserved"); return@launch
        }

        db.runTransaction { tr ->
            val snap        = tr.get(classRef)
            val current     = snap.getLong("currentSeats") ?: 0
            val max         = snap.getLong("sizeLimit") ?: 0
            if (current >= max) throw IllegalStateException("Full")

            tr.update(classRef, "currentSeats", FieldValue.increment(1))
            val userDoc = db.userDoc(user.uid).get().result
            val first   = userDoc?.getString("firstName") ?: ""
            val last    = userDoc?.getString("lastName") ?: ""

            tr.set(seatRef, mapOf(
                "userId" to user.uid,
                "firstName" to first,
                "lastName"  to last,
                "email"     to user.email
            ))
        }.runCatching {
            toast("Seat reserved!"); loadClasses()
        }.onFailure {
            toast(it.message ?: "Reservation failed")
        }
    }

    private fun removeClass(info: ClassInfo) = lifecycleScope.launch {
        runCatching {
            db.classDoc(info.classID).delete().await()
        }.onSuccess {
            toast("Class removed"); loadClasses()
        }.onFailure {
            toast("Remove failed"); Log.e(TAG, "removeClass", it)
        }
    }

    /* ═════════════════ dialogs ════════════════════ */
    private fun confirmReserve(info: ClassInfo) =
        alert("Reserve Class", info.detail()) { reserveSeat(info) }

    private fun confirmDelete(info: ClassInfo) =
        alert("Remove Class", info.detail()) { removeClass(info) }

    private fun showAddDialog() {
        val v = layoutInflater.inflate(R.layout.dialog_add_class, null)
        val titleEt = v.findViewById<EditText>(R.id.classTitle)
        val descEt  = v.findViewById<EditText>(R.id.classDescription)
        val startEt = v.findViewById<EditText>(R.id.classStartTime)
        val endEt   = v.findViewById<EditText>(R.id.classEndTime)
        val limitEt = v.findViewById<EditText>(R.id.editClassSizeLimit)

        listOf(startEt, endEt).forEach { et -> et.setOnClickListener { pickTime(et) } }

        AlertDialog.Builder(ContextThemeWrapper(this, R.style.CustomAlertDialog))
            .setView(v)
            .setTitle("Add Class")
            .setPositiveButton("Add") { _, _ ->
                val size = limitEt.text.toString().toLongOrNull()
                if (titleEt.anyEmpty(descEt, startEt, endEt) || size == null) {
                    toast("Please fill all fields"); return@setPositiveButton
                }
                addClass(
                    titleEt.text.toString(), descEt.text.toString(),
                    startEt.text.toString(), endEt.text.toString(), size
                )
            }
            .setNegativeButton("Cancel", null)
            .show()
    }

    /* ═════════════════ helpers ════════════════════ */
    private fun toggleRemoveMode() {
        removeMode = !removeMode
        btnRemove.text = if (removeMode) "Cancel" else "Remove"
        if (removeMode) toast("Select a class to remove")
    }

    private fun pickTime(target: EditText) {
        val cal = Calendar.getInstance()
        TimePickerDialog(
            this, R.style.CustomTimePickerDialog,
            { _, h, m -> target.setText(h.to12Hour(m)) },
            cal.get(Calendar.HOUR_OF_DAY), cal.get(Calendar.MINUTE), false
        ).show()
    }

    private fun addClass(t: String, d: String, start: String, end: String, limit: Long) =
        lifecycleScope.launch {
            val doc = db.collection("Classes").document()
            val payload = mapOf(
                "classID"      to doc.id,
                "vendorID"     to auth.currentUser?.uid,
                "title"        to t,
                "description"  to d,
                "startTime"    to dateStr.combine(start),
                "endTime"      to dateStr.combine(end),
                "sizeLimit"    to limit,
                "currentSeats" to 0L
            )
            runCatching { doc.set(payload).await() }
                .onSuccess { toast("Class added"); loadClasses() }
                .onFailure { toast("Add failed"); Log.e(TAG, "addClass", it) }
        }

    /* ───────────── extensions ───────────── */
    private fun String.combine(time: String): Timestamp {
        val fmt = SimpleDateFormat("MMMM dd, yyyy hh:mm a", getDefault())
        return Timestamp(fmt.parse("$this $time")!!)
    }

    private fun String.toTimestamp(hourEnd: Boolean): Timestamp {
        val fmt = SimpleDateFormat("MMMM dd, yyyy", getDefault())
        val cal = Calendar.getInstance().apply {
            time = fmt.parse(this@toTimestamp)!!
            if (hourEnd) set(23, 59, 59, 999) else set(0, 0, 0, 0)
        }
        return Timestamp(cal.time)
    }
    private fun String.toStartOfDay() = toTimestamp(false)
    private fun String.toEndOfDay()   = toTimestamp(true)

    private fun EditText.anyEmpty(vararg others: EditText) =
        text.isNullOrBlank() || others.any { it.text.isNullOrBlank() }

    private fun Int.to12Hour(min: Int): String {
        val amPm = if (this >= 12) "PM" else "AM"
        val hour = when {
            this == 0  -> 12
            this <= 12 -> this
            else       -> this - 12
        }
        return "%02d:%02d $amPm".format(hour, min)
    }

    private fun ClassInfo.detail() = """
        Title: $title
        Description: $description
        Start Time: $startTime
        End Time: $endTime
    """.trimIndent()

    private fun alert(title: String, msg: String, ok: () -> Unit) =
        AlertDialog.Builder(ContextThemeWrapper(this, R.style.CustomAlertDialog))
            .setTitle(title).setMessage(msg)
            .setPositiveButton("Yes") { _, _ -> ok() }
            .setNegativeButton("No", null).show()

    private fun toast(msg: String) =
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show()

    private fun FirebaseFirestore.classDoc(id: String) = collection("Classes").document(id)
    private fun FirebaseFirestore.userDoc(uid: String) = collection("Users").document(uid)
    private fun QuerySnapshot.toClassInfoOrNull() = runCatching {
        val start = getTimestamp("startTime")!!
        val end   = getTimestamp("endTime")!!
        ClassInfo(
            getString("classID")!!,
            getString("vendorID")!!,
            getString("title")!!,
            getString("description")!!,
            start.toFormatted(),
            end.toFormatted(),
            getLong("currentSeats") ?: 0,
            getLong("sizeLimit") ?: 0
        )
    }.getOrNull()

    private fun Timestamp.toFormatted(): String =
        SimpleDateFormat("hh:mm a", getDefault()).format(toDate())

    companion object { private const val TAG = "ViewClassActivity" }
}

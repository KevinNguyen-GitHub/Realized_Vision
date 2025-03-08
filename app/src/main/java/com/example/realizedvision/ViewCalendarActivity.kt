package com.example.realizedvision


import android.content.Intent
import android.graphics.Color
import android.os.Bundle
import android.view.Gravity
import android.view.View
import android.widget.ImageView
import android.widget.LinearLayout
import android.widget.TextView
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import androidx.core.content.ContextCompat
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FirebaseFirestore
import com.google.firebase.firestore.QuerySnapshot
import com.kizitonwose.calendar.core.CalendarDay
import com.kizitonwose.calendar.core.CalendarMonth
import com.kizitonwose.calendar.core.DayPosition
import com.kizitonwose.calendar.view.CalendarView
import com.kizitonwose.calendar.view.MonthDayBinder
import com.kizitonwose.calendar.view.MonthHeaderFooterBinder
import com.kizitonwose.calendar.view.MonthScrollListener
import java.time.DayOfWeek
import java.time.Instant
import java.time.LocalDate
import java.time.YearMonth
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.format.TextStyle
import java.util.Locale


class ViewCalendarActivity : AppCompatActivity() {

    private lateinit var calendarView : CalendarView
    private lateinit var monthText: TextView
    private val today = LocalDate.now()
    private lateinit var auth: FirebaseAuth
    private lateinit var firestore: FirebaseFirestore
    private var daysWithClasses: MutableSet<LocalDate> = mutableSetOf()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_calendar)

        firestore = FirebaseFirestore.getInstance()
        auth = FirebaseAuth.getInstance()

        calendarView = findViewById(R.id.calendarView)
        monthText = findViewById(R.id.monthText)

        val calendarIcon = findViewById<ImageView>(R.id.calendar_icon)
        val settingsIcon = findViewById<ImageView>(R.id.settings_icon)
        val profileImage = findViewById<ImageView>(R.id.profile_image)
        val profileName = findViewById<TextView>(R.id.profile_name)
        val storefrontLabel = findViewById<ImageView>(R.id.storefront_label)
        val starIcon = findViewById<ImageView>(R.id.star_icon)

        calendarIcon.setOnClickListener { view: View? -> navigateTo(ViewCalendarActivity::class.java) }
        settingsIcon.setOnClickListener { view: View? -> navigateTo(SettingsActivity::class.java) }
        storefrontLabel.setOnClickListener { view: View? -> navigateTo(StorefrontActivity::class.java) }
        starIcon.setOnClickListener { view: View? -> navigateTo(FavoritesActivity::class.java) }


        loadCalendarData()

    }

    override fun onRestart() {
        super.onRestart()
        loadCalendarData()
    }

    private fun loadCalendarData() {
        val currentUser = auth.currentUser
        if (currentUser != null) {
            val vendorId = currentUser.uid
            firestore.collection("Classes")
                .whereEqualTo("vendorID", vendorId)
                .get()
                .addOnSuccessListener { documents ->
                    processClassData(documents)
                    setupCalendar()
                }
                .addOnFailureListener { _ ->
                    Toast.makeText(this, "Error getting classes", Toast.LENGTH_SHORT).show()
                }
        }
    }

    private fun processClassData(documents: QuerySnapshot) {
        daysWithClasses.clear()
        for (document in documents) {
            val startTimeTimestamp = document.getTimestamp("startTime")
            startTimeTimestamp?.let { timestamp ->
                val localDate = Instant.ofEpochSecond(timestamp.seconds)
                    .atZone(ZoneId.systemDefault())
                    .toLocalDate()
                daysWithClasses.add(localDate)
            }
        }
    }


    private fun navigateTo(activityClass: Class<*>) {
        val intent = Intent(this, activityClass)
        startActivity(intent)
    }

    private fun setupCalendar() {

        val currentMonth = YearMonth.now()
        val firstMonth = currentMonth.minusMonths(10)
        val lastMonth = currentMonth.plusMonths(10)
        val daysOfWeek = DayOfWeek.entries
        calendarView.setup(firstMonth, lastMonth, daysOfWeek.last())
        calendarView.scrollToMonth(currentMonth)

        calendarView.dayBinder = object : MonthDayBinder<DayViewContainer> {
            override fun create(view: View) = DayViewContainer(view)

            override fun bind(container: DayViewContainer, data: CalendarDay) {
                container.textView.text = data.date.dayOfMonth.toString()

                container.textView.background = null

                if (data.position == DayPosition.MonthDate) {
                    if (data.date == today && daysWithClasses.contains(data.date)) {
                        container.textView.setTextColor(Color.WHITE)
                        container.textView.background = ContextCompat.getDrawable(
                            this@ViewCalendarActivity,
                            R.drawable.class_today
                        )
                    }
                    else if (data.date == today) {
                        container.textView.setTextColor(Color.WHITE)
                        container.textView.background = ContextCompat.getDrawable(
                            this@ViewCalendarActivity,
                            R.drawable.inset_solid_circle
                        )
                    } else if (daysWithClasses.contains(data.date)){
                        container.textView.setTextColor(ContextCompat.getColor(
                            this@ViewCalendarActivity, R.color.maroon))
                        container.textView.background = ContextCompat.getDrawable(
                            this@ViewCalendarActivity,
                            R.drawable.inset_outline_circle
                        )
                    } else {
                        container.textView.setTextColor(Color.BLACK)
                    }
                }

                container.view.setOnClickListener {
                    val intent = Intent(this@ViewCalendarActivity, ViewClassActivity::class.java)
                    val formatter = DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.getDefault())
                    intent.putExtra("selectedDate", data.date.format(formatter))
                    intent.putExtra("selectedProfileId", "HFRgKpDsOwWvwg32icAu7103f7w2")
                    startActivity(intent)
                }
            }
        }

        calendarView.monthHeaderBinder = object : MonthHeaderFooterBinder<MonthViewContainer> {
            override fun create(view: View) = MonthViewContainer(view)
            override fun bind(container: MonthViewContainer, data: CalendarMonth) {
                // Clear any existing views
                container.titlesContainer.removeAllViews()

                // Dynamically add TextViews for each day
                val daysOfWeek = data.weekDays.last().map { it.date.dayOfWeek }
                daysOfWeek.forEach { dayOfWeek ->
                    val textView = TextView(this@ViewCalendarActivity).apply {
                        text = dayOfWeek.getDisplayName(TextStyle.SHORT, Locale.getDefault())
                        layoutParams = LinearLayout.LayoutParams(
                            0,
                            LinearLayout.LayoutParams.WRAP_CONTENT,
                            1f
                        ).apply {
                            gravity = Gravity.CENTER
                        }
                        setTextColor(Color.BLACK)
                        gravity = Gravity.CENTER
                    }
                    container.titlesContainer.addView(textView)
                }
            }
        }

        calendarView.monthScrollListener = object : MonthScrollListener {
            override fun invoke(month: CalendarMonth) {
                monthText.text = month.yearMonth.month.getDisplayName(TextStyle.FULL, Locale.getDefault())
            }
        }
    }
}
package com.example.realizedvision

import android.content.Intent
import android.graphics.Color
import android.os.Bundle
import android.view.Gravity
import android.view.View
import android.widget.*
import androidx.appcompat.app.AppCompatActivity
import androidx.core.content.ContextCompat
import androidx.lifecycle.lifecycleScope
import com.google.firebase.auth.FirebaseAuth
import com.google.firebase.firestore.FirebaseFirestore
import com.google.firebase.firestore.QuerySnapshot
import com.kizitonwose.calendar.core.*
import com.kizitonwose.calendar.view.*
import kotlinx.coroutines.launch
import kotlinx.coroutines.tasks.await
import java.time.*
import java.time.format.DateTimeFormatter
import java.time.format.TextStyle
import java.util.*

class ViewCalendarActivity : AppCompatActivity() {

    /* ─────────────── views ─────────────── */
    private val calendar by lazy { findViewById<CalendarView>(R.id.calendarView) }
    private val title    by lazy { findViewById<TextView>(R.id.monthText) }
    private val btnEdit  by lazy { findViewById<Button>(R.id.btnEditAvailability) }
    private val btnBook  by lazy { findViewById<Button>(R.id.btnBookDate) }

    /* ─────────────── data ──────────────── */
    private val today           = LocalDate.now()
    private val classDays       = mutableSetOf<LocalDate>()
    private val auth            = FirebaseAuth.getInstance()
    private val db              = FirebaseFirestore.getInstance()
    private val vendorIdHardcap = "HFRgKpDsOwWvwg32icAu7103f7w2"   // TODO: replace when dynamic

    /* ══════════════════ life-cycle ═════════════════════ */
    override fun onCreate(b: Bundle?) {
        super.onCreate(b)
        setContentView(R.layout.activity_view_calendar)

        wireNav()
        configureButtons()
        refreshCalendarData()
    }

    override fun onRestart() {
        super.onRestart()
        refreshCalendarData()
    }

    /* ───────────────── navigation bar ───────────────── */
    private fun wireNav() = mapOf(
        R.id.calendar_icon   to ViewCalendarActivity::class.java,
        R.id.settings_icon   to SettingsActivity  ::class.java,
        R.id.storefront_label to StorefrontActivity::class.java,
        R.id.star_icon       to FavoritesActivity::class.java
    ).forEach { (id, cls) ->
        findViewById<ImageView>(id).setOnClickListener { start(cls) }
    }

    private fun start(cls: Class<*>) =
        startActivity(Intent(this, cls))

    /* ───────────────── vendor/user role ─────────────── */
    private fun configureButtons() {
        val uid = auth.currentUser?.uid ?: return
        db.collection("Users").document(uid).get()
            .addOnSuccessListener { snap ->
                val vendor = snap.getBoolean("isVendor") == true
                btnEdit.visibility = if (vendor) View.VISIBLE else View.GONE
                btnBook.visibility = if (vendor) View.GONE   else View.VISIBLE
            }

        btnEdit.setOnClickListener { start(EditAvailability::class.java) }
        btnBook.setOnClickListener { start(BookingAvailability::class.java) }
    }

    /* ───────────────── calendar data ────────────────── */
    private fun refreshCalendarData() = lifecycleScope.launch {
        try {
            val qs = db.collection("Classes")
                .whereEqualTo("vendorID", vendorIdHardcap)
                .get().await()
            collectClassDays(qs)
            setupCalendar()                          // (re)bind view
        } catch (e: Exception) {
            Toast.makeText(this@ViewCalendarActivity,
                "Error loading classes", Toast.LENGTH_SHORT).show()
        }
    }

    private fun collectClassDays(qs: QuerySnapshot) {
        classDays.clear()
        qs.forEach { doc ->
            doc.getTimestamp("startTime")?.toDate()?.toInstant()?.let {
                classDays += it.atZone(ZoneId.systemDefault()).toLocalDate()
            }
        }
    }

    /* ───────────────── calendar view ────────────────── */
    private fun setupCalendar() = with(calendar) {

        val rangeStart = YearMonth.now().minusMonths(10)
        val rangeEnd   = YearMonth.now().plusMonths(10)
        setup(rangeStart, rangeEnd, DayOfWeek.SATURDAY)
        scrollToMonth(YearMonth.now())

        dayBinder = object : MonthDayBinder<DayViewContainer> {
            override fun create(view: View) = DayViewContainer(view)
            override fun bind(c: DayViewContainer, day: CalendarDay) = with(c.textView) {
                text = day.date.dayOfMonth.toString()
                background = null

                if (day.position == DayPosition.MonthDate) {
                    when {
                        day.date == today && day.date in classDays -> {
                            setTextColor(Color.WHITE)
                            background = bg(R.drawable.class_today)
                        }
                        day.date == today -> {
                            setTextColor(Color.WHITE)
                            background = bg(R.drawable.inset_solid_circle)
                        }
                        day.date in classDays -> {
                            setTextColor(color(R.color.maroon))
                            background = bg(R.drawable.inset_outline_circle)
                        }
                        else -> setTextColor(Color.BLACK)
                    }
                }
                c.view.setOnClickListener { openClassList(day.date) }
            }
        }

        monthHeaderBinder = object : MonthHeaderFooterBinder<MonthViewContainer> {
            override fun create(view: View) = MonthViewContainer(view)
            override fun bind(m: MonthViewContainer, month: CalendarMonth) {
                m.titlesContainer.removeAllViews()
                month.weekDays.last().map { it.date.dayOfWeek }.forEach { dow ->
                    m.titlesContainer.addView(TextView(this@ViewCalendarActivity).apply {
                        text = dow.getDisplayName(TextStyle.SHORT, Locale.getDefault())
                        gravity = Gravity.CENTER
                        setTextColor(Color.BLACK)
                        layoutParams = LinearLayout.LayoutParams(0,
                            LinearLayout.LayoutParams.WRAP_CONTENT, 1f)
                    })
                }
            }
        }

        monthScrollListener = MonthScrollListener { mon ->
            title.text = mon.yearMonth.month.getDisplayName(TextStyle.FULL, Locale.getDefault())
        }
    }

    /* ───────────────── helpers ──────────────────────── */
    private fun openClassList(date: LocalDate) {
        val fmt = DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.getDefault())
        Intent(this, ViewClassActivity::class.java).apply {
            putExtra("selectedDate", date.format(fmt))
            putExtra("selectedProfileId", vendorIdHardcap)
        }.also(::startActivity)
    }

    private fun bg(drawable: Int) = ContextCompat.getDrawable(this, drawable)
    private fun color(id: Int)    = ContextCompat.getColor(this, id)
}

package com.example.realizedvision

import android.graphics.Color
import android.os.Bundle
import android.view.Gravity
import android.view.View
import android.widget.LinearLayout
import android.widget.TextView
import androidx.appcompat.app.AppCompatActivity
import androidx.compose.foundation.background
import androidx.core.content.ContextCompat
import androidx.core.view.children
import androidx.core.view.setPadding
import com.kizitonwose.calendar.core.CalendarDay
import com.kizitonwose.calendar.core.CalendarMonth
import com.kizitonwose.calendar.core.DayPosition
import com.kizitonwose.calendar.view.CalendarView
import com.kizitonwose.calendar.view.MonthDayBinder
import com.kizitonwose.calendar.view.MonthHeaderFooterBinder
import com.kizitonwose.calendar.view.MonthScrollListener
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.YearMonth
import java.time.format.TextStyle
import java.util.Locale


class ViewCalendarActivity : AppCompatActivity() {

    private lateinit var calendarView : CalendarView
    private lateinit var monthText: TextView
    private val today = LocalDate.now()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_calendar)

        calendarView = findViewById(R.id.calendarView)
        monthText = findViewById(R.id.monthText)

        setupCalendar()
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

                if (data.position == DayPosition.MonthDate) {
                    if (data.date == today) {
                        // Apply styling for today's date
                        container.textView.setTextColor(Color.WHITE)
                        container.textView.background = ContextCompat.getDrawable(
                            this@ViewCalendarActivity,
                            R.drawable.solid_circle
                        )
                    } else {
                        // Default styling for other dates in the month
                        container.textView.setTextColor(Color.BLACK)
                        container.textView.background = null // Remove background if it was set for today
                    }
                } else {
                    // Styling for dates outside the current month
                    container.textView.setTextColor(Color.GRAY)
                    container.textView.background = null // Remove background if it was set for today
                }

                //customization is implemented here
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
package com.example.realizedvision

import android.os.Bundle
import android.view.View
import androidx.appcompat.app.AppCompatActivity
import com.kizitonwose.calendar.core.CalendarDay
import com.kizitonwose.calendar.view.CalendarView
import com.kizitonwose.calendar.view.MonthDayBinder
import java.time.YearMonth


class ViewCalendarActivity : AppCompatActivity() {

    private lateinit var calendarView : CalendarView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_calendar)

        calendarView = findViewById(R.id.calendarView)

        setupCalendar()
    }

    private fun setupCalendar() {

        calendarView.dayBinder = object : MonthDayBinder<DayViewContainer> {
            override fun create(view: View) = DayViewContainer(view)

            override fun bind(container: DayViewContainer, data: CalendarDay) {
                container.textView.text = data.date.dayOfMonth.toString()
                //customization is implemented here
            }
        }
        val currentMonth = YearMonth.now()
        val firstMonth = currentMonth.minusMonths(10)
        val lastMonth = currentMonth.plusMonths(10)
        val daysOfWeek = emptyList<java.time.DayOfWeek>()
        calendarView.setup(firstMonth, lastMonth, daysOfWeek.first())
        calendarView.scrollToMonth(currentMonth)
    }

}
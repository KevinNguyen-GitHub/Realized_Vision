package com.example.realizedvision

import android.view.View
import android.widget.TextView
import com.kizitonwose.calendar.view.ViewContainer


class DayViewContainer(view: View) : ViewContainer(view) {
    val textView = view.findViewById<TextView>(R.id.calendarDayText)
}


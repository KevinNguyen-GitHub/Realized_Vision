package com.example.realizedvision

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.compose.ui.semantics.text
import androidx.recyclerview.widget.ListAdapter
import androidx.recyclerview.widget.RecyclerView
import java.text.SimpleDateFormat
import java.util.Locale

class ClassAdapter : ListAdapter<ClassInfo, ClassAdapter.ClassViewHolder>(ClassDiffCallback()) {

    class ClassViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val titleTextView: TextView = itemView.findViewById(R.id.classDetailTitle)
        val descriptionTextView: TextView = itemView.findViewById(R.id.classDetailDescription)
        val timeslotTextView: TextView = itemView.findViewById(R.id.classDetailTimeSlot)

        fun bind(classData: ClassInfo) {
            titleTextView.text = classData.title
            descriptionTextView.text = classData.description

            val startTimeFormatted = classData.startTime
            val endTimeFormatted = classData.endTime
            val timeSlot = "$startTimeFormatted - $endTimeFormatted"
            timeslotTextView.text = timeSlot

        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ClassViewHolder {
        val itemView = LayoutInflater.from(parent.context).inflate(R.layout.view_class, parent, false)
        return ClassViewHolder(itemView)
    }

    override fun onBindViewHolder(holder: ClassViewHolder, position: Int) {
        val classData = getItem(position)
        holder.bind(classData)
    }
}
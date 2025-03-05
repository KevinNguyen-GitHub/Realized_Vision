package com.example.realizedvision

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import java.text.DateFormat
import java.util.Locale


class ClassAdapter(private val classList: List<ClassInfo>) :
    RecyclerView.Adapter<ClassAdapter.ClassViewHolder>() {

    class ClassViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val titleTextView: TextView = itemView.findViewById(R.id.classDetailTitle)
        val descriptionTextView: TextView = itemView.findViewById(R.id.classDetailDescription)
        val timeSlotTextView: TextView = itemView.findViewById(R.id.classDetailTimeSlot)
        val reserveButton: Button = itemView.findViewById(R.id.reserveButton)
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ClassViewHolder {
        val itemView = LayoutInflater.from(parent.context)
            .inflate(R.layout.view_class, parent, false) // Inflate view_class.xml
        return ClassViewHolder(itemView)
    }

    override fun onBindViewHolder(holder: ClassViewHolder, position: Int) {
        val currentItem = classList[position]
        holder.titleTextView.text = currentItem.title
        holder.descriptionTextView.text = currentItem.description

        holder.timeSlotTextView.text = "${currentItem.startTime} - ${currentItem.endTime}"


        holder.reserveButton.setOnClickListener {
            println("Reserved: ${currentItem.title}")
        }
    }


    override fun getItemCount() = classList.size
}
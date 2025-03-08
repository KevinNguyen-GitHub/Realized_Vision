package com.example.realizedvision

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.ListAdapter
import androidx.recyclerview.widget.RecyclerView

class ClassAdapter : ListAdapter<ClassInfo, ClassAdapter.ClassViewHolder>(ClassDiffCallback()) {

    interface OnItemClickListener {
        fun onItemClick(classInfo: ClassInfo)
    }

    private var listener: OnItemClickListener? = null

    fun setOnItemClickListener(listener: OnItemClickListener) {
        this.listener = listener
    }

    class ClassViewHolder(itemView: View, private val listener: OnItemClickListener?) : RecyclerView.ViewHolder(itemView) {
        val titleTextView: TextView = itemView.findViewById(R.id.classDetailTitle)
        val descriptionTextView: TextView = itemView.findViewById(R.id.classDetailDescription)
        val timeslotTextView: TextView = itemView.findViewById(R.id.classDetailTimeSlot)
        val sizeLimitTextView: TextView = itemView.findViewById(R.id.classDetailSeatInfo)

        fun bind(classData: ClassInfo) {
            titleTextView.text = classData.title
            descriptionTextView.text = classData.description
            itemView.tag = classData

            val startTimeFormatted = classData.startTime
            val endTimeFormatted = classData.endTime
            val timeSlot = "$startTimeFormatted - $endTimeFormatted"
            timeslotTextView.text = timeSlot

            val currentSeats = classData.currentSeats
            val sizeLimit = classData.sizeLimit
            val seatInfo = "$currentSeats/$sizeLimit Seats Reserved"
            sizeLimitTextView.text = seatInfo

        }
        init {
            itemView.setOnClickListener {
                val position = getBindingAdapterPosition()
                if (position != RecyclerView.NO_POSITION) {
                    val classData = itemView.tag as ClassInfo
                    listener?.onItemClick(classData)
                }
            }
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ClassViewHolder {
        val itemView = LayoutInflater.from(parent.context).inflate(R.layout.view_class, parent, false)
        return ClassViewHolder(itemView, listener)
    }

    override fun onBindViewHolder(holder: ClassViewHolder, position: Int) {
        val classData = getItem(position)
        holder.bind(classData)
    }
}
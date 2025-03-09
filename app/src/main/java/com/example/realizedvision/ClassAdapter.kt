package com.example.realizedvision

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.ListAdapter
import androidx.recyclerview.widget.RecyclerView

class ClassAdapter(private val isViewingOtherAccount: Boolean) : ListAdapter<ClassInfo, ClassAdapter.ClassViewHolder>(ClassDiffCallback()) {

    interface OnItemClickListener {
        fun onItemClick(classInfo: ClassInfo)
        fun onReserveClick(classInfo: ClassInfo)
    }

    private var listener: OnItemClickListener? = null

    fun setOnItemClickListener(listener: OnItemClickListener) {
        this.listener = listener
    }

    class ClassViewHolder(itemView: View, private val listener: OnItemClickListener?, private val isViewingOtherAccount: Boolean) : RecyclerView.ViewHolder(itemView) {
        val titleTextView: TextView = itemView.findViewById(R.id.classDetailTitle)
        val descriptionTextView: TextView = itemView.findViewById(R.id.classDetailDescription)
        val timeslotTextView: TextView = itemView.findViewById(R.id.classDetailTimeSlot)
        val sizeLimitTextView: TextView = itemView.findViewById(R.id.classDetailSeatInfo)
        val reserveButton: TextView = itemView.findViewById(R.id.reserveButton)

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
            if (isViewingOtherAccount) {
                reserveButton.visibility = View.VISIBLE
                reserveButton.text = "Reserve"
            } else {
                reserveButton.visibility = View.VISIBLE
                reserveButton.text = "View Seats"
            }

        }
        init {
            reserveButton.setOnClickListener {
                val position = getBindingAdapterPosition()
                if (position != RecyclerView.NO_POSITION) {
                    val classData = itemView.tag as ClassInfo
                    if (isViewingOtherAccount) {
                        listener?.onReserveClick(classData)
                    } else {
                        listener?.onItemClick(classData)
                    }
                }
            }
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ClassViewHolder {
        val itemView = LayoutInflater.from(parent.context).inflate(R.layout.view_class, parent, false)
        return ClassViewHolder(itemView, listener, isViewingOtherAccount)
    }

    override fun onBindViewHolder(holder: ClassViewHolder, position: Int) {
        val classData = getItem(position)
        holder.bind(classData)
    }
}
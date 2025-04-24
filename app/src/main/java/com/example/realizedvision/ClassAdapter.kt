package com.example.realizedvision

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.ListAdapter
import androidx.recyclerview.widget.RecyclerView

class ClassAdapter(
    private val isViewingOtherAccount: Boolean,
    private val listener: OnItemClickListener
) : ListAdapter<ClassInfo, ClassAdapter.ViewHolder>(ClassDiffCallback()) {

    interface OnItemClickListener {
        fun onItemClick(classInfo: ClassInfo)
        fun onReserveClick(classInfo: ClassInfo)
    }

    /*────────────────────────── ViewHolder ──────────────────────────*/
    inner class ViewHolder(view: View) : RecyclerView.ViewHolder(view) {
        private val titleTv  = view.findViewById<TextView>(R.id.classDetailTitle)
        private val descTv   = view.findViewById<TextView>(R.id.classDetailDescription)
        private val slotTv   = view.findViewById<TextView>(R.id.classDetailTimeSlot)
        private val seatTv   = view.findViewById<TextView>(R.id.classDetailSeatInfo)
        private val actionTv = view.findViewById<TextView>(R.id.reserveButton)

        private var currentItem: ClassInfo? = null

        init {
            actionTv.setOnClickListener {
                currentItem?.let {
                    if (isViewingOtherAccount) listener.onReserveClick(it)
                    else                       listener.onItemClick(it)
                }
            }
        }

        fun bind(item: ClassInfo) {
            currentItem = item

            titleTv.text = item.title
            descTv.text  = item.description
            slotTv.text  = "${item.startTime} - ${item.endTime}"
            seatTv.text  = "${item.currentSeats}/${item.sizeLimit} Seats Reserved"

            actionTv.apply {
                visibility = View.VISIBLE
                text = if (isViewingOtherAccount) "Reserve" else "View Seats"
            }
        }
    }

    /*───────────────────────── Adapter API ──────────────────────────*/
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder =
        ViewHolder(LayoutInflater.from(parent.context)
            .inflate(R.layout.view_class, parent, false))

    override fun onBindViewHolder(holder: ViewHolder, position: Int) =
        holder.bind(getItem(position))
}

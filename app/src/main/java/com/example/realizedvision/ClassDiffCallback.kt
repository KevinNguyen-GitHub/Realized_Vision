package com.example.realizedvision

import androidx.recyclerview.widget.DiffUtil

class ClassDiffCallback : DiffUtil.ItemCallback<ClassInfo>() {
    override fun areItemsTheSame(oldItem: ClassInfo, newItem: ClassInfo): Boolean {
        return oldItem.classID == newItem.classID
    }

    override fun areContentsTheSame(oldItem: ClassInfo, newItem: ClassInfo): Boolean {
        return oldItem == newItem
    }
}
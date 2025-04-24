package com.example.realizedvision

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.BaseExpandableListAdapter
import android.widget.ImageView
import android.widget.TextView
import com.bumptech.glide.Glide

class OrderSummaryExpandableListAdapter(
    private val ctx: Context,
    private val groupTitle: String,
    private val children: List<Item>
) : BaseExpandableListAdapter() {

    private val inflater: LayoutInflater = LayoutInflater.from(ctx)

    /* ───────────────────────── group level ───────────────────────── */
    override fun getGroupCount()                  = 1
    override fun getGroup(groupPos: Int): Any     = groupTitle
    override fun getGroupId(groupPos: Int): Long  = groupPos.toLong()

    override fun getGroupView(
        groupPos: Int, isExpanded: Boolean,
        convertView: View?, parent: ViewGroup
    ): View = (convertView ?: inflater.inflate(
        android.R.layout.simple_expandable_list_item_1, parent, false)
            ).apply {
            findViewById<TextView>(android.R.id.text1).text = groupTitle
        }

    /* ───────────────────────── child level ───────────────────────── */
    override fun getChildrenCount(groupPos: Int)            = children.size
    override fun getChild(groupPos: Int, childPos: Int)      = children[childPos]
    override fun getChildId(groupPos: Int, childPos: Int)    =
        (children[childPos].hashCode() + childPos).toLong()

    override fun getChildView(
        groupPos: Int, childPos: Int, isLast: Boolean,
        convertView: View?, parent: ViewGroup
    ): View {
        val holder: ChildVH
        val v: View

        if (convertView == null) {
            v = inflater.inflate(R.layout.item_checkout, parent, false)
            holder = ChildVH(v)
            v.tag = holder
        } else {
            v = convertView
            holder = convertView.tag as ChildVH
        }

        val item = children[childPos]
        holder.name.text  = item.name
        holder.price.text = String.format("$%.2f", item.price)
        holder.qty.text   = "Qty: ${item.quantity}"

        Glide.with(ctx)
            .load(item.imageUrl)
            .placeholder(R.drawable.ic_placeholder_image)
            .into(holder.img)

        return v
    }

    override fun hasStableIds()                       = true
    override fun isChildSelectable(g: Int, c: Int)    = false

    /* ───────────────────────── view-holder ───────────────────────── */
    private class ChildVH(v: View) {
        val img   : ImageView = v.findViewById(R.id.item_image)
        val name  : TextView  = v.findViewById(R.id.item_name)
        val price : TextView  = v.findViewById(R.id.item_price)
        val qty   : TextView  = v.findViewById(R.id.item_quantity)
    }
}

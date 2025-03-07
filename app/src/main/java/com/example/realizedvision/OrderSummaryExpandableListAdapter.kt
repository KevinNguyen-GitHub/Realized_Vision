package com.example.realizedvision

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.BaseExpandableListAdapter
import android.widget.ImageView
import android.widget.TextView
import androidx.compose.ui.semantics.text
import com.bumptech.glide.Glide

class OrderSummaryExpandableListAdapter(
    private val context: Context,
    private val groupTitle: String,
    private val childItems: List<Item>
) : BaseExpandableListAdapter() {

    override fun getGroupCount(): Int {
        return 1 // Only one group: "Order Summary"
    }

    override fun getChildrenCount(groupPosition: Int): Int {
        return childItems.size
    }

    override fun getGroup(groupPosition: Int): Any {
        return groupTitle
    }

    override fun getChild(groupPosition: Int, childPosition: Int): Any {
        return childItems[childPosition]
    }

    override fun getGroupId(groupPosition: Int): Long {
        return groupPosition.toLong()
    }

    override fun getChildId(groupPosition: Int, childPosition: Int): Long {
        return childPosition.toLong()
    }

    override fun hasStableIds(): Boolean {
        return false
    }

    override fun getGroupView(
        groupPosition: Int,
        isExpanded: Boolean,
        convertView: View?,
        parent: ViewGroup?
    ): View {
        var view = convertView
        if (view == null) {
            val inflater = context.getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater
            view = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null)
        }
        val titleTextView = view?.findViewById<TextView>(android.R.id.text1)
        titleTextView?.text = getGroup(groupPosition) as String
        return view!!
    }

    override fun getChildView(
        groupPosition: Int,
        childPosition: Int,
        isLastChild: Boolean,
        convertView: View?,
        parent: ViewGroup?
    ): View {
        var view = convertView
        if (view == null) {
            val inflater = context.getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater
            view = inflater.inflate(R.layout.item_checkout, null)
        }

        val item = getChild(groupPosition, childPosition) as Item


        val itemNameTextView = view?.findViewById<TextView>(R.id.item_name)
        val itemPriceTextView = view?.findViewById<TextView>(R.id.item_price)
        val itemDescriptionTextView = view?.findViewById<TextView>(R.id.item_description)
        val itemImageView = view?.findViewById<ImageView>(R.id.item_image)


        itemNameTextView?.text = item.getName()
        itemPriceTextView?.text = String.format("$%.2f", item.getPrice())
        itemDescriptionTextView?.text = item.getDescription()


        Glide.with(context)
            .load(item.getImageUrl())
            .placeholder(R.drawable.ic_placeholder_image)
            .into(itemImageView!!)

        return view!!
    }

    override fun isChildSelectable(groupPosition: Int, childPosition: Int): Boolean {
        return false // You can change this if you want child items to be clickable
    }
}
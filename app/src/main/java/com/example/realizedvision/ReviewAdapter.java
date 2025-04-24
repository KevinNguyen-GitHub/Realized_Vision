package com.example.realizedvision;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;
import java.util.Map;

/**
 * Lightweight adapter that renders item-review docs coming straight from
 * Firestore (<code>Map&lt;String,Object&gt;</code>).
 *
 * Expected keys in each map:
 * <ul>
 *   <li><b>displayName</b> – String</li>
 *   <li><b>rating</b>      – Number (Double/Long)</li>
 *   <li><b>text</b>        – String (optional)</li>
 * </ul>
 */
public class ReviewAdapter extends RecyclerView.Adapter<ReviewAdapter.VH> {

    private final Context              ctx;
    private final LayoutInflater       inflater;
    private final List<Map<String, Object>> data;

    public ReviewAdapter(Context c, List<Map<String, Object>> reviews) {
        this.ctx      = c;
        this.data     = reviews;
        this.inflater = LayoutInflater.from(c);
        setHasStableIds(true);                   // smoother RecyclerView animations
    }

    /* ───────────────────────── ViewHolder ───────────────────────── */
    static final class VH extends RecyclerView.ViewHolder {
        final TextView name, rating, desc;
        VH(View v) {
            super(v);
            name   = v.findViewById(R.id.reviewer_name);
            rating = v.findViewById(R.id.review_rating);
            desc   = v.findViewById(R.id.review_description);
        }
    }

    /* ───────────────────── RecyclerView hooks ──────────────────── */
    @NonNull @Override
    public VH onCreateViewHolder(@NonNull ViewGroup p, int t) {
        return new VH(inflater.inflate(R.layout.item_review, p, false));
    }

    @Override
    public void onBindViewHolder(@NonNull VH h, int pos) {
        Map<String, Object> r = data.get(pos);

        String displayName = String.valueOf(r.get("displayName"));
        Number rawRating   = (Number) r.get("rating");
        String text        = String.valueOf(r.get("text"));

        h.name  .setText(ctx.getString(R.string.review_by_fmt, displayName));
        h.rating.setText(ctx.getString(R.string.rating_fmt,
                rawRating != null ? rawRating.floatValue() : 0f));
        h.desc  .setText(text == null || "null".equals(text) ? "" : text);
    }

    @Override public int  getItemCount()            { return data.size(); }
    @Override public long getItemId(int position)   { return data.get(position).hashCode(); }
}

package com.example.realizedvision;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * Immutable representation of an inventory item / menu item.
 *
 * <p>Use {@link Builder} to create instances so you never end up with a half-filled object.
 * Fields that are not guaranteed by business logic (e.g. <i>favorite</i>, <i>preferred</i>)
 * are treated as transient UI metadata and kept mutable via dedicated setters.</p>
 */
public final class Item {

    /* ────────────────────────────── core data ───────────────────────────── */
    @NonNull private final String  id;
    @NonNull private final String  vendorId;
    @NonNull private final String  name;
    @NonNull private final String  description;
    @NonNull private final String  category;
    @NonNull private final BigDecimal price;     // avoids double rounding issues
    @Nullable private final String  imageUrl;
    private       long             quantity;

    /* ────────────────────────────── UI flags ────────────────────────────── */
    private boolean favorite;
    private boolean preferred;

    /* ─────────────────────────── constructor  ───────────────────────────── */
    private Item(Builder b) {
        this.id          = b.id;
        this.vendorId    = b.vendorId;
        this.name        = b.name;
        this.description = b.description;
        this.category    = b.category;
        this.price       = b.price;
        this.imageUrl    = b.imageUrl;
        this.quantity    = b.quantity;
        this.favorite    = b.favorite;
        this.preferred   = b.preferred;
    }

    /* ───────────────────────────── getters ──────────────────────────────── */
    @NonNull public String      getId()          { return id; }
    @NonNull public String      getVendorId()    { return vendorId; }
    @NonNull public String      getName()        { return name; }
    @NonNull public String      getDescription() { return description; }
    @NonNull public String      getCategory()    { return category; }
    @NonNull public BigDecimal  getPrice()       { return price; }
    @Nullable public String     getImageUrl()    { return imageUrl; }
    public long                 getQuantity()    { return quantity; }

    /* ─────────────── UI-only mutators (safe to expose) ──────────────────── */
    public boolean isFavorite()  { return favorite; }
    public boolean isPreferred() { return preferred; }

    public void setFavorite(boolean favorite)   { this.favorite = favorite; }
    public void setPreferred(boolean preferred) { this.preferred = preferred; }
    public void setQuantity(long quantity)      { this.quantity  = quantity; }

    /* ─────────────────── equals / hashCode / toString ───────────────────── */
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Item)) return false;
        Item item = (Item) o;
        return id.equals(item.id);
    }

    @Override public int hashCode() { return id.hashCode(); }

    @Override public String toString() {
        return "Item{" + id + " – " + name + ", $" + price + '}';
    }

    /* ─────────────────────────── Builder API ────────────────────────────── */
    public static final class Builder {
        private String      id;
        private String      vendorId;
        private String      name;
        private String      description = "";
        private String      category    = "";
        private BigDecimal  price       = BigDecimal.ZERO;
        private String      imageUrl;
        private long        quantity    = 1;
        private boolean     favorite;
        private boolean     preferred;

        public Builder(@NonNull String id, @NonNull String vendorId) {
            this.id       = Objects.requireNonNull(id, "id == null");
            this.vendorId = Objects.requireNonNull(vendorId, "vendorId == null");
        }

        public Builder name(@NonNull String n)          { name = n; return this; }
        public Builder description(@NonNull String d)   { description = d; return this; }
        public Builder category(@NonNull String c)      { category = c; return this; }
        public Builder price(double p)                  { price = BigDecimal.valueOf(p); return this; }
        public Builder imageUrl(@Nullable String url)   { imageUrl = url; return this; }
        public Builder quantity(long q)                 { quantity = q; return this; }
        public Builder favorite(boolean f)              { favorite = f; return this; }
        public Builder preferred(boolean p)             { preferred = p; return this; }

        public Item build() {
            if (name == null)        throw new IllegalStateException("name missing");
            if (category == null)    throw new IllegalStateException("category missing");
            return new Item(this);
        }
    }
}

package com.example.realizedvision;

import androidx.annotation.NonNull;
import java.util.Objects;

/**
 * Immutable snapshot of a vendor document.
 *
 * Firestore uses the public no-arg constructor + setters for deserialization.
 * Your app code should prefer the all-args constructor and *getters only*,
 * keeping Vendor instances effectively read-only.
 */
public class Vendor {

    /* ───────────────────────── fields ───────────────────────── */
    private String id;
    private String companyName;
    private String address;
    private int    yearsInBusiness;

    /* ───────────────────── firestore needs ───────────────────── */
    @SuppressWarnings("unused")         // used by Firestore
    public Vendor() { }

    /* ───────────────────── public factory ───────────────────── */
    public Vendor(String id,
                  String companyName,
                  String address,
                  int yearsInBusiness) {
        this.id               = id;
        this.companyName      = companyName;
        this.address          = address;
        this.yearsInBusiness  = yearsInBusiness;
    }

    /* ─────────────────── getters (read-only) ─────────────────── */
    public String getId()              { return id; }
    public String getCompanyName()     { return companyName; }
    public String getAddress()         { return address; }
    public int    getYearsInBusiness() { return yearsInBusiness; }

    /* ───── setters kept package-private for Firestore only ───── */
    void setCompanyName(String n)     { this.companyName = n; }
    void setAddress(String a)         { this.address = a; }
    void setYearsInBusiness(int y)    { this.yearsInBusiness = y; }

    /* ───────────────────── utility overrides ─────────────────── */
    @NonNull @Override
    public String toString() {
        return "Vendor{id=" + id + ", companyName=" + companyName + '}';
    }

    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Vendor)) return false;
        Vendor v = (Vendor) o;
        return Objects.equals(id, v.id);
    }

    @Override public int hashCode() { return Objects.hash(id); }
}

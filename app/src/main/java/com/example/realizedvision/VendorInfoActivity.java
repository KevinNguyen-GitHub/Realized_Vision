package com.example.realizedvision;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.*;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.firestore.*;

import java.util.HashMap;
import java.util.Map;

/**
 * One-time “upgrade to vendor” screen.
 *
 * – Collects company name, years in business and a postal address.
 * – Writes a vendor profile under <pre>Vendors/{uid}</pre> and flips
 *   <code>Users/{uid}.isVendor = true</code>.
 * – If the user is already a vendor the form is hidden and a message is shown.
 */
public class VendorInfoActivity extends AppCompatActivity {

    /* ---------------- Firebase ---------------- */
    private final FirebaseAuth     auth = FirebaseAuth.getInstance();
    private final FirebaseFirestore db   = FirebaseFirestore.getInstance();

    /* ----------------  UI -------------------- */
    private EditText etCompany, etYears;
    private EditText etStreet,  etCity, etState, etZip;
    private Button   btnSubmit;
    private TextView tvStatus;

    /* ---------------- life-cycle -------------- */
    @Override protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_vendorinfo);

        if (auth.getCurrentUser() == null) { finish(); return; }

        bindViews();
        loadUserState();
    }

    /* ------------ bind + click listeners ------ */
    private void bindViews() {
        etCompany = findViewById(R.id.companyName);
        etYears   = findViewById(R.id.yearsInBusiness);
        etStreet  = findViewById(R.id.streetAddressEditText);
        etCity    = findViewById(R.id.cityEditText);
        etState   = findViewById(R.id.stateEditText);
        etZip     = findViewById(R.id.zipCodeEditText);

        btnSubmit = findViewById(R.id.submitButton);
        tvStatus  = findViewById(R.id.vendorStatus);

        findViewById(R.id.backButtonVendor).setOnClickListener(v -> finish());
        btnSubmit.setOnClickListener(v -> saveVendorInfo());
    }

    /* -------------- save to Firestore --------- */
    private void saveVendorInfo() {

        String company = etCompany.getText().toString().trim();
        String street  = etStreet .getText().toString().trim();
        String city    = etCity   .getText().toString().trim();
        String state   = etState  .getText().toString().trim();
        String zip     = etZip    .getText().toString().trim();
        String yearsTx = etYears  .getText().toString().trim();

        if (company.isEmpty() || street.isEmpty() || city.isEmpty()
                || state.isEmpty() || zip.isEmpty() || yearsTx.isEmpty()) {
            toast("Please fill in every field"); return;
        }
        if (!TextUtils.isDigitsOnly(yearsTx)) {
            toast("Years in business must be a number"); return;
        }
        int years = Integer.parseInt(yearsTx);

        String uid = auth.getUid();
        Map<String,Object> vendor = new HashMap<>();
        vendor.put("companyName",     company);
        vendor.put("address",         String.format("%s %s %s %s", street, city, state, zip));
        vendor.put("yearsInBusiness", years);
        vendor.put("vendorID",        uid);

        WriteBatch batch = db.batch();
        batch.update(db.collection("Users").document(uid), "isVendor", true);
        batch.set   (db.collection("Vendors").document(uid), vendor, SetOptions.merge());

        btnSubmit.setEnabled(false);

        batch.commit()
                .addOnSuccessListener(x -> {
                    toast("You are now a vendor!");
                    finish();                            // return to previous screen
                })
                .addOnFailureListener(e -> {
                    btnSubmit.setEnabled(true);
                    toast("Failed to save: " + e.getMessage());
                });
    }

    /* -------------- show/hide if already vendor -------------- */
    private void loadUserState() {
        db.collection("Users").document(auth.getUid()).get()
                .addOnSuccessListener(doc -> {
                    boolean vendor = Boolean.TRUE.equals(doc.getBoolean("isVendor"));
                    setFormVisible(!vendor);
                })
                .addOnFailureListener(e -> toast("Error loading user: " + e.getMessage()));
    }

    private void setFormVisible(boolean show) {
        int f = show ? View.VISIBLE : View.GONE;
        for (int id : new int[]{
                R.id.companyName, R.id.yearsInBusiness,
                R.id.streetAddressEditText, R.id.cityEditText,
                R.id.stateEditText, R.id.zipCodeEditText, R.id.submitButton }) {
            findViewById(id).setVisibility(f);
        }
        tvStatus.setVisibility(show ? View.GONE : View.VISIBLE);
    }

    /* ---------------- util -------------------- */
    private void toast(String m){
        Toast.makeText(this,m,Toast.LENGTH_SHORT).show();
    }
}

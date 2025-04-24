package com.example.realizedvision;

import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;

/**
 * Simple static screen that shows the app’s Terms & Conditions.
 *
 * UX: tapping the back arrow (or system back) just closes this
 * activity and returns to the previous screen.
 */
public class TermsAndConditionsActivity extends AppCompatActivity {

    @Override protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_tandc);

        findViewById(R.id.backButtonTermsAndConditions)
                .setOnClickListener(v -> finish());
    }
}

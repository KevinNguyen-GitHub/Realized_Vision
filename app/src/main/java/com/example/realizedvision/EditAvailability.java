package com.example.realizedvision;

import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;
import com.kizitonwose.calendar.core.CalendarDay;
import com.kizitonwose.calendar.view.CalendarView;
import com.kizitonwose.calendar.view.MonthDayBinder;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class EditAvailability extends AppCompatActivity {

    private CalendarView calendarView;
    private Button btnSave;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private Set<LocalDate> selectedDates = new HashSet<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_editcalendar);

        calendarView = findViewById(R.id.editCalendarView);
        btnSave = findViewById(R.id.btnSaveAvailability);
        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        setupCalendar();

        btnSave.setOnClickListener(v -> saveAvailabilityToDatabase());
    }

    private void setupCalendar() {
        YearMonth currentMonth = YearMonth.now();
        YearMonth endMonth = currentMonth.plusMonths(12); // Show 12 months from now

        calendarView.setup(currentMonth, endMonth, DayOfWeek.SUNDAY);

        calendarView.setDayBinder(new MonthDayBinder<DayViewContainer>() {
            @Override
            public DayViewContainer create(View view) {
                return new DayViewContainer(view);
            }

            @Override
            public void bind(DayViewContainer container, CalendarDay day) {
                container.getTextView().setText(String.valueOf(day.getDate().getDayOfMonth()));

                // Highlight selected dates
                if (selectedDates.contains(day.getDate())) {
                    container.getTextView().setBackgroundResource(R.drawable.solid_circle);
                } else {
                    container.getTextView().setBackground(null);
                }

                // OnClick: Toggle selection
                container.getTextView().setOnClickListener(v -> {
                    if (selectedDates.contains(day.getDate())) {
                        selectedDates.remove(day.getDate());
                        container.getTextView().setBackground(null);
                    } else {
                        selectedDates.add(day.getDate());
                        container.getTextView().setBackgroundResource(R.drawable.solid_circle);
                    }
                });
            }
        });

        loadAvailabilityFromDatabase();
    }

    private void saveAvailabilityToDatabase() {
        List<String> formattedDates = new ArrayList<>();
        for (LocalDate date : selectedDates) {
            formattedDates.add(date.toString()); // Convert to String
        }

        firestore.collection("vendors").document(String.valueOf(currentUser))
                .update("availableDates", formattedDates)
                .addOnSuccessListener(aVoid -> Toast.makeText(this, "Availability Saved!", Toast.LENGTH_SHORT).show())
                .addOnFailureListener(e -> Log.e("Calendar", "Error saving dates"));
    }

    private void loadAvailabilityFromDatabase() {
        firestore.collection("vendors").document(String.valueOf(currentUser))
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        List<String> availableDates = (List<String>) documentSnapshot.get("availableDates");
                        if (availableDates != null) {
                            for (String dateStr : availableDates) {
                                selectedDates.add(LocalDate.parse(dateStr));
                            }
                            calendarView.notifyCalendarChanged(); // Refresh UI
                        }
                    }
                })
                .addOnFailureListener(e -> Log.e("Calendar", "Error loading dates", e));
    }
}

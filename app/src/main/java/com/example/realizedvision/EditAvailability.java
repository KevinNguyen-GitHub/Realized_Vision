package com.example.realizedvision;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.SetOptions;
import com.kizitonwose.calendar.core.CalendarDay;
import com.kizitonwose.calendar.view.CalendarView;
import com.kizitonwose.calendar.view.MonthDayBinder;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.Year;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import kotlin.Unit;


public class EditAvailability extends AppCompatActivity {

    private CalendarView calendarView;
    private Button btnSave;
    private TextView monthTextView;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private DateTimeFormatter monthFormat = DateTimeFormatter.ofPattern("MMMM yyyy", Locale.getDefault());
    private Set<LocalDate> selectedDates = new HashSet<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_editcalendar);


        ImageButton backButton = findViewById(R.id.backButtonChangePass);

        calendarView = findViewById(R.id.editCalendarView);
        monthTextView = findViewById(R.id.monthTextView);
        btnSave = findViewById(R.id.btnSaveAvailability);

        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }

        setupCalendar();

        backButton.setOnClickListener(view -> navigateTo(ViewCalendarActivity.class));

        btnSave.setOnClickListener(v -> saveAvailabilities());
    }

    private void setupCalendar() {
        YearMonth currentMonth = YearMonth.now();
        YearMonth endMonth = currentMonth.plusMonths(12); // Show 12 months from now

        calendarView.setup(currentMonth, endMonth, DayOfWeek.SUNDAY);

        monthTextView.setText(monthFormat.format(currentMonth));

        // Scroll through Calendar
        calendarView.setMonthScrollListener(calendarMonth -> {
            monthTextView.setText(monthFormat.format(calendarMonth.getYearMonth()));
            return Unit.INSTANCE;
        });

        calendarView.setDayBinder(new MonthDayBinder<DayViewContainer>() {
            @Override
            public DayViewContainer create(View view) {
                return new DayViewContainer(view);
            }

            @Override
            public void bind(DayViewContainer container, CalendarDay day) {
                container.getTextView().setText(String.valueOf(day.getDate().getDayOfMonth()));

                // Sets color of date depending on status
                if (selectedDates.contains(day.getDate())) {
                    container.getTextView().setBackgroundResource(R.drawable.solid_circle);
                } else {
                    container.getTextView().setBackground(null);
                }

                // Select or unselect a date on calendar UI
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

        loadAvailabilities();
    }

    private void saveAvailabilities() {
        if (currentUser == null) {
            Toast.makeText(this, "User not logged in", Toast.LENGTH_SHORT).show();
            return;
        }
        // Creates array and converts selected dates on UI to string
        List<String> formattedDates = new ArrayList<>();
        for (LocalDate date : selectedDates) {
            formattedDates.add(date.toString());
        }
        String userID = currentUser.getUid();

        // Saves array to DB
        firestore.collection("Vendors").document(userID)
                .set(Collections.singletonMap("Availabilities", formattedDates), SetOptions.merge())
                .addOnSuccessListener(aVoid -> Toast.makeText(this, "Availability Saved!", Toast.LENGTH_SHORT).show())
                .addOnFailureListener(e -> Log.e("Calendar", "Error saving dates"));
    }

    private void loadAvailabilities() {
        if (currentUser == null) {
            Toast.makeText(this, "User not logged in", Toast.LENGTH_SHORT).show();
            return;
        }
        String userID = currentUser.getUid();

        // Queries DB for availabilties as listed by vendor
        firestore.collection("Vendors").document(userID)
                .get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        // Gets stored array from DB
                        List<String> availableDates = (List<String>) documentSnapshot.get("Availabilities");
                        if (availableDates != null) {
                            for (String dateStr : availableDates) {
                                selectedDates.add(LocalDate.parse(dateStr));
                            }
                            // Updates Calendar
                            calendarView.notifyCalendarChanged();
                        }
                    }
                })
                .addOnFailureListener(e -> Log.e("Calendar", "Error loading dates", e));
    }
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(EditAvailability.this, targetActivity);
        startActivity(intent);
    }
}

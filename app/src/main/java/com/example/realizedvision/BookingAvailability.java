package com.example.realizedvision;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.kizitonwose.calendar.core.CalendarDay;
import com.kizitonwose.calendar.view.CalendarView;
import com.kizitonwose.calendar.view.MonthDayBinder;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import kotlin.Unit;

public class BookingAvailability extends AppCompatActivity {
    private CalendarView calendarView;
    private Button btnSave;
    private TextView monthTextView;
    private FirebaseFirestore firestore;
    private FirebaseUser currentUser;
    private String vendorID;
    private DateTimeFormatter monthFormat = DateTimeFormatter.ofPattern("MMMM yyyy", Locale.getDefault());
    private Set<LocalDate> selectedDates = new HashSet<>();
    private Set<LocalDate> bookedDates = new HashSet<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_editcalendar);


        ImageButton backButton = findViewById(R.id.backButtonChangePass);

        calendarView = findViewById(R.id.editCalendarView);
        monthTextView = findViewById(R.id.monthTextView);
        btnSave = findViewById(R.id.btnSaveAvailability);

        btnSave.setVisibility(View.INVISIBLE);

        // Initialize Firebase
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if (currentUser != null) {
            firestore = FirebaseFirestore.getInstance();
        }
        // Placeholder for vendorID for Demo purposes
        vendorID = "wP1b2zpnIcasqu9yE9lB4ymTxY63";
        if (vendorID == null) {
            Toast.makeText(this, "Error: Vendor ID missing!", Toast.LENGTH_SHORT).show();
            finish();
            return;
        }

        setupCalendar();

        backButton.setOnClickListener(view -> navigateTo(ViewCalendarActivity.class));
    }
    private void setupCalendar() {
        // Date setup
        YearMonth currentMonth = YearMonth.now();
        YearMonth endMonth = currentMonth.plusMonths(12);

        calendarView.setup(currentMonth, endMonth, DayOfWeek.SUNDAY);

        monthTextView.setText(monthFormat.format(currentMonth));

        // Load existing bookings from DB
        loadAvailabilities();
        loadBookedDates();

        // Allow users to scroll through months
        calendarView.setMonthScrollListener(calendarMonth -> {
            monthTextView.setText(monthFormat.format(calendarMonth.getYearMonth()));
            return Unit.INSTANCE;
        });

        // Set up for individual dates on calendar
        calendarView.setDayBinder(new MonthDayBinder<DayViewContainer>() {
            @Override
            public DayViewContainer create(View view) {
                return new DayViewContainer(view);
            }

            @Override
            public void bind(DayViewContainer container, CalendarDay day) {
                container.getTextView().setText(String.valueOf(day.getDate().getDayOfMonth()));

                // Booleans to check status of dates
                boolean isAvailable = selectedDates.contains(day.getDate());
                boolean isBooked = bookedDates.contains(day.getDate());

                // Sets color of date depending on status
                if (isBooked) {
                    container.getTextView().setBackgroundResource(R.drawable.grey_circle);
                } else if (isAvailable) {
                    container.getTextView().setBackgroundResource(R.drawable.solid_circle);
                } else {
                    container.getTextView().setBackground(null);
                }

                // Users can book an availability
                container.getTextView().setOnClickListener(v -> {
                    if (isAvailable && !isBooked) {
                        showConfirmDialog(day.getDate());
                    } else if (isBooked) {
                        showCancelDialog(day.getDate());
                    }
                });
            }
        });
    }
    private void loadAvailabilities() {
        // Queries DB for availabilties as listed by vendor
        firestore.collection("Vendors").document(vendorID)
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
    private void loadBookedDates() {
        // Queries DB for booked dates
        firestore.collection("Vendors").document(vendorID).collection("Bookings")
                .get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    for (DocumentSnapshot document : queryDocumentSnapshots.getDocuments()) {
                        String dateStr = document.getString("date");
                        if (dateStr != null) {
                            bookedDates.add(LocalDate.parse(dateStr));
                        }
                    }
                    // Update calendar
                    calendarView.notifyCalendarChanged();
                })
                .addOnFailureListener(e -> Log.e("Calendar", "Error loading booked dates", e));
    }
    private void confirmBooking(LocalDate date) {
        if (currentUser == null) return;
        String userID = currentUser.getUid();

        // Checks if date is available
        if (!selectedDates.contains(date) || bookedDates.contains(date)) {
            Toast.makeText(this, "This date is not available!", Toast.LENGTH_SHORT).show();
            return;
        }

        // Stores booking info into DB
        Map<String, Object> bookingData = new HashMap<>();
        bookingData.put("date", date.toString());
        bookingData.put("userID", userID);
        bookingData.put("status", "confirmed");

        firestore.collection("Vendors").document(vendorID).collection("Bookings")
                .add(bookingData)
                .addOnSuccessListener(documentReference -> {
                    // Changes date availability status
                    bookedDates.add(date);

                    // Updates calendar
                    calendarView.notifyCalendarChanged();
                    Toast.makeText(this, "Booking Confirmed!", Toast.LENGTH_SHORT).show();
                })
                .addOnFailureListener(e -> Log.e("Calendar", "Error saving booking", e));
    }
    private void cancelBooking(LocalDate date) {
        if (currentUser == null) return;
        String userID = currentUser.getUid();

        // Query to find the booking document
        firestore.collection("Vendors").document(vendorID).collection("Bookings")
                .whereEqualTo("date", date.toString())
                .whereEqualTo("userID", userID)
                .get()
                .addOnSuccessListener(queryDocumentSnapshots -> {
                    for (DocumentSnapshot document : queryDocumentSnapshots.getDocuments()) {
                        // Deletes DB reference and updates booked dates array and calendar accordingly
                        document.getReference().delete()
                                .addOnSuccessListener(aVoid -> {
                                    bookedDates.remove(date);
                                    calendarView.notifyCalendarChanged();
                                    Toast.makeText(this, "Booking Canceled!", Toast.LENGTH_SHORT).show();
                                })
                                .addOnFailureListener(e -> Log.e("Calendar", "Error canceling booking", e));
                    }
                })
                .addOnFailureListener(e -> Log.e("Calendar", "Error finding booking", e));
    }
    private void showCancelDialog(LocalDate date) {
        // Confirmation Popup
        AlertDialog dialog = new AlertDialog.Builder(this)
                .setTitle("Cancel Booking")
                .setMessage("Are you sure you want to cancel your booking on " + date.toString() + "?")
                .setPositiveButton("Yes", (dialogInterface, which) -> cancelBooking(date))
                .setNegativeButton("No", null)
                .create();

        // Change button colors when the dialog is shown
        dialog.setOnShowListener(dialogInterface -> {
            Button yesButton = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
            Button noButton = dialog.getButton(AlertDialog.BUTTON_NEGATIVE);

            yesButton.setTextColor(Color.RED);
            noButton.setTextColor(Color.GREEN);
        });

        dialog.show();
    }
    private void showConfirmDialog(LocalDate date) {
        AlertDialog dialog = new AlertDialog.Builder(this)
                .setTitle("Complete Booking")
                .setMessage("Do you want to confirm your booking for " + date.toString() + "?")
                .setPositiveButton("Yes", (dialogInterface, which) -> confirmBooking(date))
                .setNegativeButton("No", null)
                .create();

        dialog.setOnShowListener(dialogInterface -> {
            Button yesButton = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
            Button noButton = dialog.getButton(AlertDialog.BUTTON_NEGATIVE);

            yesButton.setTextColor(Color.RED);
            noButton.setTextColor(Color.GREEN);
        });

        dialog.show();
    }
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(BookingAvailability.this, targetActivity);
        startActivity(intent);
    }

}

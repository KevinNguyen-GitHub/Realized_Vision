package com.example.realizedvision

import android.content.Intent
import android.graphics.Color
import android.os.Bundle
import android.view.Gravity
import android.view.View
import android.widget.LinearLayout
import android.widget.TextView
import androidx.activity.result.ActivityResultLauncher
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import androidx.core.content.ContextCompat
import androidx.credentials.CredentialManager
import androidx.credentials.GetCredentialRequest
import androidx.credentials.GetCredentialResponse
import androidx.credentials.exceptions.GetCredentialCancellationException
import androidx.credentials.exceptions.GetCredentialException
import androidx.credentials.exceptions.NoCredentialException
import com.google.android.libraries.identity.googleid.GoogleIdTokenCredential
import com.google.android.libraries.identity.googleid.GoogleIdTokenCredential.Companion.TYPE_GOOGLE_ID_TOKEN_CREDENTIAL
import com.google.android.libraries.identity.googleid.GoogleIdTokenCredential.Companion.createFrom

import com.google.android.libraries.identity.googleid.GoogleIdTokenCredential.Builder
import com.kizitonwose.calendar.core.CalendarDay
import com.kizitonwose.calendar.core.CalendarMonth
import com.kizitonwose.calendar.core.DayPosition
import com.kizitonwose.calendar.view.CalendarView
import com.kizitonwose.calendar.view.MonthDayBinder
import com.kizitonwose.calendar.view.MonthHeaderFooterBinder
import com.kizitonwose.calendar.view.MonthScrollListener
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.YearMonth
import java.time.format.DateTimeFormatter
import java.time.format.TextStyle
import java.util.Locale


class ViewCalendarActivity : AppCompatActivity() {

    private lateinit var calendarView : CalendarView
    private lateinit var monthText: TextView
    private val today = LocalDate.now()
//    private val CALENDAR_SCOPE = "https://www.googleapis.com/auth/calendar"
//    private lateinit var credentialManager: CredentialManager
//    private lateinit var getCredentialLauncher: ActivityResultLauncher<GetCredentialRequest>
//    private val CLIENT_ID = "13504432131-fsudlcn1sb9lk09s79gshglh7ks7lgnd.apps.googleusercontent.com"

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_view_calendar)

        calendarView = findViewById(R.id.calendarView)
        monthText = findViewById(R.id.monthText)

        setupCalendar()
//        credentialManager = CredentialManager.create(this)
//
//        getCredentialLauncher = registerForActivityResult(
//            ActivityResultContracts.GetCredential()
//        ) { result: GetCredentialResponse ->
//            onSignInResult(result)
//        }
//
//        signIn()
    }

//    private fun signIn() {
//        val googleIdTokenRequestOptions = GoogleIdTokenRequestOptions.Builder()
//            .setSupported(true)
//            .setServerClientId(CLIENT_ID)
//            .setNonce("YOUR_NONCE")
//            .build()
//
//        val getCredentialRequest = GetCredentialRequest.Builder()
//            .addCredentialOption(googleIdTokenRequestOptions)
//            .build()
//
//        getCredentialLauncher.launch(getCredentialRequest)
//    }
//
//    private fun onSignInResult(result: GetCredentialResponse) {
//        try {
//            val credential = result.credential
//            if (credential.type == TYPE_GOOGLE_ID_TOKEN_CREDENTIAL) {
//                val googleIdTokenCredential = createFrom(credential)
//                val idToken = googleIdTokenCredential.idToken
//                val displayName = googleIdTokenCredential.displayName
//                val email = googleIdTokenCredential.id
//                Log.d("SignIn", "User signed in: $displayName, $email, $idToken")
//                setupCalendar()
//            } else {
//                Log.e("SignIn", "Credential type not supported: ${credential.type}")
//            }
//        } catch (e: GetCredentialException) {
//            Log.e("SignIn", "Sign-in failed: ${e.message}")
//        } catch (e: NoCredentialException) {
//            Log.e("SignIn", "No credentials found: ${e.message}")
//        } catch (e: GetCredentialCancellationException) {
//            Log.e("SignIn", "Sign-in cancelled by user: ${e.message}")
//        }
//    }


    private fun setupCalendar() {

        val currentMonth = YearMonth.now()
        val firstMonth = currentMonth.minusMonths(10)
        val lastMonth = currentMonth.plusMonths(10)
        val daysOfWeek = DayOfWeek.entries
        calendarView.setup(firstMonth, lastMonth, daysOfWeek.last())
        calendarView.scrollToMonth(currentMonth)

        calendarView.dayBinder = object : MonthDayBinder<DayViewContainer> {
            override fun create(view: View) = DayViewContainer(view)

            override fun bind(container: DayViewContainer, data: CalendarDay) {
                container.textView.text = data.date.dayOfMonth.toString()

                container.textView.background = null

                if (data.position == DayPosition.MonthDate) {
                    if (data.date == today) {
                        container.textView.setTextColor(Color.WHITE)
                        container.textView.background = ContextCompat.getDrawable(
                            this@ViewCalendarActivity,
                            R.drawable.solid_circle
                        )
                    } else {
                        container.textView.setTextColor(Color.BLACK)
                    }
                }

                container.view.setOnClickListener {
                    val intent = Intent(this@ViewCalendarActivity, ViewClassActivity::class.java)
                    val formatter = DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.getDefault())
                    intent.putExtra("SELECTED_DATE", data.date.format(formatter))
                    startActivity(intent)
                }
            }
        }

        calendarView.monthHeaderBinder = object : MonthHeaderFooterBinder<MonthViewContainer> {
            override fun create(view: View) = MonthViewContainer(view)
            override fun bind(container: MonthViewContainer, data: CalendarMonth) {
                // Clear any existing views
                container.titlesContainer.removeAllViews()

                // Dynamically add TextViews for each day
                val daysOfWeek = data.weekDays.last().map { it.date.dayOfWeek }
                daysOfWeek.forEach { dayOfWeek ->
                    val textView = TextView(this@ViewCalendarActivity).apply {
                        text = dayOfWeek.getDisplayName(TextStyle.SHORT, Locale.getDefault())
                        layoutParams = LinearLayout.LayoutParams(
                            0,
                            LinearLayout.LayoutParams.WRAP_CONTENT,
                            1f
                        ).apply {
                            gravity = Gravity.CENTER
                        }
                        setTextColor(Color.BLACK)
                        gravity = Gravity.CENTER
                    }
                    container.titlesContainer.addView(textView)
                }
            }
        }


        calendarView.monthScrollListener = object : MonthScrollListener {
            override fun invoke(month: CalendarMonth) {
                monthText.text = month.yearMonth.month.getDisplayName(TextStyle.FULL, Locale.getDefault())
            }
        }
    }
}
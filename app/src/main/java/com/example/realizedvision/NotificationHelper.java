package com.example.realizedvision;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.core.app.NotificationCompat;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.*;

/**
 * One-stop helper for reading a user’s notification-preference flags and then
 * routing the message to the correct transport (in-app, e-mail, SMS).
 *
 * Preferences are stored in Realtime DB:
 *   /Users/{uid}/notificationPreferences/{type} = Boolean
 *
 * If a Boolean for that {type} is missing, the default is:
 *   • true  for *in-app* and *e-mail*
 *   • false for *sms*    (opt-in only)
 *
 * Types must be prefixed with <b>app_</b>, <b>email_</b>, or <b>sms_</b>
 * (e.g. <i>app_orderReady</i>, <i>email_newsletter</i>).
 */
public class NotificationHelper {

    private static final String CHANNEL_ID = "realized_vision_notifications";

    private final Context context;
    private final DatabaseReference prefsRef;
    private final FirebaseUser user;

    public NotificationHelper(Context ctx) {
        this.context = ctx;
        this.user = FirebaseAuth.getInstance().getCurrentUser();

        if (user == null)
            throw new IllegalStateException("NotificationHelper: user not signed-in");

        this.prefsRef = FirebaseDatabase.getInstance()
                .getReference("Users")
                .child(user.getUid())
                .child("notificationPreferences");

        ensureChannel();
    }

    /* ───────────────────────── public API ───────────────────────── */
    public void deliver(String type, String title, String body) {
        prefsRef.child(type).addListenerForSingleValueEvent(
                new ValueEventListener() {
                    @Override public void onDataChange(@NonNull DataSnapshot snap) {
                        Boolean enabled = snap.getValue(Boolean.class);
                        boolean shouldSend = enabled != null ? enabled : !type.startsWith("sms_");
                        if (shouldSend) route(type, title, body);
                    }
                    @Override public void onCancelled(@NonNull DatabaseError e) { /* no-op */ }
                });
    }

    /* ─────────────────────── routing layer ─────────────────────── */
    private void route(String type, String title, String body) {
        switch (type.split("_")[0]) {
            case "app":   inApp(title, body);   break;
            case "email": email(title, body);   break;
            case "sms":   sms(title, body);     break;
        }
    }

    /* ───────────────────── transports (stubs) ───────────────────── */
    private void inApp(String title, String body) {
        Intent i = new Intent(context, MainActivity.class)
                .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        PendingIntent pi = PendingIntent.getActivity(context, 0, i,
                PendingIntent.FLAG_IMMUTABLE);

        NotificationCompat.Builder b = new NotificationCompat.Builder(context, CHANNEL_ID)
                .setSmallIcon(R.drawable.reicon)
                .setContentTitle(title)
                .setContentText(body)
                .setAutoCancel(true)
                .setPriority(NotificationCompat.PRIORITY_DEFAULT)
                .setContentIntent(pi);

        NotificationManager nm = (NotificationManager)
                context.getSystemService(Context.NOTIFICATION_SERVICE);
        nm.notify((int) System.currentTimeMillis(), b.build());
    }

    private void email(String title, String body) {
        if (user.getEmail() == null) return;
        /* TODO — hook up JavaMail / SendGrid / etc.
           Stub left as demonstration. */
    }

    private void sms(String title, String body) {
        /* TODO — integrate Twilio or Android SmsManager as per requirements. */
    }

    /* ──────────────────── notif-channel helper ─────────────────── */
    private void ensureChannel() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) return;

        NotificationManager nm = context.getSystemService(NotificationManager.class);
        if (nm.getNotificationChannel(CHANNEL_ID) != null) return;

        NotificationChannel ch = new NotificationChannel(
                CHANNEL_ID,
                "App notifications",
                NotificationManager.IMPORTANCE_DEFAULT);
        ch.setDescription("Realized Vision notifications");
        nm.createNotificationChannel(ch);
    }
}

package com.example.realizedvision;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import androidx.core.app.NotificationCompat;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentReference;
import com.google.firebase.firestore.DocumentSnapshot;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.remoteconfig.FirebaseRemoteConfig;
import com.google.firebase.remoteconfig.FirebaseRemoteConfigSettings;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.mail.AuthenticationFailedException;
import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

public class NotificationHelper {
    private static final String CHANNEL_ID = "realized_vision_notifications";
    private static final String PREFERENCES_PATH = "notificationPreferences";
    private final FirebaseRemoteConfig remoteConfig;
    private String smtpUser;
    private String smtpPass;
    private final Context context;
    private final FirebaseFirestore firestore;
    private final FirebaseUser currentUser;
    private DocumentReference userPrefsRef;

    public NotificationHelper(Context context) {
        this.context = context;
        this.firestore = FirebaseFirestore.getInstance();
        this.currentUser = FirebaseAuth.getInstance().getCurrentUser();
        this.smtpUser = "";
        this.smtpPass = ""; //initialize as empty to test fetching

        //Firebase Remote Config settings for SMTP settings
        this.remoteConfig = FirebaseRemoteConfig.getInstance();
        FirebaseRemoteConfigSettings configSettings = new FirebaseRemoteConfigSettings.Builder()
                .setMinimumFetchIntervalInSeconds(0)
                .build();
        remoteConfig.setConfigSettingsAsync(configSettings);
        remoteConfig.setDefaultsAsync(R.xml.remote_config_defaults);

        loadSmtpCredentials();

        if (currentUser != null) {
            this.userPrefsRef = firestore.collection("Users").document(currentUser.getUid());
            initializeNotificationPreferences(); // Initialize if they don't exist
        }
        createNotificationChannel();
    }

    private void loadSmtpCredentials() {
        remoteConfig.fetchAndActivate().addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                smtpUser = remoteConfig.getString("smtp_user").trim();
                smtpPass = remoteConfig.getString("smtp_password").trim();
                Log.d("SMTP", "Credentials loaded");

                // Verify we got actual values
                if (smtpUser.isEmpty() || smtpPass.isEmpty()) {
                    Log.e("SMTP", "Empty credentials from RemoteConfig");
                }
            } else {
                Log.e("SMTP", "Failed to load credentials", task.getException());
            }
        });
    }
    private synchronized boolean verifyCredentials() {
        if (smtpUser == null || smtpPass == null) {
            Log.e("SMTP", "Null credentials detected");
            return false;
        }

        // Additional validation
        boolean isValid = !smtpUser.isEmpty() &&
                smtpUser.contains("@") &&
                !smtpPass.isEmpty();

        if (!isValid) {
            Log.e("SMTP", "Invalid credentials - User: " + smtpUser + " Pass: " + smtpPass.length() + " chars");
        }

        return isValid;
    }
    private void initializeNotificationPreferences(){
        userPrefsRef.get().addOnCompleteListener(task -> {
            if(task.isSuccessful()){
                DocumentSnapshot document = task.getResult();
                if(document != null && !document.exists()){
                    //Create new user document for preferences
                    Map<String, Object> userData = new HashMap<>();
                    userData.put(PREFERENCES_PATH, getDefaultPreferences());
                    userPrefsRef.set(userData)
                            .addOnSuccessListener(w -> {
                                Log.d("Notification Helper", "User preferences created");
                            }).addOnFailureListener(e -> {
                                Log.d("Notification Helper", "Error creating preferences");
                            });
                } else if (document != null && !document.contains(PREFERENCES_PATH)) {
                    //add preferences to path
                    Map<String, Object> updates = new HashMap<>();
                    updates.put(PREFERENCES_PATH, getDefaultPreferences());

                    userPrefsRef.update(updates)
                            .addOnSuccessListener(w ->{
                                Log.d("Notification Helper", "User preferences updated successfully");
                            }).addOnFailureListener(e -> {
                                Log.e("Notification Helper", "Error updating preferences: "+  e.getMessage());
                            });
                }
            }else{
                Log.e("Notification Helper", "Error checking preferences", task.getException());
            }
        });


    }

    private Map<String, Boolean> getDefaultPreferences(){
        Map<String, Boolean> defaults = new HashMap<>();
        // App notifications
        defaults.put("app_purchases", true);
        defaults.put("app_messages", true);
        defaults.put("app_reservations", true);
        // Email notifications
        defaults.put("email_purchases", true);
        defaults.put("email_messages", true);
        defaults.put("email_reservations", true);
        // System alerts
        defaults.put("app_security", true);
        defaults.put("email_security", true);
        return defaults;
    }
    public void checkAndSendNotification(String notificationType, String title, String message) {
        if (currentUser == null){
         Log.w("Notification Helper", "Current user is null, cannot send notification");
         return;
        }

        userPrefsRef.get().addOnCompleteListener(task -> {
            if(task.isSuccessful()){
                DocumentSnapshot document = task.getResult();
                if(document != null && document.exists()){
                    Map<String, Boolean> prefs = (Map<String, Boolean>) document.get(PREFERENCES_PATH);
                    if(prefs != null && prefs.containsKey(notificationType)){
                        boolean shouldSend = Boolean.TRUE.equals(prefs.get(notificationType));
                        if(shouldSend){
                            sendNotification(notificationType, title, message);
                        }
                    }else{
                        Log.w("Notification Helper", "Notification type not found in preferences");
                    }
                }
            }else{
                Log.e("Notification Helper", "Error checking notification preferences");
            }
        });
    }

    public void updateNotificationPreference(String preferenceKey, boolean value){
        if(currentUser == null){
            return;
        }
        Map<String, Object> update = new HashMap<>();
        update.put(PREFERENCES_PATH + "." + preferenceKey, value);

        userPrefsRef.update(update)
                .addOnSuccessListener(w ->{
                    Log.d("Notification Helper", "preferences updated successfully");
                })
                .addOnFailureListener(e -> {
                    Log.e("Notification Helper", "Error updating preferences", e);
                });
    }

    public void createNotificationChannel(){
        if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.O){
            CharSequence name = "App Notifications";
            String description = "Channel for app notifications";
            int importance = NotificationManager.IMPORTANCE_DEFAULT;
            NotificationChannel channel = new NotificationChannel(CHANNEL_ID, name, importance);
            channel.setDescription(description);

            NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    public void sendNotification(String type, String title, String content){
        switch (type.split("_")[0]){
            case "app":
                sendInAppNotification(title, content);
                break;
            case "email":
                sendEmailNotification(title, content);
                break;
        }
    }
    public void sendInAppNotification(String title, String content){
        Intent intent = new Intent(context, MainActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        PendingIntent pendingIntent = PendingIntent.getActivity(context,0, intent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder builder = new NotificationCompat.Builder(context, CHANNEL_ID)
                .setSmallIcon(R.drawable.reicon)
                .setContentTitle(title)
                .setContentText(content)
                .setPriority(NotificationCompat.PRIORITY_DEFAULT)
                .setContentIntent(pendingIntent)
                .setAutoCancel(true);
        NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.notify((int) System.currentTimeMillis(), builder.build());
    }

    private boolean areSmtpCredentialsValid() {
        return smtpUser != null && !smtpUser.isEmpty() &&
                smtpPass != null && !smtpPass.isEmpty() &&
                smtpUser.contains("@") && smtpPass.length() >= 8;
    }

    public void sendEmailNotification(String title, String content) {
        if (currentUser == null || currentUser.getEmail() == null) {
            Log.e("Email", "No user email available");
            return;
        }

        // First try with current credentials
        if (areSmtpCredentialsValid()) {
            sendEmail(title, content);
            return;
        }

        // If invalid, reload and try again after 2 seconds
        Log.w("Email", "Reloading SMTP credentials...");
        loadSmtpCredentials();

        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            if (areSmtpCredentialsValid()) {
                sendEmail(title, content);
            } else {
                Log.e("Email", "Failed to send - invalid SMTP credentials");
            }
        }, 2000);
    }

    private void sendEmail(String title, String content) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        executor.execute(() -> {
            try {
                Properties props = new Properties();
                props.put("mail.smtp.host", "smtp.gmail.com");
                props.put("mail.smtp.port", "587");
                props.put("mail.smtp.auth", "true");
                props.put("mail.smtp.starttls.enable", "true");
                props.put("mail.smtp.ssl.protocols", "TLSv1.2");
                props.put("mail.smtp.connectiontimeout", "10000");
                props.put("mail.smtp.timeout", "10000");

                Session session = Session.getInstance(props, new Authenticator() {
                    @Override
                    protected PasswordAuthentication getPasswordAuthentication() {
                        return new PasswordAuthentication(smtpUser, smtpPass);
                    }
                });

                Message message = new MimeMessage(session);
                message.setFrom(new InternetAddress(smtpUser));
                message.setRecipients(Message.RecipientType.TO,
                        InternetAddress.parse(currentUser.getEmail()));
                message.setSubject(title);
                message.setText(content);

                Transport.send(message);
                Log.d("Email", "Email successfully sent to " + currentUser.getEmail());
            } catch (Exception e) {
                Log.e("Email", "SMTP Error: " + e.getClass().getSimpleName() + ": " + e.getMessage());

                // Specific error handling
                if (e instanceof AuthenticationFailedException) {
                    Log.e("SMTP", "Invalid credentials - resetting");
                    smtpUser = "";
                    smtpPass = "";
                }
            }
        });
    }
}

package com.example.realizedvision;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.os.Message;

import androidx.annotation.NonNull;
import androidx.core.app.NotificationCompat;

import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.database.DataSnapshot;
import com.google.firebase.database.DatabaseError;
import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;
import com.google.firebase.database.ValueEventListener;
import com.google.firebase.firestore.FirebaseFirestore;


public class NotificationHelper {
    private static final String CHANNEL_ID = "realized_vision_notifications";
    private final Context context;
    private DatabaseReference userPrefsRef;
    private FirebaseUser currentUser;

    public NotificationHelper(Context context){
        this.context = context;
        currentUser = FirebaseAuth.getInstance().getCurrentUser();
        if(currentUser != null){
            userPrefsRef = FirebaseDatabase.getInstance()
                    .getReference("Users")
                    .child(currentUser.getUid())
                    .child("notificationPreferences");
        }
        createNotificationChannel();
    }

    public void checkAndSendNotification(String notificationType, String title, String message){
        userPrefsRef.child(notificationType).addListenerForSingleValueEvent(
                new ValueEventListener() {
                    @Override
                    public void onDataChange(@NonNull DataSnapshot snapshot) {
                        Boolean isEnabled = snapshot.getValue(Boolean.class);
                        boolean shouldSend = isEnabled != null ? isEnabled :
                                !notificationType.startsWith("sms_");

                        if (shouldSend) {
                            sendNotification(notificationType, title, message);
                        }
                    }

                    @Override
                    public void onCancelled(@NonNull DatabaseError error) {

                    }
                }
        );
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
            case "sms":
                sendSMSNotification(title, content);
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
                .setContentInfo(content)
                .setPriority(NotificationCompat.PRIORITY_DEFAULT)
                .setContentIntent(pendingIntent)
                .setAutoCancel(true);
        NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.notify((int) System.currentTimeMillis(), builder.build());
    }

    public void sendEmailNotification(String title, String content){
        if(currentUser == null || currentUser.getEmail() == null){
            return;
        }
        new Thread(() ->{
            try {
                //email properties
                java.util.Properties props = new java.util.Properties();
                props.put("mail.smtp.host", "your.smtp.server.com");
                props.put("mail.smtp.port", "587");
                props.put("mail.smtp.auth", "true");
                props.put("mail.smtp.starttls.enable", "true");
//                 TODO: implement javax api, setup SMTP server
//                javax.mail.Session session = javax.mail.Session.getInstance(props);
//                Message message = new MimeMessage(session);
            }catch (Exception e){
//                e.printStackTrace();
            }
        }).start();
    }

    public void sendSMSNotification(String title, String content){
//TODO: decide between Twilio API or smsManager

    }
}

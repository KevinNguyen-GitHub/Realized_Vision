package com.example.realizedvision;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.net.Uri;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.fragment.app.FragmentActivity;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.JsonArrayRequest;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.Volley;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.maps.android.PolyUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * Displays a map centred on the current user (vendor or customer) and lets
 * them tap other markers to see distance + walking/biking/driving ETA, with
 * routes drawn via the OpenRouteService Directions API.
 */
public class MapActivity extends FragmentActivity implements OnMapReadyCallback {

    private static final String TAG     = "MapActivity";
    private static final String ORS_KEY = "5b3ce3597851110001cf624847b71804162340aba367fb026dde707c";

    /* ───────────────────────── Firebase ───────────────────────── */
    private final FirebaseFirestore db = FirebaseFirestore.getInstance();
    private final FirebaseUser      user = FirebaseAuth.getInstance().getCurrentUser();

    /* ─────────────────────── map / routing ────────────────────── */
    private GoogleMap map;
    private Polyline  route;
    private LatLng    userLatLng;

    /* ───────────────────────── UI refs ─────────────────────────── */
    private TextView tvDistance, tvWalk, tvBike, tvDrive;
    private ImageView icWalk, icBike, icDrive;

    /* ───────────────────────── lifecycle ───────────────────────── */
    @Override
    protected void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.activity_map);

        if (user == null) { toast("Not signed in"); finish(); return; }

        initViews();
        ((SupportMapFragment) getSupportFragmentManager()
                .findFragmentById(R.id.map)).getMapAsync(this);

        // Target address passed from previous screen
        String selected = getIntent().getStringExtra("selectedAddress");
        if (selected != null && !selected.isEmpty()) geocodeAddress(selected, this::pinDestination);

        // Figure out user (or vendor) address
        fetchUserHome();
    }

    /* ───────────────────── view wiring helpers ─────────────────── */
    private void initViews() {
        tvDistance = findViewById(R.id.distanceTextView);
        tvWalk     = findViewById(R.id.walkingDurationTextView);
        tvBike     = findViewById(R.id.bikingDurationTextView);
        tvDrive    = findViewById(R.id.drivingDurationTextView);
        icWalk     = findViewById(R.id.walkingIcon);
        icBike     = findViewById(R.id.bikingIcon);
        icDrive    = findViewById(R.id.drivingIcon);

        Button back = findViewById(R.id.back_button);
        back.setOnClickListener(v -> db.collection("Users").document(user.getUid()).get()
                .addOnSuccessListener(snap -> {
                    Class<?> dest = Boolean.TRUE.equals(snap.getBoolean("isVendor"))
                            ? StorefrontActivity.class : ProfileActivity.class;
                    startActivity(new Intent(this, dest));
                })
                .addOnFailureListener(e -> toast("Failed to load profile")));
    }

    /* ───────────────────── Firebase address fetch ───────────────── */
    private void fetchUserHome() {
        db.collection("Users").document(user.getUid()).get()
                .addOnSuccessListener(snap -> {
                    boolean isVendor = Boolean.TRUE.equals(snap.getBoolean("isVendor"));
                    String col   = isVendor ? "Vendors" : "Users";
                    db.collection(col).document(user.getUid()).get()
                            .addOnSuccessListener(doc -> {
                                String addr = doc.getString("address");
                                if (addr != null && !addr.isEmpty())
                                    geocodeAddress(addr, latLng -> {
                                        userLatLng = latLng;
                                        if (map != null) centreOnUser();
                                    });
                            });
                });
    }

    /* ───────────────────── map callbacks ───────────────────────── */
    @Override
    public void onMapReady(GoogleMap gMap) {
        map = gMap;
        map.getUiSettings().setZoomControlsEnabled(true);
        map.getUiSettings().setZoomGesturesEnabled(true);

        if (userLatLng != null) centreOnUser();

        map.setOnMarkerClickListener(mk -> {
            if (userLatLng == null) return false;
            if (mk.getPosition().equals(userLatLng)) return true;

            drawRoute(userLatLng, mk.getPosition());
            tvDistance.setText(String.format("Distance: %.2f mi",
                    haversine(userLatLng, mk.getPosition())));
            fetchEtas(userLatLng, mk.getPosition());
            return true;
        });
    }

    private void centreOnUser() {
        map.addMarker(new MarkerOptions().position(userLatLng)
                .title("You").icon(BitmapDescriptorFactory.defaultMarker(
                        BitmapDescriptorFactory.HUE_GREEN)));
        map.moveCamera(CameraUpdateFactory.newLatLngZoom(userLatLng, 12));
    }

    /* ─────────────────────── geocoding ─────────────────────────── */
    private void geocodeAddress(String addr, GeocodeCb cb) {
        String url = "https://nominatim.openstreetmap.org/search?q=" +
                Uri.encode(addr) + "&format=json";
        Volley.newRequestQueue(this).add(new JsonArrayRequest(Request.Method.GET, url, null,
                arr -> {
                    if (arr.length() == 0) { toast("Location not found"); return; }
                    try {
                        JSONObject o = arr.getJSONObject(0);
                        cb.ok(new LatLng(o.getDouble("lat"), o.getDouble("lon")));
                    } catch (JSONException e) { toast("Invalid response"); }
                }, e -> toast("Geocoding failed")));
    }

    private interface GeocodeCb { void ok(LatLng latLng); }

    private void pinDestination(LatLng p) {
        if (map == null) return;
        map.addMarker(new MarkerOptions().position(p).title("Pinned Location")
                .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_RED)));
    }

    /* ─────────────────────── routing ───────────────────────────── */
    private void drawRoute(LatLng from, LatLng to) {
        if (route != null) { route.remove(); route = null; }

        String url = "https://api.openrouteservice.org/v2/directions/driving-car"
                + "?api_key=" + ORS_KEY
                + "&start=" + from.longitude + ',' + from.latitude
                + "&end="   + to.longitude   + ',' + to.latitude;

        Volley.newRequestQueue(this).add(
                new JsonObjectRequest(Request.Method.GET, url, null,
                        res -> {
                            try {
                                JSONArray coords = res.getJSONArray("features")
                                        .getJSONObject(0)
                                        .getJSONObject("geometry")
                                        .getJSONArray("coordinates");
                                List<LatLng> pts = new ArrayList<>();
                                for (int i=0;i<coords.length();i++){
                                    JSONArray p = coords.getJSONArray(i);
                                    pts.add(new LatLng(p.getDouble(1), p.getDouble(0)));
                                }
                                route = map.addPolyline(new PolylineOptions()
                                        .addAll(pts).width(10).color(Color.BLUE));
                            } catch (JSONException e) { toast("Route parse error"); }
                        }, e -> toast("Route fetch failed")));
    }

    /* ─────────────────────── durations ─────────────────────────── */
    private void fetchEtas(LatLng from, LatLng to) {
        String[] prof = {"foot-walking","cycling-regular","driving-car"};
        TextView[] views = {tvWalk,tvBike,tvDrive};
        ImageView[] icons = {icWalk,icBike,icDrive};

        for (int i=0;i<prof.length;i++){
            String url = "https://api.openrouteservice.org/v2/directions/" + prof[i] +
                    "?api_key=" + ORS_KEY +
                    "&start=" + from.longitude + ',' + from.latitude +
                    "&end="   + to.longitude   + ',' + to.latitude;

            final int idx = i;
            Volley.newRequestQueue(this).add(
                    new JsonObjectRequest(Request.Method.GET, url, null,
                            res -> {
                                try{
                                    double min = res.getJSONArray("features")
                                            .getJSONObject(0)
                                            .getJSONObject("properties")
                                            .getJSONArray("segments")
                                            .getJSONObject(0)
                                            .getDouble("duration")/60;
                                    views[idx].setText(String.format("%.1f min",min));
                                    views[idx].setVisibility(View.VISIBLE);
                                    icons[idx].setVisibility(View.VISIBLE);
                                }catch(JSONException e){/*ignore*/}
                            }, e -> Log.e(TAG,"ETA fail",e)));
        }
    }

    /* ─────────────────────── math util ─────────────────────────── */
    private static double haversine(LatLng a, LatLng b){
        final double R=3959; // miles
        double dLat=Math.toRadians(b.latitude-a.latitude);
        double dLon=Math.toRadians(b.longitude-a.longitude);
        double l1=Math.toRadians(a.latitude), l2=Math.toRadians(b.latitude);
        double h=Math.sin(dLat/2)*Math.sin(dLat/2)+
                Math.cos(l1)*Math.cos(l2)*Math.sin(dLon/2)*Math.sin(dLon/2);
        return R*2*Math.asin(Math.sqrt(h));
    }

    /* ─────────────────────── toast helper ──────────────────────── */
    private void toast(String m){ Toast.makeText(this,m,Toast.LENGTH_SHORT).show(); }
}

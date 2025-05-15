
package com.example.realizedvision;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.util.Pair;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

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
import com.google.firebase.firestore.FirebaseFirestore;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class MapActivity extends FragmentActivity implements OnMapReadyCallback {
    private GoogleMap mMap; //instance of map to be used
    private Polyline currentPolyline; //used for ploting route
    private double latitude, longitude; //used for plotting on map. for user location and target location

    private double testLat, testLong;


    private TextView distanceTextView; //distance text view at top

    //walking,biking,driving times textViews at bottom
    private TextView walkingDurationTextView;
    private TextView bikingDurationTextView;
    private TextView drivingDurationTextView;




    private String orsApiKey;

    /**
     * Creates the map and does the following:
     *  1.Initializing and linking UI components for duration and distance measurements
     *  2.Handles map loading to account for async tasks finishing before map can load
     *  3.Handles and plots selectedAddress which is the addresss that the user clicked on to get to the map
     *  4.Handle back button functionality. If isVendor is true then go to vendor profile, else user profile
     *
     * @param savedInstanceState If the activity is being re-initialized after
     *     previously being shut down then this Bundle contains the data it most
     *     recently supplied in {@link #onSaveInstanceState}.  <b><i>Note: Otherwise it is null.</i></b>
     *
     */
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_map);

        // Initialize UI components
        distanceTextView = findViewById(R.id.distanceTextView);
        walkingDurationTextView = findViewById(R.id.walkingDurationTextView);
        bikingDurationTextView = findViewById(R.id.bikingDurationTextView);
        drivingDurationTextView = findViewById(R.id.drivingDurationTextView);
        //There was issue with it not displaying, so manually force display
        distanceTextView.setVisibility(View.VISIBLE);
        walkingDurationTextView.setVisibility(View.VISIBLE);
        bikingDurationTextView.setVisibility(View.VISIBLE);
        drivingDurationTextView.setVisibility(View.VISIBLE);

        //Need this to load map asynchronously, otherwise plotting and routing might occur before async tasks complete
        SupportMapFragment mapFragment = (SupportMapFragment) getSupportFragmentManager().findFragmentById(R.id.map);
        if (mapFragment != null) {
            mapFragment.getMapAsync(this);
        }

        //When user/vendor click on an address it will pass it here into 'selectedAddress' to be plotted
        String selectedAddress = getIntent().getStringExtra("selectedAddress");

        //Fetch current logged in user authentication
        FirebaseAuth auth = FirebaseAuth.getInstance();
        FirebaseFirestore firestore = FirebaseFirestore.getInstance();
        String userId = auth.getCurrentUser().getUid();

        //checks if an address was passed to 'selectedAddress' variable, it then plots it on the map
        if (selectedAddress != null && !selectedAddress.isEmpty()) {
            //convert from string address to lat lon to be plotted
            geocodeAddress(selectedAddress, (lat, lon) -> {
                latitude = lat;
                longitude = lon;
                plotPin(lat, lon); //plot red pin on map
            });
        }

        //see if document in database exists, if it does get the address of the current user
        //by accessing Users document and getting the address field
        //geocodes user address to lat lon object to be plotted onMapReady()

        // Step 1: Check if the user is a vendor
        firestore.collection("Users").document(userId).get()
                .addOnSuccessListener(documentSnapshot -> {
                    if (documentSnapshot.exists()) {
                        Boolean isVendor = documentSnapshot.getBoolean("isVendor");
                        if (isVendor != null && isVendor) {
                            // Fetch from Vendors collection
                            firestore.collection("Vendors").document(userId).get()
                                    .addOnSuccessListener(vendorDoc -> {
                                        if (vendorDoc.exists()) {
                                            String vendorAddress = vendorDoc.getString("address");
                                            if (vendorAddress != null && !vendorAddress.isEmpty()) {
                                                geocodeAddress(vendorAddress, (lat, lon) -> {
                                                    latitude = lat;
                                                    longitude = lon;
                                                    loadMap(); // Load map with vendor location
                                                });
                                            }
                                        }
                                    })
                                    .addOnFailureListener(e -> Log.e("FirestoreError", "Failed to fetch vendor address", e));
                        } else {
                            // Fetch from Users collection
                            String userAddress = documentSnapshot.getString("address");
                            if (userAddress != null && !userAddress.isEmpty()) {
                                geocodeAddress(userAddress, (lat, lon) -> {
                                    latitude = lat;
                                    longitude = lon;
                                    loadMap(); // Load map with user location
                                });
                            }
                        }
                    }
                })
                .addOnFailureListener(e -> Log.e("FirestoreError", "Failed to fetch user isVendor status", e));



//        firestore.collection("Vendors").document(userId).get()
//                .addOnSuccessListener(documentSnapshot -> {
//                    if (documentSnapshot.exists()) {
//                        String userAddress = documentSnapshot.getString("address");
//                        if (userAddress != null && !userAddress.isEmpty()) {
//                            //takes in string address from address fields, converts to lat lon, and pins loads map
//                            geocodeAddress(userAddress, (lat, lon) -> {
//                                latitude = lat;
//                                longitude = lon;
//                                loadMap(); // Now that we have lat/lon, load the map
//                            });
//                        } else {
//                            Log.e("FirestoreError", "Vendor address is empty.");
//                        }
//                    }
//                })
//                .addOnFailureListener(e -> Log.e("FirestoreError", "Failed to fetch user address", e));


        // Handles the back button for when the user wants to exit the map.
        Button backButton = findViewById(R.id.back_button);
        backButton.setOnClickListener(view -> {
            //see if document in database exists, if it does get the isVendor value of the current user
            firestore.collection("Users").document(userId).get()
                    .addOnSuccessListener(documentSnapshot -> {
                        if (documentSnapshot.exists()) {
                            Boolean isVendor = documentSnapshot.getBoolean("isVendor");
                            //if isVendor == true then go to vendor profile, else go to user profile
                            if (isVendor != null && isVendor) {
                                navigateTo(StorefrontActivity.class);
                            } else {
                                navigateTo(ProfileActivity.class);
                            }
                        } else {
                            Toast.makeText(MapActivity.this, "User data not found.", Toast.LENGTH_SHORT).show();
                        }
                    })
                    .addOnFailureListener(e -> {
                        Toast.makeText(MapActivity.this, "Error fetching user data.", Toast.LENGTH_SHORT).show();
                        Log.e("FirestoreError", "Failed to fetch user data", e);
                    });
        });

//        geocodeAddress("Los Angeles CA", (lat, lon) ->{
//            testLat = lat;
//            testLong = lon;
//            plotPin(lat,lon);
//        });


    }


    /**
     * Helper function for loading the map to account for async tasks that might finish before map loading
     * Used for when map may not load correctly
     */
    private void loadMap() {
        SupportMapFragment mapFragment = (SupportMapFragment) getSupportFragmentManager()
                .findFragmentById(R.id.map);
        if (mapFragment != null) {
            mapFragment.getMapAsync(this);
        }
    }


    /**
     * Handles changes that may occur in real time while map is up and running. Map must be loaded and asynch tasks
     * must be completed before this executes
     *  1.
     * @param googleMap --> passes the instance of the map to be loaded
     */
    @SuppressLint("PotentialBehaviorOverride")
    @Override
    public void onMapReady(GoogleMap googleMap) {
        mMap = googleMap;

        // Get the intent extra to check if user location is available
        boolean hasUserLocation = getIntent().getBooleanExtra("hasUserLocation", false);

        // Only plot user location if available and coordinates are valid
        if (hasUserLocation && latitude != 0 && longitude != 0) {
            LatLng userLocation = new LatLng(latitude, longitude);
            mMap.addMarker(new MarkerOptions()
                    .position(userLocation)
                    .title("Your Location")
                    .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_GREEN)));

            // Center map on user location if available
            mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(userLocation, 12));
        }

        // Get the selected address from intent
        String selectedAddress = getIntent().getStringExtra("selectedAddress");

        // Plot the selected vendor location (red pin) if available
        if (selectedAddress != null && !selectedAddress.isEmpty()) {
            geocodeAddress(selectedAddress, (lat, lon) -> {
                LatLng vendorLocation = new LatLng(lat, lon);
                mMap.addMarker(new MarkerOptions()
                        .position(vendorLocation)
                        .title("Vendor Location")
                        .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_RED)));

                // If no user location, center on vendor
                if (!hasUserLocation) {
                    mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(vendorLocation, 12));
                }

                // If both locations are available, calculate route and durations
                if (hasUserLocation && latitude != 0 && longitude != 0) {
                    LatLng userLocation = new LatLng(latitude, longitude);
                    double distance = calculateDistance(userLocation, vendorLocation);
                    drawRouteWithORS(userLocation, vendorLocation);
                    fetchTravelDurations(userLocation, vendorLocation);

                    // Update UI for distance
                    distanceTextView.setText("Distance: " + String.format("%.2f", distance) + " mi");
                    distanceTextView.setVisibility(TextView.VISIBLE);
                }
            });
        }

        // Keep all your existing map settings and click listeners
        mMap.getUiSettings().setZoomControlsEnabled(true);
        mMap.getUiSettings().setZoomGesturesEnabled(true);

        // Marker click listener remains the same
        mMap.setOnMarkerClickListener(marker -> {
            LatLng destination = marker.getPosition();
            if (hasUserLocation && latitude != 0 && longitude != 0) {
                LatLng userLocation = new LatLng(latitude, longitude);
                if (!destination.equals(userLocation)) {
                    double distance = calculateDistance(userLocation, destination);
                    drawRouteWithORS(userLocation, destination);
                    fetchTravelDurations(userLocation, destination);

                    // Update UI for distance
                    distanceTextView.setText("Distance: " + String.format("%.2f", distance) + " mi");
                    distanceTextView.setVisibility(TextView.VISIBLE);
                } else {
                    clearRoute();
                    distanceTextView.setText("Distance: 0.0 mi");
                    walkingDurationTextView.setText("0.0 mi");
                    bikingDurationTextView.setText("0.0 mi");
                    drivingDurationTextView.setText("0.0 mi");
                }
            } else {
                // No user location available - just clear any existing route
                clearRoute();
                distanceTextView.setText("Distance: N/A");
                walkingDurationTextView.setText("N/A");
                bikingDurationTextView.setText("N/A");
                drivingDurationTextView.setText("N/A");
            }
            return false;
        });
    }

//    @SuppressLint("PotentialBehaviorOverride")
//    @Override
//    public void onMapReady(GoogleMap googleMap) {
//        //saves instance of map to a variable
//        mMap = googleMap;
//
//        //Use the geocoded user location and plot (geocoding of userLocation happned in onCreate())
//        //Plots and makes the marker green to signifiy the current location of the user
//        LatLng userLocation = new LatLng(latitude, longitude);
//        Marker userMarker = mMap.addMarker(new MarkerOptions()
//                .position(userLocation)
//                .title("Your Location")
//                .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_GREEN)));
//
//        // Move camera to user's location
//        mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(userLocation, 12));
//        mMap.getUiSettings().setZoomControlsEnabled(true);
//        mMap.getUiSettings().setZoomGesturesEnabled(true);
//
//        //Allow real time interaction with map
//        //Upon clicking on a location, distance and durations will change accordingly
//        mMap.setOnMarkerClickListener(marker -> {
//            LatLng destination = marker.getPosition();
//            //If you click on a location that is not your own, calc route, plot distance, and calc durations
//            if (!destination.equals(userLocation)) {
//                double distance = calculateDistance(userLocation, destination);
//                //get route and display to selected pin
//                drawRouteWithORS(userLocation, destination);
//                //get the travel durations for 3 modes and display
//                fetchTravelDurations(userLocation, destination);
//
//                // Update UI for distance
//                distanceTextView.setText("Distance: " + String.format("%.2f", distance) + " mi");
//                distanceTextView.setVisibility(TextView.VISIBLE);
//            } else {
//                //if you click on your current location, clear any drawn routes
//                clearRoute();
//
//                //reset distances to 0 because you clicked on your location
//                distanceTextView.setText("Distance: 0.0 mi");
//                distanceTextView.setVisibility(TextView.VISIBLE);
//
//                walkingDurationTextView.setText("0.0 mi");
//                walkingDurationTextView.setVisibility(TextView.VISIBLE);
//
//                bikingDurationTextView.setText("0.0 mi");
//                bikingDurationTextView.setVisibility(TextView.VISIBLE);
//
//                drivingDurationTextView.setText("0.0 mi");
//                drivingDurationTextView.setVisibility(TextView.VISIBLE);
//            }
//            return false;
//        });
//    }


    /**
     * This function will take in an address and will take an async listener and will conduct
     * the conversion from string address to latitude longitude values to be used for plotting.
     *  1.Specify JSON format for API response and use Volley (HTTP library) to help with parsing data
     *  2.
     *
     * @param address --> string address to be converted into a lat lon object
     * @param listener --> call back funciton to handle asynchronous environment
     *                 otherwise, you cant return the lat lon values.
     */
    private void geocodeAddress(String address, OnGeocodeCompleteListener listener) {

        //puts response in json
        String url = "https://nominatim.openstreetmap.org/search?q=" +
                Uri.encode(address) + "&format=json";

        //Use Volley to add response to requestqueue
        RequestQueue queue = Volley.newRequestQueue(this);

        JsonArrayRequest request = new JsonArrayRequest(Request.Method.GET, url, null,
                response -> {
                    //Parsing response
                    try {
                        //Make sure response has something before doing logic
                        if (response.length() > 0) {
                            //'place' will hold value at first index of JSON data (only one value per request)
                            JSONObject place = response.getJSONObject(0);
                            //works because 'lat' and 'lon' are fields in the JSON data
                            double lat = place.getDouble("lat");
                            double lon = place.getDouble("lon");

                            //use the event listener to return the lat lon data. Use callback function cuz Volley is async
                            listener.onGeocodeSuccess(lat, lon); // Callback to return data

                        } else {
                            Toast.makeText(this, "Location not found: " + address, Toast.LENGTH_SHORT).show();
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                        Toast.makeText(this, "Error fetching location for " + address, Toast.LENGTH_SHORT).show();
                    }
                },
                error -> Toast.makeText(this, "Geocoding failed for " + address, Toast.LENGTH_SHORT).show());
        //adds to queue for async task to be done
        queue.add(request);
    }

    /**
     * Uses geocodeAddress to convert multiple addresses at once.
     *  1.Converts list of addresses to lat lon
     *  2.Plots each address as a red pin via the plotPin() function
     *
     * @param addresses --> List of string addresses to be passed for when multiple addresses need to be plotted
     */
    private void geocodeAddresses(List<String> addresses) {
        for (String address : addresses) {
            geocodeAddress(address, (latitude, longitude) -> plotPin(latitude, longitude));
        }
    }

    /**
     * This function takes in two LatLng objects and uses them to calculate and draw the route w/ the help of API
     *  1.Call clearRoute() first to clear any routing that may still be there from prior click
     *  2.Perform API call and pass origin longitude and latitude positions for both origin and destination
     *  3.Use Volley HTTP library to help handle the response
     *  4.Extract the polyline coordinates by first converting the 'geometry' JSON objects into JSON array coordinates
     *  5.Create new list to hold coordinates and loop through JSON array coordinates to create polyline points (these points represent turns the car might take to go to the destination)
     *  6.Use the addPolyline function to generate the line through the points, hence the route is shown
     *
     * @param origin --> starting position for the route to be drawn (current user's location)
     * @param destination --> end position for route to end.
     */
    private void drawRouteWithORS(LatLng origin, LatLng destination) {
        clearRoute(); // Clear previous route before drawing a new one

        String apiKey = "5b3ce3597851110001cf624847b71804162340aba367fb026dde707c"; // Replace with your actual API key
        String url = "https://api.openrouteservice.org/v2/directions/driving-car?api_key=" + apiKey +
                "&start=" + origin.longitude + "," + origin.latitude +
                "&end=" + destination.longitude + "," + destination.latitude;

        RequestQueue queue = Volley.newRequestQueue(this);
        JsonObjectRequest request = new JsonObjectRequest(Request.Method.GET, url, null,
                response -> {
                    try {
                        JSONArray routes = response.getJSONArray("features");
                        if (routes.length() == 0) {
                            Toast.makeText(this, "No route found!", Toast.LENGTH_SHORT).show();
                            return;
                        }

                        // Extract Polyline coordinates by converting 'geometry' JSON object to JSON array coordinates
                        JSONObject geometry = routes.getJSONObject(0).getJSONObject("geometry");
                        JSONArray coordinates = geometry.getJSONArray("coordinates");

                        //Loop through JSON array coordinates and convert from JSON data to points to be plotted. (Send lat lon data to variables for plotting)
                        List<LatLng> polylinePoints = new ArrayList<>();
                        for (int i = 0; i < coordinates.length(); i++) {
                            //generate point for the lat,lon at each index
                            JSONArray point = coordinates.getJSONArray(i);
                            //lon at index 0, lat at index 1
                            double lon = point.getDouble(0);
                            double lat = point.getDouble(1);
                            //add polylinepoints for generating route.
                            polylinePoints.add(new LatLng(lat, lon));
                        }

                        // Draw New Route on Map based on polylinePoints calculated above
                        currentPolyline = mMap.addPolyline(new PolylineOptions()
                                .addAll(polylinePoints)
                                .width(10)
                                .color(Color.BLUE)
                                .geodesic(true));
                    } catch (JSONException e) {
                        e.printStackTrace();
                        Toast.makeText(MapActivity.this, "Error parsing route data", Toast.LENGTH_SHORT).show();
                    }
                },
                error -> {
                    Log.e("RouteError", "Failed to get route: " + error.getMessage());
                    Toast.makeText(MapActivity.this, "Failed to get route", Toast.LENGTH_SHORT).show();
                }
        );
        //add the request to the queue to be dealt with because its asynchronous
        queue.add(request);
    }

    /**
     * Clears any routes that are drawn otherwise routes drawn from previous clicks on the map will stay there.
     */
    private void clearRoute() {
        if (currentPolyline != null) {
            currentPolyline.remove();
            currentPolyline = null;
        }
    }

    /**
     * Helper function for plotting points on the map. (Not inclusive to current user location, that is handled in onMapReady())
     * @param latitude --> latitude value
     * @param longitude --> longitude value
     */
    private void plotPin(double latitude, double longitude) {
        if (mMap != null) {
            LatLng location = new LatLng(latitude, longitude);
            mMap.addMarker(new MarkerOptions()
                    .position(location)
                    .title("Pinned Location")
                    .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_RED))); // Change color if needed
        } else {
            Toast.makeText(this, "Map is not ready yet", Toast.LENGTH_SHORT).show();
        }
    }

    /**
     * Calculates the distance in miles based on the origin and destination coordinates
     *  1.Make proper latitude/longitude conversion
     *  2.Plug values into distance algorithm
     * @param origin --> origin as a LAtLng object containing latitude and longitude fields
     * @param destination --> destination as a LAtLng object containing latitude and longitude fields
     * @return
     */
    private double calculateDistance(LatLng origin, LatLng destination) {
        final int EARTH_RADIUS = 3959; // Earth radius in miles

        //find the difference in latitude/longitude and convert to radians
        double latDiff = Math.toRadians(destination.latitude - origin.latitude);
        double lonDiff = Math.toRadians(destination.longitude - origin.longitude);

        //take latitudes from original origin and destination and save
        double lat1 = Math.toRadians(origin.latitude);
        double lat2 = Math.toRadians(destination.latitude);

        //Implement algorithm for calculating distance
        double a = Math.sin(latDiff / 2) * Math.sin(latDiff / 2) +
                Math.cos(lat1) * Math.cos(lat2) *
                        Math.sin(lonDiff / 2) * Math.sin(lonDiff / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return EARTH_RADIUS * c; // Returns distance in miles
    }

    /**
     * Calculates and displays the walking,biking, and driving travel times.
     *  1.Initialize profile list for syntax specific api call for each duration
     *  2.Initialize durationViews list for handling UI text display
     *  3.Perform API call and send the origin/destination lat lon values
     *  4.Check if current location is destination, if so then return from function, onMapReady() will handle zeroing out values
     *
     * @param origin --> origin in LatLng object format
     * @param destination --> destination in LatLng object format
     */
    private void fetchTravelDurations(LatLng origin, LatLng destination) {
        String apiKey = "5b3ce3597851110001cf624847b71804162340aba367fb026dde707c"; // Your ORS API Key
        //profile list needed for api call
        String[] profiles = {"foot-walking", "cycling-regular", "driving-car"};
        //list for handling text view on the UI
        TextView[] durationViews = {walkingDurationTextView, bikingDurationTextView, drivingDurationTextView};
        String[] modeLabels = {"Walking", "Biking", "Driving"};
        ImageView[] icons = {findViewById(R.id.walkingIcon), findViewById(R.id.bikingIcon), findViewById(R.id.drivingIcon)};

        //Loops through each profile and performs api call + calculation + display for walking/biking/driving
        for (int i = 0; i < profiles.length; i++) {
            final int index = i;
            final String profile = profiles[i]; //specific syntax for api call
            final TextView durationView = durationViews[i];
            final String modeLabel = modeLabels[i];
            final ImageView icon = icons[i];

            //Call api and send lat lon values to web server
            String url = "https://api.openrouteservice.org/v2/directions/" + profile + "?api_key=" + apiKey +
                    "&start=" + origin.longitude + "," + origin.latitude +
                    "&end=" + destination.longitude + "," + destination.latitude;


            //Async Volley to handle response from API
            RequestQueue queue = Volley.newRequestQueue(this);
            JsonObjectRequest request = new JsonObjectRequest(Request.Method.GET, url, null,
                    response -> {
                        try {
                            JSONArray routes = response.getJSONArray("features");

                            //check to make sure that location other than current is selected, if so then return and do nothing
                            //onMapReady() will handle zeroing out values
                            if (routes.length() == 0) {
                                durationView.setText("N/A");
                                icon.setVisibility(View.VISIBLE);
                                return;
                            }

                            //
                            JSONObject properties = routes.getJSONObject(0).getJSONObject("properties");
                            //Convert calculated duration to mins.
                            double duration = properties.getJSONArray("segments").getJSONObject(0).getDouble("duration") / 60; // Convert to minutes
                            //Make the calculated value visible to the first decimal
                            //Also make sure that everything else is visible
                            durationView.setText(String.format("%.1f min", duration));
                            durationView.setVisibility(View.VISIBLE);
                            icon.setVisibility(View.VISIBLE);
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    },
                    error -> Log.e("RouteError", "Failed to fetch duration: " + error.getMessage()));
            //Add logic to asynch queue
            queue.add(request);
        }
    }

    /**
     * Interface for geocoding. Need because geocoding is asynchronous so to get values
     * that are generated from within that function, need a callback function to be called
     * when a successful geocode is completed.
     *
     * onGeocodeSuccess, is a function that can be implemented in whatever context is needed
     * after attaining the latitude and longitude values from the geocoding.
     *
     *
     */
    interface OnGeocodeCompleteListener {
        void onGeocodeSuccess(double latitude, double longitude);
    }

    /**
     * Help with back button functionality
     * @param targetActivity --> target UI to go to
     */
    private void navigateTo(Class<?> targetActivity) {
        Intent intent = new Intent(MapActivity.this, targetActivity);
        startActivity(intent);
    }
}






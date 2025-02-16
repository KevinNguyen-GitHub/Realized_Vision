package com.example.realizedvision;
import java.util.Date;
public class Favorite {
    private String favorite_id;
    private String item_id;
    private String vendor_id;
    private Date timestamp;

    public Favorite(){} //No argument constructor for Firestore
    public Favorite(String favorite_id, String item_id, String vendor_id, Date timestamp){
        this.favorite_id = favorite_id;
        this.item_id = item_id;
        this.vendor_id = vendor_id;
        this.timestamp = timestamp;
    }

    public String getFavorite_id(){return favorite_id;}
    public String getItem_id(){return item_id;}
    public String  getVendor_id(){return vendor_id;}

    public Date getTimestamp(){return timestamp;}
    public void setTimestamp(Date timestamp) {this.timestamp = timestamp;}
}

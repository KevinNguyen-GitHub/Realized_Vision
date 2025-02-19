package com.example.realizedvision;

public class Item {
    private String itemID;
    private String vendorID;
    private String name;
    private String description;
    private double price;
    private String imageUrl; // Or a drawable resource ID
    public Item(){
        //default constructor
    }
    public Item(String itemID, String vendorID, String description, String name, double price, String imageUrl) {
        this.itemID = itemID;
        this.vendorID = vendorID;
        this.description = description;
        this.name = name;
        this.price = price;
        this.imageUrl = imageUrl;
    }

    // Getters and setters
    public String getItemID(){return itemID;}
    public String getVendorID(){return vendorID;}
    public String getDescription(){return description;}
    private void setDescription(String description){this.description = description;}

    public String getName() {
        return name;
    }

    public double getPrice() {
        return price;
    }
    private void setPrice(double price){this.price = price;}

    public String getImageUrl() {
        return imageUrl;
    }
}
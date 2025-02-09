package com.example.realizedvision;

public class Item {
    private String name;
    private double price;
    private String imageUrl; // Or a drawable resource ID
    private String description;
    public Item(){
        //default constructor
    }
    public Item(String name, double price, String imageUrl, String description) {
        this.name = name;
        this.price = price;
        this.imageUrl = imageUrl;
        this.description = description;
    }

    // Getters and setters
    public String getName() {
        return name;
    }

    public double getPrice() {
        return price;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public String getDescription(){return description;}
}
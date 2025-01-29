package com.example.realizedvision;

public class Item {
    private String name;
    private double price;
    private String imageUrl; // Or a drawable resource ID

    public Item(String name, double price, String imageUrl) {
        this.name = name;
        this.price = price;
        this.imageUrl = imageUrl;
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
}
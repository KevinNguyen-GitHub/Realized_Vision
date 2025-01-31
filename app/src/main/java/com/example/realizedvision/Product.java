package com.example.realizedvision;

import java.math.BigDecimal;

public class Product {
    private final int productId;
    private final int vendorId;
    private String productName;
    private String description;
    private BigDecimal price;
    private final int imageResId;
    private final int categoryId;

    public Product(int productId, int vendorId, String productName, String description,
                   BigDecimal price, int imageResId, int categoryId){
        this.productId = productId;
        this.vendorId = vendorId;
        this.productName = productName;
        this.description = description;
        this.price = price;
        this.imageResId = imageResId;
        this.categoryId = categoryId;
    }
    public int getProductId(){return productId;}
    public int getVendorId(){return vendorId;}
    public String getProductName(){return productName;}
    public String getDescription(){return description;}
    public BigDecimal getPrice(){return price;}
    public int getImageResId(){return imageResId;}
    public int getCategoryId(){return categoryId;}

}
package com.example.realizedvision;

import java.math.BigDecimal;

public class Product {
    private String productId;
    private String vendorId;
    private String productName;
    private String description;
    private BigDecimal price;
    private String imageUrl;
    private String categoryId;

    public Product(String productId, String vendorId, String productName, String description,
                   BigDecimal price, String imageUrl, String categoryId){
        this.productId = productId;
        this.vendorId = vendorId;
        this.productName = productName;
        this.description = description;
        this.price = price;
        this.imageUrl = imageUrl;
        this.categoryId = categoryId;
    }
    public String getProductId(){return productId;}
    public String getVendorId(){return vendorId;}
    public String getProductName(){return productName;}
    public String getDescription(){return description;}
    public BigDecimal getPrice(){return price;}
    public String getImageUrl(){return imageUrl;}
    public void setImageUrl(String imageUrl){this.imageUrl = imageUrl;}
    public String getCategoryId(){return categoryId;}

}
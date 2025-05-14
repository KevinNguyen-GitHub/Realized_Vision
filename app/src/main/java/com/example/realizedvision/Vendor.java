package com.example.realizedvision;

import androidx.annotation.NonNull;

public class Vendor {
    private String id;
    private String companyName;
    private String address;
    private int yearsInBusiness;

    public Vendor(){};
    public Vendor(String id, String companyName, String address, int yearsInBusiness){
        this.id = id;
        this.companyName = companyName;
        this.address = address;
        this.yearsInBusiness = yearsInBusiness;
    }

    public String getId(){return id;}
    public String getCompanyName(){return companyName;}
    public void setCompanyName(String companyName){this.companyName = companyName;}

    public String getAddress(){return address;}
    public void setAddress(String address){this.address = address;}

    public int getYearsInBusiness(){return yearsInBusiness;}
    public void setYearsInBusiness(int yearsInBusiness){this.yearsInBusiness = yearsInBusiness;}

    @NonNull
    @Override
    public String toString() {
        return "Vendor{" +
                "id='" + id + '\'' +
                ", name='" + companyName + '\'' +
                '}';
    }
}

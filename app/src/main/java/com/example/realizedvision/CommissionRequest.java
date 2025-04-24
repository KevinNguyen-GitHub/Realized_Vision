package com.example.realizedvision;

public class CommissionRequest {
    private String name;
    private String type;
    private String status;
    private String documentId;
    private String size;
    private String style;
    private String budget;
    private String additionalNote;

    // Newly added fields
    private String vendorId;
    private String userId;
    private long timestamp;

    public CommissionRequest() { }

    // Constructor including all fields
    public CommissionRequest(
            String name,
            String type,
            String status,
            String size,
            String style,
            String budget,
            String additionalNote
    ) {
        this.name = name;
        this.type = type;
        this.status = status;
        this.size = size;
        this.style = style;
        this.budget = budget;
        this.additionalNote = additionalNote;
        // vendorId, userId, and timestamp remain uninitialized here
    }

    // Getters
    public String getName()             { return name; }
    public String getType()             { return type; }
    public String getStatus()           { return status; }
    public String getDocumentId()       { return documentId; }
    public String getSize()             { return size; }
    public String getStyle()            { return style; }
    public String getBudget()           { return budget; }
    public String getAdditionalNote()   { return additionalNote; }
    public String getVendorId()         { return vendorId; }
    public String getUserId()           { return userId; }
    public long   getTimestamp()        { return timestamp; }

    // Setters
    public void setName(String name)              { this.name = name; }
    public void setType(String type)              { this.type = type; }
    public void setStatus(String status)          { this.status = status; }
    public void setDocumentId(String documentId)  { this.documentId = documentId; }
    public void setSize(String size)              { this.size = size; }
    public void setStyle(String style)            { this.style = style; }
    public void setBudget(String budget)          { this.budget = budget; }
    public void setAdditionalNote(String note)    { this.additionalNote = note; }
    public void setVendorId(String vendorId)      { this.vendorId = vendorId; }
    public void setUserId(String userId)          { this.userId = userId; }
    public void setTimestamp(long timestamp)      { this.timestamp = timestamp; }
}

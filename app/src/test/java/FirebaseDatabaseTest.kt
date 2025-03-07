import com.google.firebase.database.DataSnapshot
import com.google.firebase.database.DatabaseError
import com.google.firebase.database.FirebaseDatabase
import com.google.firebase.database.ValueEventListener
import org.junit.Assert.assertEquals
import org.junit.Test

class FirebaseDatabaseTest {

    private val database = FirebaseDatabase.getInstance()
    private val testRef = database.getReference("test_data")

    @Test
    fun writeAndReadData() {
        val testData = "Hello, Firebase!"

        // Write data to the database
        testRef.setValue(testData)
            .addOnSuccessListener {
                // Data written successfully, now read it back
                testRef.addListenerForSingleValueEvent(object : ValueEventListener {
                    override fun onDataChange(snapshot: DataSnapshot) {
                        val retrievedData = snapshot.getValue(String::class.java)
                        assertEquals(testData, retrievedData)
                    }

                    override fun onCancelled(error: DatabaseError) {
                        // Handle error
                    }
                })
            }
            .addOnFailureListener {
                // Handle error
            }
    }
}
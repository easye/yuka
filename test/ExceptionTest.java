public class ExceptionTest {
    
    private static void e() throws Exception {
        throw new Exception("ExceptionTest");
    }

    public static void main(String args[]) {
        try {
            e();
        } catch (Exception ex) {
            System.out.println(ex);
        }
    }
}
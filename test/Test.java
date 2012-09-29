public class Test implements java.io.Serializable {

    public Test(int a, String b) {
	this.a = a;
	this.b = b;
    }

    public Test(Test that) {
	this.a = that.a;
	this.b = that.b;
    }

    public int getA() {
	return a;
    }

    public String getB() {
	return b;
    }

    private int a;
    private String b;

    public static void main(String args[]) {
	Test t = new Test(100, "hello, world");
	System.out.println(t.getA() + ", " + t.getB());
    }
}

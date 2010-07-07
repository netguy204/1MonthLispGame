// for some reason the >>> operator is missing
// from clojure core... so this is how I'm adding them
package MonthGame;

public class BitShift {
    public static int unsignedShiftRight(int v, int n) {
	return v >>> n;
    }

    public static byte lowByte(short sh) {
	return (byte) sh;
    }

    public static byte highByte(short sh) {
	return (byte) (sh >>> 8);
    }
}

	

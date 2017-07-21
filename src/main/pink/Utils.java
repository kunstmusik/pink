package pink;

import clojure.lang.IFn;

public class Utils {

  /* from Zach Tellman's Primitive Math library
   * https://github.com/ztellman/primitive-math 
   */
  public static boolean neq(double a, double b) {
    return a != b;
  }

  /* from Zach Tellman's Primitive Math library
   * https://github.com/ztellman/primitive-math 
   */
  public static boolean neq(long a, long b) {
    return a != b;
  }

  /* Trys to call a function, returns the func's return value or
   * nil if an exception was caught.
   */
  public static Object tryFunc(IFn fn) {
    try {
      return fn.invoke();
    } catch (Throwable t) {
      t.printStackTrace();
      return null;
    }
  }
}

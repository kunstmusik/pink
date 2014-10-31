package pink;

import clojure.lang.IFn;

public final class Operator {
 
  public static void sum(double[] out, double[] a) {
    int length = out.length;
    for(int i = 0; i < length; i++) {
      out[i] += a[i];
    }
  }

  public static double[] sum(double[] out, IFn[] fns) {
    double[] result  = (double[])fns[0].invoke(); 
    int length = out.length;
    int fun_length = fns.length;
    if (result == null) {
      return null;
    }
    System.arraycopy(result, 0, out, 0, length);

    for (int i = 1; i < fun_length; i++) {
      result = (double[])fns[i].invoke(); 

      if (result == null) {
        return null;
      }
      for (int j = 0; j < length; j++) {
        out[j] = out[j] + result[j];
      }
    }
    return out;
  }

  public static double[] sub(double[] out, IFn[] fns) {
    double[] result  = (double[])fns[0].invoke(); 
    int length = out.length;
    int fun_length = fns.length;
    if (result == null) {
      return null;
    }
    System.arraycopy(result, 0, out, 0, length);

    for (int i = 1; i < fun_length; i++) {
      result = (double[])fns[i].invoke(); 

      if (result == null) {
        return null;
      }
      for (int j = 0; j < length; j++) {
        out[j] = out[j] - result[j];
      }
    }
    return out;
  }


  public static double[] mul(double[] out, IFn[] fns) {
    double[] result  = (double[])fns[0].invoke(); 
    int length = out.length;
    int fun_length = fns.length;
    if (result == null) {
      return null;
    }
    System.arraycopy(result, 0, out, 0, length);

    for (int i = 1; i < fun_length; i++) {
      result = (double[])fns[i].invoke(); 

      if (result == null) {
        return null;
      }
      for (int j = 0; j < length; j++) {
        out[j] = out[j] * result[j];
      }
    }
    return out;
  }


  public static double[] div(double[] out, IFn[] fns) {
    double[] result  = (double[])fns[0].invoke(); 
    int length = out.length;
    int fun_length = fns.length;
    if (result == null) {
      return null;
    }
    System.arraycopy(result, 0, out, 0, length);

    for (int i = 1; i < fun_length; i++) {
      result = (double[])fns[i].invoke(); 

      if (result == null) {
        return null;
      }
      for (int j = 0; j < length; j++) {
        out[j] = out[j] / result[j];
      }
    }
    return out;
  }

}

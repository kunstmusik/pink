package pink;
import java.nio.ByteBuffer;

public final class EngineUtils {

  public static void writeDoublesToByteBufferAsShorts(double[] audio, ByteBuffer buffer) {
    int len = audio.length;
    double val;
    short min = Short.MIN_VALUE;
    short max = Short.MAX_VALUE;

    for(int i = 0; i < len; i++) {
      val = audio[i] * max;

      if (val < min) {
        buffer.putShort(min);
      } else if (val > max) {
        buffer.putShort(max);
      } else {
        buffer.putShort((short)val);
      }
    }
  }

}

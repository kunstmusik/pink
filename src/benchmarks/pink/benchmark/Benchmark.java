package pink.benchmark;

public class Benchmark {

  public static class Phasor {

    private double cur_phase = 0.0;
    double[] buffer = new double[64];
    double freq, phase;
    double phase_incr;
    int buffer_size;

    public Phasor(double freq, double phase, int sr, int buffer_size) {
       this.freq = freq;
       this.phase = phase;
       this.phase_incr = freq / sr;
       this.buffer_size = buffer_size;
    }

    public double[] tick() {
      double phs = this.phase;

      for(int i = 0; i < buffer_size; i++) {
        buffer[i] = phs;
        phs = (phs + phase_incr) / 1.0;
      }

      this.phase = phs;

      return buffer;
    }

  }

}

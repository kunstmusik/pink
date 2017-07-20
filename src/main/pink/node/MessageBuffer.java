package pink.node;

import java.util.concurrent.atomic.AtomicInteger;
import clojure.lang.IFn;
import clojure.lang.Keyword;

/** Ring buffer with fixed size array of pre-allocated Messages.  
 *  Designed to be lock-free, wait-free, and used with multiple writers and
 *  single reader.  
 *
 *  Does not currently handle back pressure and assumes user will allocate
 *  a large enough size for capacity.
 *
 *  @author Steven Yi
 */
public class MessageBuffer {

  private final int capacity;
  private final Message[] messages;
  private int readStartIndex = 0;
  private final AtomicInteger readEndIndex = new AtomicInteger();
  private final AtomicInteger getIndex = new AtomicInteger();
  private final AtomicInteger putIndex = new AtomicInteger();

  public MessageBuffer() {
    this(512);
  }

  public MessageBuffer(int initialCapacity) {
    this.capacity = initialCapacity;
    messages = new Message[initialCapacity];
    for(int i = 0; i < initialCapacity; i++) {
      messages[i] = new Message();
    }
  }

  private static int getAndIncrementWithModulus(AtomicInteger aInt, int modulus) {
    int getVal; 
    int newVal;

    do {
      getVal = aInt.get();

      newVal = (getVal + 1);
      if(newVal == modulus) {
        newVal = 0;  
      }
    } while(!aInt.compareAndSet(getVal, newVal));

    return getVal; 
  }

  public void postMessage(Keyword msgType, IFn msg) {
    Message m = messages[getAndIncrementWithModulus(getIndex, capacity)];
    m.setMessage(msgType, msg);
    messages[getAndIncrementWithModulus(putIndex, capacity)] = m;
    getAndIncrementWithModulus(readEndIndex, capacity);
  }

  public boolean isEmpty() {
    return readStartIndex == readEndIndex.get();
  }

  public int getReadStart() {
    return readStartIndex;
  }

  public void setReadStart(int start) {
    readStartIndex = start;
  }

  public int getReadEnd() {
    return readEndIndex.get();  
  }

  public Message getMessage(int index) {
    return messages[index];
  }

  public int getCapacity() {
    return capacity;
  }

  //static class Message {
  //  private Keyword msgType;
  //  private IFn msg;

  //  public void setMessage(Keyword msgType, IFn msg) { 
  //    this.msgType = msgType; 
  //    this.msg = msg; 
  //  }

  //  public IFn getMsg() { 
  //    return msg; 
  //  }

  //  public Keyword getMsgType() { 
  //    return msgType; 
  //  }
  //}
}

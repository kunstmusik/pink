package pink;

import clojure.lang.IFn;
import clojure.lang.Keyword;

public class Message {
  private Keyword msgType;
  private IFn msg;

  public void setMessage(Keyword msgType, IFn msg) { 
    this.msgType = msgType; 
    this.msg = msg; 
  }

  public void reset() {
    this.msgType = null;
    this.msg = null;
  }

  public IFn getMsg() { 
    return msg; 
  }

  public Keyword getMsgType() { 
    return msgType; 
  }
}

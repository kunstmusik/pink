package pink.node;

import clojure.lang.IFn;
import java.util.ArrayList;


/** Utility class to hold lists of IFn classes (i.e., Clojure functions).
 * Designed to eliminate runtime object allocations but still grow if
 * necessary. Also designed to be used by single-thread, single-owner.
 *
 *  @author Steven Yi
 */

public class IFnList {
  
  private ArrayList<IFn> activeList; 
  private ArrayList<IFn> backList; 

  public IFnList() {
    activeList = new ArrayList<>(128);
    backList = new ArrayList<>(128);
  }

  public ArrayList<IFn> getActiveList() {
    return activeList;
  }

  public boolean isEmpty() {
    return activeList.size() == 0; 
  }


  public void putBack(IFn func) {
    backList.add(func);
  }

  /** Used at end of function list processing */
  public void swap() {
    ArrayList<IFn> temp = backList;
    backList = activeList;
    activeList = temp;
    backList.clear();
  }

}

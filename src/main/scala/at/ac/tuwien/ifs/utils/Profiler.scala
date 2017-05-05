package at.ac.tuwien.ifs.utils

/**
  * Created by aldo on 31/08/16.
  */
object Profiler {

  def time[T](a:() => (T)): T = {
    print("Profiler: ")
    val t0 = System.currentTimeMillis()
    val r = a()
    val t1 = System.currentTimeMillis()
    println("completed in " + (t1 - t0) / 1000f + "s")
    r
  }

}

package at.ac.tuwien.ifs.r

import scala.sys.process._
/**
  * Created by aldo on 12/05/16.
  */
object Stats {

  def mean(vs:Seq[AnyVal]):Double = if(vs.isEmpty) Double.NaN else {
    (("Rscript -e mean(as.numeric(commandArgs(TRUE))) " + vs.mkString(" ")) !!).drop(4).toDouble
  }

  def sd(vs:Seq[AnyVal]):Double = if(vs.isEmpty) Double.NaN else {
    val r = (("Rscript -e sd(as.numeric(commandArgs(TRUE))) " + vs.mkString(" ")) !!).drop(4).trim
    if(r == "NA") Double.NaN else r.toDouble
  }

  def quantile(vs:Seq[AnyVal]):(Double, Double, Double, Double, Double) = if(vs.isEmpty) (Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN) else {
    (("Rscript -e quantile(as.numeric(commandArgs(TRUE))) " + vs.mkString(" ")) !!).trim.split("\n").last.trim.split("\\s+").map(_.toDouble) match {
      case Array(a: Double, b: Double, c: Double, d: Double, e: Double) => (a, b, c, d, e)
    }
  }

  def cor(a:Seq[AnyVal], b:Seq[AnyVal]) = if(a.size == b.size) {
    (("Rscript -e all=as.numeric(commandArgs(TRUE));a=head(all,length(all)/2);b=tail(all,length(all)/2);cor(a,b) " + a.mkString(" ") + " " + b.mkString(" ")) !!).drop(4).toDouble
  }else
    throw new Exception("wrong sizes")
}

import at.ac.tuwien.ir.evaluation.PoolAnalyser
import at.ac.tuwien.ir.model.{QRels, Run, RunRecord, Runs}
import org.scalatest.{FlatSpec, Matchers}

class TestPoolAnalyser extends FlatSpec with Matchers {

  val runs1Input = List(
    Array("1","Q0","document_id_1", "1", "10"),
    Array("1","Q0","document_id_4", "5", "9"),
    Array("1","Q0","document_id_3", "a", "8"),
    Array("1","Q0","document_id_2", "3", "7"),
    Array("1","Q0","document_id_5", "2", "6"),
    Array("1","Q0","document_id_6", "1", "5"),
    Array("1","Q0","document_id_7", "5", "4"),
    Array("1","Q0","document_id_8", "a", "3"),
    Array("1","Q0","document_id_9", "3", "2"),
    Array("1","Q0","document_id_10", "2", "1"),
    Array("2","Q0","document_id_1", "1", "10"),
    Array("2","Q0","document_id_2", "5", "9"),
    Array("2","Q0","document_id_3", "a", "8"),
    Array("2","Q0","document_id_4", "3", "7"),
    Array("2","Q0","document_id_5", "2", "6"),
    Array("2","Q0","document_id_6", "1", "5"),
    Array("2","Q0","document_id_7", "5", "4"),
    Array("2","Q0","document_id_8", "a", "3"),
    Array("2","Q0","document_id_9", "3", "2"),
    Array("2","Q0","document_id_10", "2", "1"))

  val runs2Input = List(
    Array("1","Q0","document_id_1", "1", "10"),
    Array("1","Q0","document_id_2", "5", "9"),
    Array("1","Q0","document_id_3", "a", "8"),
    Array("1","Q0","document_id_x", "3", "7"),
    Array("1","Q0","document_id_x", "2", "6"),
    Array("1","Q0","document_id_x", "1", "5"),
    Array("1","Q0","document_id_x", "5", "4"),
    Array("1","Q0","document_id_x", "a", "3"),
    Array("1","Q0","document_id_9", "3", "2"),
    Array("1","Q0","document_id_10", "2", "1"),
    Array("2","Q0","document_id_1", "1", "10"),
    Array("2","Q0","document_id_2", "5", "9"),
    Array("2","Q0","document_id_3", "a", "8"),
    Array("2","Q0","document_id_4", "3", "7"),
    Array("2","Q0","document_id_5", "2", "6"),
    Array("2","Q0","document_id_1", "1", "5"),
    Array("2","Q0","document_id_x", "5", "4"),
    Array("2","Q0","document_id_x", "a", "3"),
    Array("2","Q0","document_id_x", "3", "2"),
    Array("2","Q0","document_id_x", "2", "1"))

  val runs3Input = List(
    Array("1","Q0","document_id_x", "1", "10"),
    Array("1","Q0","document_id_x", "5", "9"),
    Array("1","Q0","document_id_4", "a", "8"),
    Array("1","Q0","document_id_x", "3", "7"),
    Array("1","Q0","document_id_x", "2", "6"),
    Array("1","Q0","document_id_x", "1", "5"),
    Array("1","Q0","document_id_x", "5", "4"),
    Array("1","Q0","document_id_x", "a", "3"),
    Array("1","Q0","document_id_x", "3", "2"),
    Array("1","Q0","document_id_x", "2", "1"),
    Array("2","Q0","document_id_4", "1", "10"),
    Array("2","Q0","document_id_x", "5", "9"),
    Array("2","Q0","document_id_x", "a", "8"),
    Array("2","Q0","document_id_x", "3", "7"),
    Array("2","Q0","document_id_x", "2", "6"),
    Array("2","Q0","document_id_x", "1", "5"),
    Array("2","Q0","document_id_x", "5", "4"),
    Array("2","Q0","document_id_x", "a", "3"),
    Array("2","Q0","document_id_x", "3", "2"),
    Array("2","Q0","document_id_x", "2", "1"))

  val qRelsInput = List(
    Array("1","Q0","document_id_1", "1"),
    Array("1","Q0","document_id_2", "0"),
    Array("1","Q0","document_id_3", "0"),
    Array("1","Q0","document_id_4", "0"),
    Array("1","Q0","document_id_5", "1"),
    Array("2","Q0","document_id_1", "0"),
    Array("2","Q0","document_id_2", "0"),
    Array("2","Q0","document_id_3", "0"))

  "PoolAnalyser" should "shoud return a proper depth" in {
    val runs1 = Runs.fromListOfItems(runs1Input)
    val runs2 = Runs.fromListOfItems(runs2Input)
    val runs3 = Runs.fromListOfItems(runs3Input)
    val runs = List(runs1, runs2, runs3)
    val qRels = QRels.fromListOfItems("Test", qRelsInput)

    val poolAnalyser = new PoolAnalyser(runs, qRels)

    poolAnalyser.d should be (3)
  }

  "PoolAnalyser" should "print values in a proper number of pooled Runs" in {
    val runs1 = Runs.fromListOfItems(runs1Input)
    val runs2 = Runs.fromListOfItems(runs2Input)
    val runs3 = Runs.fromListOfItems(runs3Input)
    val runs = List(runs1, runs2, runs3)
    val qRels = QRels.fromListOfItems("Test", qRelsInput)

    val poolAnalyser = new PoolAnalyser(runs, qRels)

    poolAnalyser.pooledRuns.size should be (2)
  }

  "PoolAnalyser" should "should repool a run correctly" in {
    val runs1 = Runs.fromListOfItems(runs1Input)
    val runs2 = Runs.fromListOfItems(runs2Input)
    val runs3 = Runs.fromListOfItems(runs3Input)
    val runs = List(runs1, runs2, runs3)
    val qRels = QRels.fromListOfItems("Test", qRelsInput)

    val poolAnalyser = new PoolAnalyser(runs, qRels)

    poolAnalyser.repoolWith(List(runs2)).size should be (6)
    poolAnalyser.repoolWith(List(runs3)).size should be (1)
    poolAnalyser.repoolWith(runs).size should be (7)
  }

}
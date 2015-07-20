import at.ac.tuwien.ir.model.{Runs, RunRecord, Run}
import at.ac.tuwien.ir.model.Runs
import org.scalatest._

class TestModel extends FlatSpec with Matchers {

  "RunRecord" should "print values in a proper way" in {
    val runRecord = RunRecord.fromItems(Array("Q0","document_id", "1", "0.500"))
    runRecord.toString should be ("Q0 document_id 1 0.5")
  }

  val runInput = List(
    Array("1","Q0","document_id_1", "1", "300"),
    Array("1","Q0","document_id_2", "5", "500"),
    Array("1","Q0","document_id_3", "a", "600"),
    Array("1","Q0","document_id_4", "3", "700"),
    Array("1","Q0","document_id_5", "2", "800"))

  "Run" should "print values in a proper way" in {
    val run = Run.fromListOfItems(runInput)
    run.toString should be (
      "1 Q0 document_id_5 1 800.0\n" +
      "1 Q0 document_id_4 2 700.0\n" +
      "1 Q0 document_id_3 3 600.0\n" +
      "1 Q0 document_id_2 4 500.0\n" +
      "1 Q0 document_id_1 5 300.0")
  }

  "Run" should "return null if the document doesn't exist" in {
    val run = Run.fromListOfItems(runInput)
    run.getByDocumentId("document_id_10") should be (null)
  }

  "Run" should "return the right document if the document_id exists" in {
    val run = Run.fromListOfItems(runInput)
    run.id should be (1)
    run.getByDocumentId("document_id_3").toString should be ("Q0 document_id_3 3 600.0")
  }

  "Normalize Run in Run" should "normalize the ranks of the list" in {
    val run = Run.fromListOfItems(runInput)
    val nRun = new Run(1, Run.normalizeRank(run.runRecords.map(r => new RunRecord(r.iteration,r.document,1,r.score))))
    nRun.getByDocumentId("document_id_3").toString should be ("Q0 document_id_3 3 600.0")
    nRun.getByDocumentId("document_id_1").toString should be ("Q0 document_id_1 5 300.0")
    nRun.toString should be (
      "1 Q0 document_id_5 1 800.0\n" +
      "1 Q0 document_id_4 2 700.0\n" +
      "1 Q0 document_id_3 3 600.0\n" +
      "1 Q0 document_id_2 4 500.0\n" +
      "1 Q0 document_id_1 5 300.0")
  }

  val runsInput = List(
    Array("1","Q0","document_id_1", "1", "300", "runs_id"),
    Array("1","Q0","document_id_2", "5", "500", "runs_id"),
    Array("1","Q0","document_id_3", "a", "600", "runs_id"),
    Array("2","Q0","document_id_4", "3", "700", "runs_id"),
    Array("1","Q0","document_id_5", "2", "800", "runs_id"),
    Array("2","Q0","document_id_7", "1", "300", "runs_id"),
    Array("2","Q0","document_id_2", "5", "500", "runs_id"),
    Array("2","Q0","document_id_3", "a", "600", "runs_id"),
    Array("1","Q0","document_id_4", "3", "700", "runs_id"),
    Array("2","Q0","document_id_5", "2", "800", "runs_id"))

  "Runs" should "print values in a proper way" in {
    val runs = Runs.fromListOfItems(runsInput)
    runs.toString should be (
    "1 Q0 document_id_5 1 800.0 runs_id\n" +
    "1 Q0 document_id_4 2 700.0 runs_id\n" +
    "1 Q0 document_id_3 3 600.0 runs_id\n" +
    "1 Q0 document_id_2 4 500.0 runs_id\n" +
    "1 Q0 document_id_1 5 300.0 runs_id\n" +
    "2 Q0 document_id_5 1 800.0 runs_id\n" +
    "2 Q0 document_id_4 2 700.0 runs_id\n" +
    "2 Q0 document_id_3 3 600.0 runs_id\n" +
    "2 Q0 document_id_2 4 500.0 runs_id\n" +
    "2 Q0 document_id_7 5 300.0 runs_id")
  }

}
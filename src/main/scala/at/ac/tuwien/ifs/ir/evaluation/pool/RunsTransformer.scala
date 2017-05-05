package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Run, Runs}

/**
  * Created by aldo on 28/08/16.
  */
object RunsTransformer {

  def resizeRuns(size:Int, lRuns:List[Runs]):List[Runs] = {
    lRuns.map(runs => resizeRuns(size, runs))
  }

  def resizeRuns(size:Int, runs:Runs):Runs = {
    new Runs(runs.id,
      runs.runs.map(run => new Run(run.id, run.runRecords.take(size))))
  }

}

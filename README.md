# PoolBiasEstimators
With Pool bias in Information Retrieval (IR) is meant the bias of a test collection against new systems that appears as side-effect of the use of the pooling technique.
In this repository we find the implementation of the pool bias estimators analysed by Lipani et al. [1]. 
The developed estimators are: the true estimator (TrueEstimator or TE), the estimator based on the pool (PoolEstimator or PE), the estimator based on runs developed by Webber and Park [2] (WebberOnRunsEstimator or WORE), and the estimator 
developed by Lipani et al. [1] (LipaniEstimator or LE).
The TrueEstimator computes, when possible, the score of the selected run when pooled, useful to compare the true score with the estimated ones; the PoolEstimator computes the score of a run as it was not present during the constraction of the pool, usefull to measure the bias of the test collection and as baseline when comparing the performance of the other estimators. The details about the other estimators can be found in the rispective referenced papers.

The application has two use cases, **analysis** of the test collection and run bias **correction**.
In both cases it is required to provide a) the relevance assessments and b) the set of runs pooled to produce the relevance assessments. All the input files have to fulfil the standard TREC format with exception of the optional *run description file* and *pvalues files* that have their own format (below the details).

The available IR measures are: P@10, P@15, P@20, P@30 and P@100. All the IR measures are *trec_eval* consistent, which means that the following two behaviours are preserved: 1) the order of the documents in a run file is based on their scores and in case of tie, in their document ids in descending order; 2) the document scores are handled in float.

To compare the performances of the varius methods the following measures of error and rank correlation are used: 
Mean Absolute Error (MAE), System Rank Error (SRE), System Rank Error with Statistical Significance (SRE*) and 
Kendall's Tau coefficient B (TauB).

Following the help message of the application:
```sh
$ java -jar poolbiasestimators.jar --help

pool_bias_estimators 1.0
Usage: pool_bias_estimators [options] <trec_rel_file> <trec_runs_dir> [<trec_run_file>]

  -r | --leaveOneRunOut
        active the leave-one-run-out, only in analysis mode
  -o | --leaveOneOrganizationOut
        active the leave-one-organization-out, it requires the file of description of the runs, only in analysis mode
  -s | --onlyRunScoresReport
        print only the run scores report, only in analysis mode
  -b | --onlyPoolBiasReport
        print only the pool bias report, only in analysis mode
  -t | --top75Runs
        use only the top 75% of pooled runs per metric
  -p <value> | --pValues <value>
        directory of files in which are stored the p-values for each pair of runs, if not provided the metrics based on statistical significance (*) are not computed
  -d <value> | --desc <value>
        file of description of the runs, if not provided the leave-one-organization-out can NOT be computed
  <trec_rel_file>
        relevance assessments in standard TREC format
  <trec_runs_dir>
        directory of the runs in standard TREC format
  <trec_run_file>
        run in standard TREC format, if not provided it activates the analysis mode
  --help
        prints this usage text
```

## Analysis of the Pool Bias of a Test Collection
The analysis of the pool bias of a test collection consists in a report that is based on the calculation of how much the score of a run would have been different if an entity had not taken part in the construction of the pool. Where with entity is meant a single run or all the runs submitted by an organization to which the selected run belongs.
To simulate the effect of the test collection on a new system, for each run is unpooled its entity and then the new score compared with the pooled one, where with to unpool is meant pragmatically that all the uniquely identified documents by the given entity are removed from the relevance assessments.
In this way it is possible to compare the score of a run when its entity is and is not part of the pool, and calculate 
how much the score and its rank would have been changed. When it is done for each run in the pool we obtain how much the 
test collection is biased against new systems.

Following an example:
```sh
java -jar poolbiasestimators.jar ./TREC/TREC-14/Robust/QRels ./TREC/TREC-14/Robust/Runs
List Parameters
trec_rel_file  	./TREC/TREC-14/Robust/QRels
trec_runs_dir  	./TREC/TREC-14/Robust/Runs

Test Collection Properties
num_of_runs    	74
num_of_topics  	50

Pool Properties
depth_of_pool  	55
num_pooled_runs	18

Metric	P_5
Leave one RUN out approach
True:
	apl05pd        	0.3280
	ASUDIV         	0.4520
	CKonT          	0.3920
	HKPU2CT        	0.4600
	humR05tl       	0.4960
    [...]
    
Pool:
	apl05pd        	0.3200
	ASUDIV         	0.4400
	CKonT          	0.3760
	HKPU2CT        	0.4560
	humR05tl       	0.4960
	[...]

WebberOnRuns:
	apl05pd        	0.3416
	ASUDIV         	0.4616
	CKonT          	0.3967
	HKPU2CT        	0.4779
	humR05tl       	0.5176
	[...]
	
Lipani:
	apl05pd        	0.3200
	ASUDIV         	0.4400
	CKonT          	0.3760
	HKPU2CT        	0.4560
	humR05tl       	0.4961
	[...]
	
Pool:
	MAE   	0.0204
	SRE   	17
	KTauB 	0.7895

WebberOnRuns:
	MAE   	0.0240
	SRE   	25
	KTauB 	0.7895

Lipani:
	MAE   	0.0165
	SRE   	14
	KTauB 	0.8262

Only 75% Top Best Runs: 13
[...]
```

## Correction of a Run
The second use case, the *correction* of a run, computes given a run its estimated score. Here we can distinguish two output that depend wheater the provided run is part or is not part of the pool. When it is part of the pool also the TE is output, instead when it is not, the TE's score assumes the value of NaN. 

Following an example:
```sh
$ java -jar poolbiasestimators.jar ./TREC/TREC-14/Robust/QRels ./TREC/TREC-14/Robust/Runs ./TREC/TREC-14/Robust/Runs/input.ASUDIV.gz

List Parameters
trec_rel_file	./TREC/TREC-14/Robust/QRels
trec_runs_dir	./TREC/TREC-14/Robust/Runs
trec_runs_file	./TREC/TREC-14/Robust/Runs/input.ASUDIV.gz

Test Collection Properties
num_of_runs    	74
num_of_topics  	50

Pool Properties
depth_of_pool  	55
num_pooled_runs	18

Metric	P_5
True        	ASUDIV         	0.4520
Pool        	ASUDIV         	0.4520
WebberOnRuns	ASUDIV         	0.4724
Lipani      	ASUDIV         	0.4520

Only 75% Top Best Runs: 13
Metric	P_5
True        	ASUDIV         	0.4520
Pool        	ASUDIV         	0.4520
WebberOnRuns	ASUDIV         	0.4763
Lipani      	ASUDIV         	0.4520

Metric	P_10
True        	ASUDIV         	0.4740
Pool        	ASUDIV         	0.4740
WebberOnRuns	ASUDIV         	0.4977
Lipani      	ASUDIV         	0.4740

Only 75% Top Best Runs: 13
Metric	P_10
True        	ASUDIV         	0.4740
Pool        	ASUDIV         	0.4740
WebberOnRuns	ASUDIV         	0.5038
Lipani      	ASUDIV         	0.4740

[...]
```

## Arguments of the Application
### Leave One Run/Organization Out approach (-s or -o with -d <value>), Available Only in *Analysis* Mode
The pooling technique is mostly used during evaluation campaigns. In a evaluation campaign many challenges take place with a 
number of organization partecipating at them, and each organization in each challenge is allowed to submit more then one run. Sometimes the runs submitted by the same organization are generated using the same system with just some difference in the tuning of its parameters. This behaviour has the effect to produce slightly similar runs that would contribute to the pool with a similar set of documents to be judged. To avoid this effect that would minimize the measure of the pool bias, and to make a more realistic analysis of it, we implemented the possibility to switch approach from the default one leave-one-run-out to leave-one-organization-out approach, in which all the runs submitted by the organization of the selected run are unpooled.

The leave-one-organization-out approach requires additional information that needs to be provided through an xml description file like in the following example, in which it is specified to which organization every run belongs.

```xml
<set>
    <runs>
        <tag>CLCLUS</tag>
        <organization>CLARITECH Corporation</organization>
    </runs>
    <runs>
        ...
    </runs>
    ...
</set>
```
### Print Runs Scores Report, Pool Bias Report or both (-r or -b [with -p], default)
The application can output the *runs scores report* or the *pool bias report* or by deafault both.
The runs scores report prints the score of each run for each estimator; the pool bias report prints the values obtained using the measure of error and correlation for each estimators with respect to the TrueEstimator.

### All Pooled Runs or Only Top Best 75% of Pooled Runs (default or -t)
Due to the prototypical nature of IR challenges sometimes organization submit runs with modest performance.
To avoid their effect, as it has been done in the literature, it is possible to remove the bottom 25% of runs poorly scoring from the set of analysed runs.

### Provide pValues (-p /path/to/the/folder)
The pValues files are used by the mesures of errors and correlation marked by a star. For each IR measure is required a separated file that has the following name pattern: "pValues.[Metric].csv". All the values greater then 
0.05 are considered non statistically significance. The format of the files is a list of coma separated values (CSV) as shown in the following example:
```csv
input.ARCJ5,input.ARCJ0,0.58162
input.JuruFull,input.ARCJ0,0.00441
input.JuruFullQE,input.ARCJ0,0.00244
[...]
```

## Bibliography

[1] Splitting Water: Precision and Anti-Precision to Reduce Pool Bias

[2] William Webber and Laurence A. F. Park. 2009. Score adjustment for correction of pooling bias. In Proceedings of the 32nd international ACM SIGIR conference on Research and development in information retrieval (SIGIR '09). ACM, New York, NY, USA, 444-451. DOI=10.1145/1571941.1572018 http://doi.acm.org/10.1145/1571941.1572018
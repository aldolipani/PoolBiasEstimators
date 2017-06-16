# PoolBiasEstimators
With Pool bias in Information Retrieval (IR) is meant the bias of a test collection against new systems that appears as side-effect of the use of the pooling technique.
In this repository we find the implementation of the pool bias estimators analyzed by Lipani et al. [1,2]. 
The developed estimators are: the true estimator (TrueEstimator or TE), the estimator based on the pool (PoolEstimator or PE), the estimator based on runs developed by Webber and Park (WebberOnRunsEstimator or WORE) [3], and the estimator 
developed by Lipani et al. (LipaniEstimator or LE) [1,2].
The TrueEstimator computes, when possible, the score of the selected run when pooled, useful to compare the true score with the estimated ones; the PoolEstimator computes the score of a run as it was not present during the construction of the pool, useful to measure the bias of the test collection and as baseline when comparing the performance of the other estimators. The details about the other estimators can be found in the respective referenced papers.

The application has two use cases, **analysis** of the test collection and run bias **correction**.
In both cases it is required to provide a) the relevance assessments and b) the set of runs pooled to produce the relevance assessments. All the input files have to fulfill the standard TREC format with exception of the optional *run description file* and *pvalues files* that have their own format (below the details).

The available IR measures are: P@10, P@15, P@20, P@30 and P@100. All the IR measures are *trec_eval* consistent, which means that the following two behaviors are preserved: 1) the order of the documents in a run file is based on their scores and in case of tie, in their document ids in descending order; 2) the document scores are handled in float.

To compare the performances of the various methods the following measures of error and rank correlation are used: 
Mean Absolute Error (MAE), System Rank Error (SRE), System Rank Error with Statistical Significance (SRE*) and 
Kendall's Tau coefficient B (TauB).

Following the help message of the application:
```sh
$ java -jar PoolBiasEstimators.jar --help

pool_bias_estimators 2.0
Usage: pool_bias_estimators [options] <trec_rel_file> <trec_runs_dir> [<trec_run_file>]

  --command <value>
        commands avaialble are biasAnalysis and generatePool, if not provided biasAnalysis is default
  --measures <value>
        provide which measures to analyze, if not provided P_*,recall_* is default, biasAnalysis command only
  --estimators <value>
        select which pool bias estimators to use, biasAnalysis command only
  --sizeRuns <value>
        cut the runs to this cut-off, if not provived 0 is default, which means un-sized
  --restore
        restore the previous state of the pool generation, generatePool command only
  -r | --leaveOneRunOut
        active the leave-one-run-out, biasAnalysis command only
  -o | --leaveOneOrganizationOut
        active the leave-one-organization-out, it requires the file of description of the runs, biasAnalysis command only
  -s | --onlyRunScoresReport
        print only the run scores report, biasAnalysis command only
  -b | --onlyPoolBiasReport
        print only the pool bias report, biasAnalysis command only
  -a | --onlyPoolBiasAnalysis
        print only the pool bias analysis, biasAnalysis command only
  --toPoolingStrategy <value>
        create a syntetic pool from Depth@d pooling strategy to another pooling strategy
  --poolAnalyzerType <value>
        specify which algorithm of inference to use to infer the pooled runs and depth of the provided pool, mode or min_depth, if not provided mode is default
  -p | --discoverPooledRuns
        discover the pooled runs based on the provided relevance assessments, generatePool command only
  -i | --interactivePool
        enable the interaction with the pool generation process through RESTful API on the port 3737, generatePool command only
  -t | --top75Runs
        use only the top 75% of pooled runs per metric
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

This application provides to commands, one to analysis pool bias, and the second to generate pools.

## biasAnalysis Command

### Analysis of the Pool Bias of a Test Collection
The analysis of the pool bias of a test collection consists in a report that is based on the calculation of how much the score of a run would have been different if an entity had not taken part in the construction of the pool. Where with entity is meant a single run or all the runs submitted by an organization to which the selected run belongs.
To simulate the effect of the test collection on a new system, for each run is unpooled its entity and then the new score compared with the pooled one, where with to unpool is meant pragmatically that all the uniquely identified documents by the given entity are removed from the relevance assessments.
In this way it is possible to compare the score of a run when its entity is and is not part of the pool, and calculate 
how much the score and its rank would have been changed. When it is done for each run in the pool we obtain how much the 
test collection is biased against new systems.

Following an example:
```sh
java -jar PoolBiasEstimators.jar ./TREC/TREC-14/Robust/QRels ./TREC/TREC-14/Robust/Runs
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

### Correction of a Run
The second use case, the *correction* of a run, computes given a run its estimated score. Here we can distinguish two output that depend whether the provided run is part or is not part of the pool. When it is part of the pool also the TE is output, instead when it is not, the TE's score assumes the value of NaN. 

Following an example:
```sh
$ java -jar PoolBiasEstimators.jar ./TREC/TREC-14/Robust/QRels ./TREC/TREC-14/Robust/Runs ./TREC/TREC-14/Robust/Runs/input.ASUDIV.gz

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

### Arguments of the Application
#### Leave One Run/Organization Out approach (-s or -o with -d <value>), Available Only in *Analysis* Mode
The pooling technique is mostly used during evaluation campaigns. In a evaluation campaign many challenges take place with a 
number of organization participating at them, and each organization in each challenge is allowed to submit more then one run. Sometimes the runs submitted by the same organization are generated using the same system with just some difference in the tuning of its parameters. This behavior has the effect to produce slightly similar runs that would contribute to the pool with a similar set of documents to be judged. To avoid this effect that would minimize the measure of the pool bias, and to make a more realistic analysis of it, we implemented the possibility to switch approach from the default one leave-one-run-out to leave-one-organization-out approach, in which all the runs submitted by the organization of the selected run are unpooled.

The leave-one-organization-out approach requires additional information that needs to be provided through an XML description file like in the following example, in which it is specified to which organization every run belongs.

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
#### Print Runs Scores Report, Pool Bias Report or both (-r or -b [with -p], default)
The application can output the *runs scores report* or the *pool bias report* or by default both.
The runs scores report prints the score of each run for each estimator; the pool bias report prints the values obtained using the measure of error and correlation for each estimators with respect to the TrueEstimator.

#### All Pooled Runs or Only Top Best 75% of Pooled Runs (default or -t)
Due to the prototypical nature of IR challenges sometimes organization submit runs with modest performance.
To avoid their effect, as it has been done in the literature, it is possible to remove the bottom 25% of runs poorly scoring from the set of analyzed runs.

#### Transform a Depth@d pooling strategy to another (-toPoolingStrategy <value>)
To analyze a test collection or a run assuming a different pooling strategy than the one used to build the test collection, it is possible to create a synthetic pool, from a pool constructed with a Depth@d pooling strategy, with one of the following pooling strategies: Depth@d, SampledDepth@d&r and Stratified.

## generatePool Command

In this section we present the generatePool command, which aims to generate a pool based on, existing relevance assessments, or user interaction.

### Generate a Pool from existing Relevance Assessments

To generate a new set of relevance assessments based on existing relevance assessments, for example using the TREC 14 Robust test collection, with run size 100 and using a Multi-Armed Bandit-based pooling strategy named MaxMean [4], we run the following command:
```sh
$ java -jar PoolBiasEstimators.jar --command generatePool \
                                   --sizeRuns 100 \
                                   --toPoolingStrategy mabbased_maxmean:10000 \
                                   /Users/aldo/Projects/TestCollections/TREC/TREC-14/Robust/QRels \
                                   /Users/aldo/Projects/TestCollections/TREC/TREC-14/Robust/Runs
```

This will print out the new set of relevance assessments:

```sh
303 0 XIE20000522.0056 0
303 0 XIE20000519.0111 0
303 0 XIE19991228.0201 0
303 0 XIE19990813.0262 0
303 0 XIE19990416.0045 2
303 0 XIE19990109.0245 2
303 0 XIE19981010.0047 2
...
303 0 NYT20000218.0252 -1
```
Rel equals to -1 indicates that the document has not been judged. This happens when the document is also not judged in the provided relevance assessments.

### Generate a Pool from User Interaction

To make users (or third-party software) able to interact with the pool generation functionality we have developed a RESTful API. To activate this service you need to provide the interactive flag (-i). As in the previous example, to generate a new set of relevance assessments based on the TREC 14 Robust test collection, with the run size 100 and using a Multi-Armed Bandit-based pooling strategy named MaxMean [4], we run the following command:

```sh
$ java -jar PoolBiasEstimators.jar --command generatePool \
                                   -i \
                                   --sizeRuns 100 \
                                   --toPoolingStrategy mabbased_maxmean:10000 \
                                   /Users/aldo/Projects/TestCollections/TREC/TREC-14/Robust/QRels \
                                   /Users/aldo/Projects/TestCollections/TREC/TREC-14/Robust/Runs
```

The application will then start a server visible at the address <http://localhost:3737>. Following the reply of the web server when sent an HTTP GET request at this address soon after the web server has been started:

```json
{
  "value": [{
    "topic": 303,
    "document": "",
    "left": -1,
    "state": "wait"
  }, {
    "topic": 307,
    "document": "",
    "left": -1,
    "state": "wait"
  }, {
    "topic": 310,
    "document": "",
    "left": -1,
    "state": "wait"
  }, ...],
  "status": "OK"
}
```

Following the reply of the web server when ready to start the judgment process (note that "wait" changed to "judge"):

```json
{
  "value": [{
    "topic": 303,
    "document": "APW19990310.0063",
    "left": 275,
    "state": "judge"
  }, {
    "topic": 307,
    "document": "XIE19971211.0031",
    "left": 809,
    "state": "judge"
  }, {
    "topic": 310,
    "document": "NYT20000726.0313",
    "left": 590,
    "state": "judge"
  }, ...],
  "status": "OK"
}
```

Now if we want to judge document "APW19990310.0063" for topic 303 as a relevant document, we need to send this HTTP GET request:
<http://localhost:3737/judge?topic=303&document=APW19990310.0063&rel=1>
or if non relevant:
<http://localhost:3737/judge?topic=303&document=APW19990310.0063&rel=0>
To both requests the server will reply with:
```json
{
  "status": "OK"
}
```

In case of accidental interruption of the generation process, the application provides a way to restore the status of the generation process from the point when it was interrupted. This is done by providing the last generated relevance assessments, and the flag restore (--restore), as follows:

```sh
$ java -jar PoolBiasEstimators.jar --command generatePool \
                                   -i \
                                   --restore \
                                   --sizeRuns 100 \
                                   --toPoolingStrategy mabbased_maxmean:10000 \
                                   /Users/aldo/Projects/TestCollections/TREC/TREC-14/Robust/QRels \
                                   /Users/aldo/Projects/TestCollections/TREC/TREC-14/Robust/Runs
```

## Bibliography

[1] Lipani, Aldo, Mihai Lupu, and Allan Hanbury. "Splitting Water: Precision and Anti-Precision to Reduce Pool Bias." Proceedings of the 38th International ACM SIGIR Conference on Research and Development in Information Retrieval. ACM, 2015. DOI=10.1145/2766462.2767749 http://doi.acm.org/10.1145/2766462.2767749

[2] Lipani, Aldo, Mihai Lupu, and Allan Hanbury. "The curious incidence of bias corrections in the pool." European Conference on Information Retrieval. Springer International Publishing, 2016. https://link.springer.com/chapter/10.1007/978-3-319-30671-1_20

[3] Webber, William, and Laurence AF Park. "Score adjustment for correction of pooling bias." Proceedings of the 32nd international ACM SIGIR conference on Research and development in information retrieval. ACM, 2009. DOI=10.1145/1571941.1572018 http://doi.acm.org/10.1145/1571941.1572018

[4] TOIS Paper
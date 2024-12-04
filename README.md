# SIMPLE
Code to simulate the fishes flow at landing in the small pelagic fishery and compare different sampling designs and estimators of the variance.

The current status of the simulator is shown below. For instance consider a trip having caught 50 tonnes of fishes in three hauls, with the catch deriving from one haul with proportion (in weight) of herring respectively being 0.66.

In order to accomodate such a catch we need to build a tank and tube of sufficient height and width. 
The simulation parameters are hence:

``` 
nHaul = 1 # Number of hauls
p_herring = c(0.66) # Proportion of herring. 
W = c(50000) # Catch of both species. 
tankHeight = 3500
tankLength = 2000
heightTube = 1000 
lengthTube = 3000

```
The fishes caught sampled are expected to come from an empirical length weight distribution.  

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/LWRcatch_4.png" width="500" title="hover text">
</p>

Therefore the simulator generates the single fishes by sampling from a species - specific weight distribution until the weight of the catch specified for each haul (just one in this case) is met. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/DistExtractedFishsim4.png" width="500" title="hover text">
</p>

Once a vector of fishes belonging to a species with a weight assigned to each is generated and the fishes are mixed randomly, the weight variable is used to calculate the volume of each fish based on the species it belongs to (conv. factors: her = 932.274568364 gram/L; spr = 852.182251494 gram/L). 
These information flow into an array containing information on each fish species, weight and volume used to represent the details of the catch in each of the hauls. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/assets/99275660/cb63d60f-29fa-4f37-8285-ecbb14eeb4f9" width="500" title="hover text">
</p>

This is done for each of the hauls, allowing us to build an haul list containing all the information needed for the simulation. 

```
## We obtain the features of the haul enclosed in the haulList object: 
# For instance for the first haul
haulsList$Haul_1 # Content of the first haul
haulsList$Haul_1$catch_w # Catch in the first haul
haulsList$Haul_1$p_herring # Proportion of herring in the first haul
haulsList$Haul_1$p_sprat # Proportion of sprat in the first haul
haulsList$Haul_1$fishes[,,1] # Identifier of the single fishes in the first haul
haulsList$Haul_1$fishes[,,2] # Species of the single fishes in the first haul
haulsList$Haul_1$fishes[,,3] # Weight of the single fishes in the first haul
haulsList$Haul_1$fishes[,,4] # Volume of the single fishes in the first haul

```

Then, the system hosting the fish flow is built. In principle, this consists in two arrays simulating the tank and the tube and a connection between the two. 

![emptyFlow_sim4](https://github.com/hlab-design-analysis/SIMPLE/assets/99275660/6090dd2d-3d6c-4db5-9f09-3b53c75c96f6)

The function written to build the system allows to visualize also the different dimensions of the array (i.e. to see the flow of fishes coloured by volume or weight instead of species) and to visualize the proportion of herring and sprat detected at different "sensors" (i.e. row/col intervals) of both the tube and the tank.

 ![emptyFlow_withAdditionalVarAndProp](https://github.com/hlab-design-analysis/SIMPLE/assets/99275660/befe8a2d-4672-422d-b9ed-82bdd896680c)

Once the system is built, the fishes are "poured" in the tank. This operation is done in order by haul (starting with haul 1) in case more than one haul is present and allocating the fishes of each allow to random positions in a range of the tank that is proportional to the magnitude in weight of the relative catch. Each fish (identified by the fish identifier) is followed by its species, weight, volume and haul information. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/allPouredTank_sim4.png" width="1000" title="hover text">
</p>

The fishes are then "sinked" to the bottom of the tank, i.e. they fill all the species below them and leave the top of the tank empty if the magnitude of the total catch allow. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/allSinkedTank_sim4.png" width="1000" title="hover text">
</p>

Finally the fishes are allowed to flow through the tube. This is done by sampling from the bottom of the tank an amount of fishes equal (or all fishes in case the total amount of fishes in the bottom row is less than) the height of the tube at each time step, until the tank is full. The tube matrix is saved when the time step is equal to its length, ensuring the possibility to reconstruct each step of the flow. At each time step a plot of the flow can be produced and stored, to be later used to produce a video of the flow (note this may temporary differ here from the simulation presented above.. :). 

https://github.com/hlab-design-analysis/SIMPLE/assets/99275660/a76c112c-ff2b-4caf-a6a9-4a3ebcabb65b

The information on the flow of fishes is recorded leading to the possibility of representing the whole flow that happened in the tube, from the start until both the tank and the tube are completely empty. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/flowTot_sim4_static.png" width="1000" title="hover text">
</p>

This flow is then free of the long empty sections on the right (corresponding to the phase in which the tube was to be filled) and on the right (corresponding to the phase in which the tube got empty again). 
Finally, the flow is segmented by an algorithm that moves along the flow matrix from the first fish entering the tube and to the last and assign each of the fishes to a ton and a bucket. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/flowSegmentedInTons.png" width="1000" title="hover text">
</p>

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/flowSegmentedInBuckets.png" width="1000" title="hover text">
</p>

Once the flow is segmented two strategy of sampling an agreed number (n = 30) of buckets are applied. 

<br> 
<br> 

First, an initial simple random sampling (SRS) scheme is applied, by extracting randomly the buckets and linking them to the ton they belong to. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/selectedTonSRS.png" width="1000" title="hover text">
</p>

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/selectedBucketsSRS.png" width="1000" title="hover text">
</p>


Second, a systematic sampling (SS) scheme is applied, by generating all possible combination of n (n = 30) buckets available from the population of buckets, extracting one sequence at random, and linking the resulting buckets to the ton they belong to. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/selectedTonsSS.png" width="1000" title="hover text">
</p>

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/selectedBucketsSS.png" width="1000" height="800" title="hover text">
</p>

Finally, the proportion obtained using the two schemes are checked leading to the results of the simulation, showing the proportion estimated (E(P)) with each sampling design ("Scheme"), its variance ("V(P)"), and how much it differs ("P-E(P)") from the true value ("P") of the proportion for the two species (1 = Herring, 2 = Sprat). 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/finalComparisonSchemes_singleExtraction.png" width="1000" height="500" title="hover text">
</p>

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/results_ComparisonSchemes_4_singleExtraction.jpg" width="1000" height="500" title="hover text">
</p>

When the mentioned sampling schemes are repeated (n = 500) the following results are observed. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/finalComparisonSchemes_multiExtraction.png" width="1000" height="500" title="hover text">
</p>

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/results_ComparisonSchemes_4_multiExtraction.jpg" width="1000" height="500" title="hover text">
</p>


Eight alternative methods for calculating the variance of systematic samples were tested. These methods are defined by Wolter (1984).

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/varianceEstimatorsWolter1984.png" width="1000" height="500" title="hover text">
</p>


The comparison between the estimator was made across schemes (SRS and SS) for each replica (n = 1000). The results are available for both [SRS](https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/results_VarianceComparisonSchemes_4_multiExtraction_SRS.jpg) and  [SS](https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/results_VarianceComparisonSchemes_4_multiExtraction_SS.jpg) at the given links. 
The comparison of the variances produced by different estimators allowed to assess that the estimator V8 is the variance estimator performing best in the majority of the cases and across both sampling schemes. 

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/finalComparisonSchemes_varianceEstimation_multiExtraction.png" width="1000" height="500" title="hover text">
</p>

<p align="center">
  <img src="https://github.com/hlab-design-analysis/SIMPLE/blob/dev/Simulators/Simulator2_SIMPLE/results_SIMPLE/Simulation4/results_VarianceComparisonSchemes_4_multiExtraction_FINAL.jpg"width="1000" height="500" title="hover text">
</p>





















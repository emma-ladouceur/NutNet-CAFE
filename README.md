# NutNet-CAFE

Code Authors: Emma Ladouceur, [Shane A. Blowes](https://github.com/sablowes) & [Adam T. Clark](https://github.com/adamtclark)
 
Community Assembly and the Functioning of Ecosystems (CAFE) : temporal responses to Nutrient addition in a globally distributed experiment: [The Nutrient Network](https://nutnet.org/home).

### Data
Species level data is not provided. Data openly available to reproduce results includes;

**plot.csv** Includes details about sites, blocks, plots, time, treatments, and plot-level measures of species richness and strip level biomass. These data are needed to run models associated with, and to produce Figure 2.

**nutnet_cumulative_time.csv** Includes details about pairwise comparisons in terms of sites, blocks, plots, time, and provides plot-level measures of species loss (s.loss), species gain (s.gain), biomass loss associated with species loss (SL), biomass loss associated with  species gain (SG), and biomass loss associated with persistent species (PS). These data are needed to run models associated with and to produce Figure 3, 4, & 5.

**Model Objects**  The data listed above are used in statistical models, then model objects are saved and provided. Data is extracted from these model objects  and are saved as data objects which are also provided, and then can then be used to produce figures without re-running the models. 


### **R Scripts** 
R Script file names- which are listed below, are numbered and listed in the order they should be used. Price equation partitions and statistical models are run on the cluster, which means this workflow can not be perfectly run through continuously on a local machine. However, code from cluster scripts can be used pedagogicly or be adapted to run on a local machine, just be aware they take a few hours each and require substantial computation power.

**1_Data_Prep.R** This script cleans data to living herbaceous vascular species only and calculates per species biomass according to total plot biomass and life form group (graminoid, forb, legume etc.). Original data associasted with this code not provided.

**2_Data_Prep.R** This workflow takes cleaned, filtered data (woody non-vasculars removed) per species biomass estimate data and re-calculates measures of species richness. Original data associated with this code not provided.

**3_Data_Prep.R** This preps data for price equation comparisons, which just pairs data into temporal subsets for every site for meaningful comparisons and outputs subsets of the data in pairs

**FOLDER: cluster -> price_pairs**
- *Price_Pairs_Wrapper.sh* This is a wrapper script to submit multiple jobs for every subsetted dataset to the cluster
- *Price_Pairs_Submit.sh* This is the submit script to submit each job to the cluster
- *Price_Pairs.R* This is the R script that runs in the cluster iteratively over every subset of data

**4_Data_Prep.R** This workflow uses the data produced by the script 'Price_Pairs.R", which is submitted to the cluster by wrapper submit script, "Price_Pairs_Wrapper.sh" and nested Submit Script "Price_Pairs_Submit.sh". In this script we prune the pairwise comparisons down to only the meaningful temporal pairings that we are interested in.

**FOLDER: cluster -> univariate models, multivariate models**
- *A submit script ('.sh') to send each R script to the cluster, and an R Script ('.R') for every statistical model, named intuitively*

**5_Model_Data_Extract.R** This workflow pulls Data out of *Richness & Biomass* model objects and preps data for visualization in Figure 2. Models have been run on a cluster and each have an R script and a submit script (.sh)

**6_Model_Data_Extract.R** This workflow pulls Data out of *Price equation partitions* model objects and preps data for visualization in Figure 3. Models have been run on a cluster and each have an R script and a submit script (.sh)

**7_Model_Data_Posteriors.R** This workflow pulls data out of posterior distributions from *Price equation partitions* model objects and prepares it for: Figure 2 & 3 inset plots, Figure 2c, Figure 5, Figure S5, & Shiny App, Also produces 'quadrant categories' used in several Figures and in Table S1.

**8_Figure_2.R** This script produces Figure 2 a), b) & c).

**9_Figure_3**  The script produces Figure 3 a) - f).

## **Figure4 is under costruction**
**10_Figure_4.R** This script produces Figure 4 currently in MS.

**10_Figure_4.2.R** This is Figure 4 with the PS effect plotted first. Might become supplementary.

**10_Figure_4.3.R** This also puts persistent species on the x axis. Does this even make sense? I dont think so. If it does, this is the favoured version. Has a mistake! need to fix. Control species gains (x-axis) arent added properly.

**10_Figure_4.4.R** This plots persistent species first, and puts persistent species on the x axis. Still doesnt make sense.

**11_Figure_5.R** This script produces Figure 5 a)- e).

**12_Supplementary_Figures.R** This script produces Figure S1, S2, S5.

**13_Supplementary_Figure_S4.R** This script produces Figure S4.

**14_Supplementary_Tables** This script produces Tables S1 & S2.


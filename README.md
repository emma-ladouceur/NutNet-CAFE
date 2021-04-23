# NutNet-CAFE

Code Authors: Emma Ladouceur, [Shane A. Blowes](https://github.com/sablowes) & [Adam T. Clark](https://github.com/adamtclark)
 
Community Assembly and the Functioning of Ecosystems (CAFE) : temporal responses to Nutrient addition in a globally distributed experiment: The Nutrient Network.

### Data
Will detail data provided here.

### **R Scripts** 
R Script file names- which are listed below, are numbered and listed in the order they should be used. Price equation partitions and statistical models are run on the cluster, which means this workflow can not be perfectly run through continuously on a local machine. However, code from cluster scripts can be used pedagogicly or be adapted to run o a local machine, just be aware they take time and require immense computation power.

**1_Data_Prep.R** This script cleans data to living herbaceous vascular species only and calculates per species biomass according to total plot biomass or life form group (graminoid, forb, legume etc.)

**2_Data_Prep.R** This workflow takes cleaned, filter data (woody non-vasculars removed) per species biomass estimate data and re-calculates measures of species richness

**3_Data_Prep.R** Preps data for price equation comparisons, which just pairs data into temporal subsets for meaningful comparisons and outputs subsets of the data in pairs

**FOLDER: cluster -> price_pairs**

- *Price_Pairs_Wrapper.sh* This is a wrapper script to submit multiple jobs for every subsetted dataset to the cluster

- *Price_Pairs_Submit.sh* This is the submit script for the cluster

- *Price_Pairs.R* This is the R script that runs in the cluster

**4_Data_Prep.R** This workflow uses the data produced by the script 'Price_Pairs.R", which is submitted to the cluster by wrapper submit script, "Price_Pairs_Wrapper.sh" and nested Submit Script "Price_Pairs_Submit.sh". In this script we prune the pairwise comparisons down to only meaningful temporal pairings.

**FOLDERs: cluster -> univariate models, multivariate models**
- *A submit script ('.sh') and an R Script ('.R') for every statistical model, named intuitively*

**5_Model_Data_Extract.R** This workflow pulls Data out of *Richness & Biomass* model objects and preps data for visualization in Figure 2. Models have been run on a cluster and each have an R script and a submit script (.sh)

**6_Model_Data_Extract.R** This workflow pulls Data out of *Price equation partitions* model objects and preps data for visualization in Figure 3. Models have been run on a cluster and each have an R script and a submit script (.sh)

**7_Model_Data_Posteriors.R** This workflow pulls data out of posterior  distributions from *Price equation partitions* model objects and prepares it for: Figure 5, Figure #, Figure # & Shiny App

**8_Figure_2.R** This script produces Figure 2 a) , b) & c).

**9_Figure_3**  The script produces Figure 3 a) - f).

**10_Figure_4.2.R**

**11_Figure_4.3.R**

**10_Figure_4.R** This script produces Figure 4.

**11_Figure_5.R** This script produces Figure 5.

**12_Supplementary_Figures.R** This script produces Figure S1, S2, S5.

**13_Supplementary_Figure_S4.R** This script produces Figure S4.

**14_Supplementary_Tables** This script produces Tables S1 & S2.







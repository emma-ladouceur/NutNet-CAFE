# NutNet-CAFE

Code Authors: Emma Ladouceur, [Shane A. Blowes](https://github.com/sablowes) & [Adam T. Clark](https://github.com/adamtclark)
 
Linking changes in species composition and biomass in a globally distributed grassland experiment: [The Nutrient Network](https://nutnet.org/home).

*Emma Ladouceur, Shane A. Blowes, Jonathan M. Chase, Adam T. Clark, Magda Garbowski, Juan Alberti, Carlos Alberto Arnillas, Jonathan D. Bakker, Isabel C. Barrio, Siddharth Bharath, Elizabeth T. Borer, Lars A. Brudvig, Marc W. Cadotte, Qingqing Chen, Scott L. Collins, Christopher R. Dickman, Ian Donohue, Guozhen Du, Anne Ebeling, Nico Eisenhauer, Philip A. Fay, Nicole Hagenah, Yann Hautier, Anke Jentsch, Ingibjörg S. Jónsdóttir, Kimberly Komatsu, Andrew MacDougall, Jason P. Martina, Joslin L. Moore , John W. Morgan, Pablo L. Peri, Sally A. Power, Zhengwei Ren, Anita C. Risch, Christiane Roscher, Max A. Schuchardt, Eric W. Seabloom, Carly J. Stevens, G.F. (Ciska) Veen, Risto Virtanen, Glenda M. Wardle, Peter A. Wilfahrt, W. Stanley Harpole. (2022) Linking changes in species composition and biomass in a globally distributed grassland experiment. Ecology Letters. Article DOI: & Data DOI: Coming soon*

#### **Shiny App**: Explore site-level results from this paper and analyses [Here](https://emma-ladouceur.shinyapps.io/nn-cafe-app/) & see Code to produce App [here](https://github.com/emma-ladouceur/NN-CAFE-App).

Approach based on Community Assembly and the Functioning of Ecosystems (CAFE) Approach. Some nice papers on the subject by  [Leibold, Chase & Ernest (2017)](https://doi.org/10.1002/ecy.1697) and [Bannar-Martin et al. (2018)](https://doi.org/10.1111/ele.12895).

### Data
Species level data is not provided. Data openly available to reproduce results includes;

**plot.csv** Data used for the plot-level. Includes details about sites, blocks, plots, time, treatments, and plot-level measures of species richness and strip level biomass. These data are needed to run models associated with, and to produce Figure S5.

**FOLDER: Price Pairs Data** Compressed folder containing data produced by *cluster -> Price_Pairs* scripts for the following responses:
- *Biomass* -> used for main analyses
- *Cover* Used for Figure S4

**nutnet_cumulative_time.csv** Data used for main analysis. Price equation partition outputs using per species biomass estimates.  Includes details about pairwise comparisons in terms of sites, blocks, plots, time, and provides plot-level measures of species loss (s.loss), species gain (s.gain), biomass loss associated with species loss (SL), biomass loss associated with  species gain (SG), and biomass loss associated with persistent species (PS). These data are needed to run models associated with and to produce Figure 2, 3, & 4.

**nutnet_cumulative_time_cover.csv** Price equation partition outputs using per species cover as a response. Used for Figure S4.

**FOLDER: Model Fits** The data listed above are used in statistical models, then model objects are produced with scripts in *cluster* folder and saved and provided here. 
- *3* -> Model fits used in main analyses, named after each response
- *cover* -> For Figure S4
- *multi* -> multivariate models (see more below)

**FOLDER: Model Extract** Compressed folder containing data extracted from model objects (Script 5-7) for use in analyses (script 8 on wards)

More data produced within workflow to produce figures is also provided but can be derived from these main data sets.

### **R Scripts** 
R Script file names- which are listed below, are numbered and listed in the order they should be used. Price equation partitions and statistical models are run on the cluster, which means this workflow can not be perfectly run through continuously on a local machine. However, code from cluster scripts can be used pedagogically or be adapted to run on a local machine, just be aware they take a few hours each and require substantial computation power.

**1_Data_Prep.R** This script cleans data to living herbaceous vascular species only and estimates per species biomass according to total plot biomass and life form group (graminoid, forb, legume etc.). Original data associated with this code not provided.

**2_Data_Prep.R** This workflow takes cleaned, filtered data (woody and non-vascular species removed) per species biomass estimate data and calculates measures of species richness. Original data associated with this code not provided.

**3_Data_Prep.R** This preps data for price equation comparisons, which just pairs data into temporal subsets for every site for meaningful comparisons and outputs subsets of the data in pairs

**FOLDER: cluster -> price_pairs**
These scripts are run on an HPC and not a local machine. Uses package [*PriceTools* ](https://github.com/ctkremer/priceTools/) developed by [Colin Kremer](https://scholar.google.com/citations?user=BRbxQwwAAAAJ&hl=en).
- *Price_Pairs_Wrapper.sh* This is a wrapper script to submit multiple jobs for every subset to the cluster
- *Price_Pairs_Submit.sh* This is the submit script to submit each job to the cluster
- *Price_Pairs.R* This is the R script that runs in the cluster iteratively over every subset of data

**4_Data_Prep.R** This workflow uses the data produced by the script 'Price_Pairs.R", which is submitted to the cluster by wrapper submit script, "Price_Pairs_Wrapper.sh" and nested Submit Script "Price_Pairs_Submit.sh". In this script we prune the pairwise comparisons down to only the meaningful temporal pairings that we are interested in.

**FOLDER: cluster -> univariate models, multivariate models**
Here you will find a *A submit script ('.sh')* to send each R script to the HPC cluster, and an *R Script ('.R')* *for every statistical model, named intuitively after each response*
- *univariate models* -> models for main analysis, main results reported in Table S2, model specifications in Table S3, fits Figure S6 a)-g).
- *multivariate models* -> multivariate models explore correlations between responses. Specifications in Table S4, correlations in Table S5, fits in Figure S i)-n)

**5_Model_Data_Extract.R** This workflow pulls data out of *Richness & Biomass* model objects and preps data for visualization in Figure Figure S5 a), b) & c). Models have been run on a cluster and each have an R script and a submit script (.sh) (see folder above). Produces Fig S6 a)-b).

**6_Model_Data_Extract.R** This workflow pulls data out of *Price equation partitions* and *species loss and species gains* model objects and preps data for visualization in Figure 3 & Figure S5c-h. Models have been run on a cluster and each have an R script and a submit script (.sh). Produces Fig S6 c)-n)

**7_Model_Data_Posteriors.R** This workflow pulls data out of posterior distributions from *Price equation partitions* model objects and prepares it for: Figure 2 & 3 inset plots, Figure 2, Figure S5, & Shiny App, Also produces 'quadrant categories' used in several Figures and in Table S1.

**8_Figure_2.R** This script produces Figure 2 a),- e)

**9_Figure_3**  The script produces Figure 3 a) - e).

**10_Figure_4.R** This script produces Figure 4 a) - b).

**11_Supplementary_Figures.R** This script produces Figure S1, S2, S9.

**12_Supplementary_Figure_S3_4** This script produces Figure S3 & S4

**13_Supplementary_Figure_S6.R** This script produces Figure S5.

**14_Supplementary_Figure_S7.R** This script produces Figure S7.

**15_Supplementary_Figure_S8** This script produces Figure S8.

**16_Supplementary_Figure_S10** This script produces Figure S10.

**17_Supplementary_Figure_S11** This script produces Figure S11.

**16_Supplementary_Tables** This script produces Tables S1, S2 & Shiny App Table.


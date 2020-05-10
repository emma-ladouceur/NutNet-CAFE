##### COMPOSITIONAL ANALYSIS
## 'Illusion of control' manuscript
## JD Bakker et al
## Based on analyses from several NutNet workshops and other efforts
## 191213

## Notes:
## Partition each site separately and then compare across sites to correlate with site variables
## Focus on unfenced plots
## Compare Yr0 (pretreatment) through Yr3

# 0.0 Set working directory ----
setwd(choose.dir()) # choose root folder that contains 'Taxonomic.Adjustments.function.191115.R' and subfolders for 'data' and 'graphs'

# 1.0 LOAD ITEMS -----------------------------------------------------------
# 1.1 Load packages ----
library(plyr)
library(labdsv)
library(vegan)
library(tidyverse)
library(geometry)
library(stringr)
library(ggfortify)
library(emmeans)
library(lme4)
library(lmerTest)
library(ggdendro)
library(betapart)
library(maps)
library(multcomp)
library(GGally)

# 1.2 Load functions ---------------------------------------------------------------
CV <-function(x) 100 * (sd(x,na.rm=TRUE) / mean(x,na.rm=TRUE))

# Function to tally species in a site (taxon x year matrix)
spplist <- function(datafile = datafile, site_code = site_code) {
  temp <- matrify(ddply(datafile[datafile$site_code == site_code,], .(Taxon, year), summarize, N = length(max_cover > 0)))
  temp <- merge(x = temp, by.x = "row.names", all.x = TRUE,
                y = unique(datafile[,c("Taxon", "Family", "live")]), by.y = "Taxon", all.y = FALSE)
  temp <- temp[order(temp$Family, temp$Row.names), ]
  temp
}

# Function to tally species in a site (taxon x block matrix)
spplist2 <- function(datafile = datafile, site_code = site_code) {
  temp <- matrify(ddply(datafile[datafile$site_code == site_code,], .(Taxon, block), summarize, N = length(max_cover > 0)))
  temp <- merge(x = temp, by.x = "row.names", all.x = TRUE,
                y = unique(datafile[,c("Taxon", "Family", "live")]), by.y = "Taxon", all.y = FALSE)
  temp <- temp[order(temp$Family, temp$Row.names), ]
  temp
}

# Function to tally plots in a site (block x year matrix)
plotlist <- function(datafile = datafile, site_code = site_code) {
  temp <- matrify(ddply(datafile[datafile$site_code == site_code,], .(plot, year), summarize, 
                        S = length(max_cover > 0)))
  temp <- merge(x = unique(datafile[datafile$site_code == site_code, c("block", "plot", "subplot", "trt")]),
                by.x = "plot", all.x = FALSE, y = temp, by.y = "row.names", all.y = TRUE)
  temp <- temp[order(temp$block, temp$plot), ]
  temp[temp == 0] <- "."
  temp
}

# Function to conduct stepAIC for a response variable
stepAIC.function <- function(data = data, y = y) {
  stepAIC(object = lm(data[[y]] ~ 1, data = data),
          scope = list(upper = ~ total_mass + total_mass_CV + log.S + ANN_TEMP_RANGE + MAP,
                       lower = ~ 1),
          direction = "both", k = log(nrow(data)))
}
#currently removed MAT, TEMP_VAR, TEMP_WET_Q, MAP_VAR, management, N_Dep

# Function to conduct stepAIC for a response variable (with all explanatory variables scaled)
stepAIC.function2 <- function(data = data, y = y) {
  stepAIC(object = lm(data[[y]] ~ 1, data = data),
          scope = list(upper = ~ scale(total_mass) + scale(total_mass_CV) + scale(log.S) + scale(ANN_TEMP_RANGE) + scale(MAP),
                       lower = ~ 1),
          direction = "both", k = log(nrow(data)))
}
#currently removed MAT, TEMP_VAR, TEMP_WET_Q, MAP_VAR, management, N_Dep

# Function to create biplot from PCA (with color-coded arrows)
pca.plot <- function(pca.object, x = x, y = y, expansion = 1, cutoff = 0.3, lab.x.title = NULL, lab.y.title = NULL, source.colours = source.colours) {
  PCAvalues <- pca.object$scores
  PCAloadings <- data.frame(pca.object$loadings[ , c(x, y)])
  PCAloadings.max <- apply(abs(PCAloadings), 1, FUN = max)
  PCAloadings <- PCAloadings[PCAloadings.max > cutoff, ]
  PCAloadings$source <- rownames(PCAloadings)
  ggplot(data = PCAvalues, aes_string(x = x, y = y)) +
    geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = PCAloadings[ , x] * expansion, 
                                         yend = PCAloadings[ , y] * expansion, colour = source),
                 arrow = arrow(length = unit(1/2, "picas"))) +
    geom_point(size = 1.5, alpha = 0.5, shape = 21, colour = "black", fill = "dark grey") +
    annotate("text", x = PCAloadings[ , x] * (1.1 * expansion), y = PCAloadings[ , y] * (1.1 * expansion), label = rownames(PCAloadings), size = 3) +
    theme_bw(base_size = 10) +
    scale_colour_manual(values = source.colours, guide = FALSE) +
    labs(x = lab.x.title, y = lab.y.title)# +
  #    coord_fixed()
}

# Function to create violin plots for subsets (All, Control, Pretreatment)
subset.plot <- function(data = data, x = x, y = y, lab.x.title = "Subset", lab.y.title = NULL) {
  ggplot(data = data, aes_string(x = x, y = y)) + 
    geom_violin(colour = "grey50", draw_quantiles = c(0.5)) + 
    geom_jitter(width = 0.1, height = 0, size = 1, shape = 21, colour = "black", fill = "dark grey") +
    annotate("text", x = subset.letters$Subset, y = subset.letters$y, label = subset.letters$letters, size = 4) +
    theme_bw(base_size = 10) + labs(x = lab.x.title, y = lab.y.title)
}

# Function to create scatter plot relating dissimilarity metric (y) to an explanatory variable (x) and species richness (lines)
scatter.plot <- function(data.actual, data.pred, x = x, y = y, S = S, lab.x.title = NULL, lab.y.title = NULL, y.lims = c(0,1)) {
  ggplot(data = data.actual, aes_string(x = x, y = y)) +
    geom_line(data = data.pred, aes(linetype = factor(S)), size = 1) +
    geom_point(aes(fill = log(S)), size = 1.25, shape = 21, colour = "black") +
    scale_fill_gradient(low = "white", high = "black", guide = FALSE) +
    guides(linetype = FALSE) +
    lims(y = y.lims) +
    theme_bw(base_size = 10) + labs(x = lab.x.title, y = lab.y.title) 
}

# Function to save graphic to standard 2x3" size
standard.ggsave <- function(filename = filename, height = 1.85, width = 3) {
  ggsave(filename, height = height, width = width, units = "in", dpi = 600)
}

# Color scheme for sources of variation
source.colours <- c("Block" = "darkblue", "Year" = "firebrick", "Nutrient" = "goldenrod",
                    "Block.Year" = "forest green", "Block.Nutrient" = "darkorchid4", "Year.Nutrient" = "darkorange2",
                    "Block.Year.Nutrient" = "grey", 
                    "B" = "darkblue", "Y" = "firebrick", "N" = "goldenrod",
                    "BxY" = "forest green", "BxN" = "darkorchid4", "YxN" = "darkorange2", "BxYxN" = "grey")

# Function to conduct site-specific taxonomic adjustments
source("Taxonomic.Adjustments.function.191115.R")

# 1.3 Load data ----
original.triplet <- read.csv("data/full-cover-01-November-2019.csv", header = T) # 224975 x 18
triplet <- original.triplet # backup

original.site.covars <- read.csv("data/comb-by-plot-clim-soil-diversity-01-Nov-2019.csv") # 18600 x 90
site.covars.orig <- original.site.covars

map.dat <- read.csv("data/sites-02-August-2019.csv", header = TRUE)  #140 x 12

#treatment codes
trts <- data.frame(trt = c("Control", "N", "P", "K", "PK", "NK", "NP", "NPK", "Fence", "NPK+Fence"),
                   N = c("No", "Yes", "No", "No", "No", "Yes", "Yes", "Yes", "No", "Yes"),
                   P = c("No", "No", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "Yes"),
                   K = c("No", "No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes"),
                   Fence = c("No", "No", "No", "No", "No", "No", "No", "No", "Yes", "Yes"),
                   Num.Fert = c(0, 1, 1, 1, 2, 2, 2, 3, 0, 3))

# 2.0 PRE-PROCESSING OF COVER DATA ------------------------------------------------------------
# 2.1 Choose sites ----

#sevi.us: plots completely randomly assigned to treatments. Verified with S. Collins on 181024.
#Assigning plots to 5 'pseudo-blocks' (as contiguous as feasible) for analysis.
site.covars.orig$block[site.covars.orig$site_code == "sevi.us" & site.covars.orig$plot %in% c(2, 3, 4, 12, 13, 17, 21, 26)] <- 2
site.covars.orig$block[site.covars.orig$site_code == "sevi.us" & site.covars.orig$plot %in% c(5, 8, 9, 10, 15, 19, 20, 30)] <- 3
site.covars.orig$block[site.covars.orig$site_code == "sevi.us" & site.covars.orig$plot %in% c(14, 18, 23, 24, 25, 31, 34, 35)] <- 4
site.covars.orig$block[site.covars.orig$site_code == "sevi.us" & site.covars.orig$plot %in% c(28, 29, 33, 36, 37, 38, 39, 40)] <- 5
triplet$block[triplet$site_code == "sevi.us" & triplet$plot %in% c(2, 3, 4, 12, 13, 17, 21, 26)] <- 2
triplet$block[triplet$site_code == "sevi.us" & triplet$plot %in% c(5, 8, 9, 10, 15, 19, 20, 30)] <- 3
triplet$block[triplet$site_code == "sevi.us" & triplet$plot %in% c(14, 18, 23, 24, 25, 31, 34, 35)] <- 4
triplet$block[triplet$site_code == "sevi.us" & triplet$plot %in% c(28, 29, 33, 36, 37, 38, 39, 40)] <- 5

## drop observational sites and those with < 3 years of post-treatment data
siteyear <- ddply(triplet, . (site_code), summarize,
                  years = length(unique(year_trt)),
                  min_year_trt = min(unique(year_trt)),
                  max_year_trt = max(unique(year_trt)),
                  blocks = length(unique(block)))
exptsites.all <- siteyear[siteyear$years > 1 & siteyear$min_year_trt == 0 &
                            siteyear$max_year_trt >= 3, ] # select sites with 3 years of post-treatment data (n = 62)

#drop sites with too few blocks or with missing data
exptsites.sub <- exptsites.all
#exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "sval.no",] #2016: year_trt = 999999?
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "azi.cn",] #no Yr3 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "badlau.de",] #plot 5 missing Yr3 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "barta.us",] #no Yr3 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "bldr.us",] #only two blocks
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "bnch.us",] #missing two plots in Yr2
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "doane.us",] #only one block has Yr 1-3 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "hopl.us",] #plots 6, 16, 26 missing Yr0 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "lake.us",] #plot 5 missing Yr3 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "mcla.us",] #plot 14 missing Yr2 data
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "pape.de",] #only one block
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "sava.us",] #only two blocks
exptsites.sub <- exptsites.sub[exptsites.sub$site_code != "sgs.us",] #multiple control in Yr0 but other trts missing that yr (verified with D. Blumenthal on 140718)

# 2.2 Taxonomic QC ----
data1.all <- Taxonomic.Adjustments(datafile = triplet[triplet$site_code %in% exptsites.all$site_code , ]) # 171323 x 9
# code includes some notes about sites with too few blocks or missing data (most of which have not had taxonomic QC)

# 2.3 Choose which treatment(s) and years to focus on ----
data1.all <- merge(x = data1.all, all.x = TRUE, y = trts)

data1.all <- data1.all[! data1.all$trt %in% c("Fence", "NPK+Fence"), ]
data1.all <- data1.all[data1.all$year_trt %in% c(0, 1, 2, 3), ]

#skip to include all sites:
data1.sub <- data1.all[data1.all$site_code %in% exptsites.sub$site_code, ] #65489 x 14

# 2.4 Site-specific adjustments - drop individual blocks, plots ----
#skip section to include all plots and blocks

#cbgb.us: six blocks. Verified with L. Biederman on 170324. Delete blocks 4-6
data1.sub <- with(data1.sub, data1.sub[!(site_code == "cbgb.us" & block %in% c(4, 5, 6)), ])

#cdcr.us: five blocks (delete 4-5)
data1.sub <- with(data1.sub, data1.sub[!(site_code == "cdcr.us" & block %in% c(4, 5)), ])

#cdpt.us: six blocks. On 170802, J. Knops suggested grouping blocks 1, 5, 6 (different) and 2, 3, 4 (similar). Delete blocks 1, 5, 6
data1.sub <- with(data1.sub, data1.sub[!(site_code == "cdpt.us" & block %in% c(1, 5, 6)), ])

#ethass.au: add four plot-years with no living plants (11_2013, 28_2013, 28_2014, 5_2014)
temp <- with(data1.sub, data1.sub[site_code == "ethass.au" &
                                    ((plot %in% c(11, 28) & year == 2015 & Taxon == "ARISTIDA HOLATHERA") | 
                                       (plot %in% c(5, 28) & year == 2016 & Taxon == "TRIODIA BASEDOWII")) , ])
temp$Taxon <- "dummy"; temp$max_cover <- 0.001
temp$year <- temp$year - 2
data1.sub <- rbind(data1.sub, temp)

#kbs.us: five blocks (delete 4, 5)
data1.sub <- with(data1.sub, data1.sub[!(site_code == "kbs.us" & block %in% c(4, 5)), ])

#kilp.fi: four blocks (delete 4)
data1.sub <- with(data1.sub, data1.sub[!(site_code == "kilp.fi" & block %in% c(4)), ])

#koffler.ca: 3 control plots in each block; different subplot for plot 19 in 2014
#focus on first control plot in each block. Verified with M. Cadotte on 170324.
data1.sub <- with(data1.sub, data1.sub[!(site_code == "koffler.ca" & plot %in% c(9, 11, 17, 21, 34, 36)), ])

#marc.ar: 3 control plots in blocks 1 and 2. Verified with J. Alberti on 170324.
data1.sub <- with(data1.sub, data1.sub[!(site_code == "marc.ar" & plot %in% c(6, 8, 11, 17)), ])

#mtca.au: four blocks (delete 4). Verified with S. Prober on 170324.
data1.sub <- with(data1.sub, data1.sub[!(site_code == "mtca.au" & block %in% c(4)), ])

#saana.fi: four blocks (delete 4)
data1.sub <- with(data1.sub, data1.sub[!(site_code == "saana.fi" & block %in% c(4)), ])

#sedg.us: 2 control, 2 NPK (no fences) in each block
data1.sub <- with(data1.sub, data1.sub[!(site_code == "sedg.us" & plot %in% c(7, 10, 17, 18, 27, 28)), ])

#sevi.us: 3 of 5 'pseudo-blocks' (as contiguous as feasible) - using those farthest away from one another
data1.sub <- with(data1.sub, data1.sub[!(site_code == "sevi.us" & block %in% c(2, 4)), ])

#shps.us: four blocks (delete 4?)
data1.sub <- with(data1.sub, data1.sub[!(site_code == "shps.us" & block %in% c(4)), ])

#sier.us: five blocks (delete 4-5?); no Yr0 data for blocks 4-5
data1.sub <- with(data1.sub, data1.sub[!(site_code == "sier.us" & block %in% c(4, 5)), ])

#summ.za: 3 control (no fences) in each block; one control measured only in Yr0 and another not measured in Yr2
data1.sub <- with(data1.sub, data1.sub[!(site_code == "summ.za" & plot %in% c(1, 10, 15, 16, 21, 30)), ])

#temple.us: drop extra plots in block 2
data1.sub <- with(data1.sub, data1.sub[!(site_code == "temple.us" & plot %in% c(19, 20)), ])

#ukul.za: 2 control, 2 NPK (no fences) in each block
data1.sub <- with(data1.sub, data1.sub[!(site_code == "ukul.za" & plot %in% c(8, 10, 19, 20, 25, 30)), ])

#yarra.au: four blocks (delete 4)
data1.sub <- with(data1.sub, data1.sub[!(site_code == "yarra.au" & block %in% c(4)), ])


# 2.5 Final adjustments to site and plot data -------------------------
data1 <- data1.sub #set to data1.sub for balanced subset of sites

data1$site_code <- factor(data1$site_code)
data1$year_trt <- factor(data1$year_trt, ordered = TRUE, levels = c(0, 1, 2, 3))
data1$block <- factor(data1$block)
data1$plot <- factor(data1$plot)
data1$UBI <- do.call(paste, c(data1[c("site_code", "block")], sep = "_"))
data1$UPI <- do.call(paste, c(data1[c("site_code", "block", "plot")], sep = "_"))
data1$UPYI <- do.call(paste, c(data1[c("UPI", "year")], sep = "_"))
data1$STYI <- do.call(paste, c(data1[c("site_code", "trt", "year_trt")], sep = "_"))
data1$SNYI <- do.call(paste, c(data1[c("site_code", "N", "year_trt")], sep = "_"))

sites <- factor(unique(data1$site_code)) # 50 sites
UBIs <- unique(data1$UBI) # 150 blocks
UPIs <- unique(data1$UPI) # 1200 UPIs
UPYIs <- unique(data1$UPYI) # 4800 UPYIs
STYIs <- unique(data1$STYI) # 1600 site-trt-year combinations

#Commented code here creates a summary table for each site based just on plots included in this analysis
#sites.data1 <- unique(data1$site_code)
#for(i in 1:length(sites.data1)) {
#  write.csv(spplist(datafile = data1, site_code = sites.data1[i]), file = paste(as.character(sites.data1[i]), ".csv", sep = ""))
#}

# 3.0 PRE-PROCESS SITE-LEVEL COVARIATES -------------------------------------
# subset to selected sites
site.covars.orig$UPYI <- do.call(paste, c(site.covars.orig[c("site_code", "block", "plot", "year")], sep = "_"))
site.covars.sub <- site.covars.orig[site.covars.orig$UPYI %in% UPYIs, ]

# calculate management index
site.covars <- ddply(site.covars.sub[site.covars.sub$year_trt == 0, ], .(site_code), summarize, habitat = unique(habitat),
                     country = unique(country), managed = unique(managed), burned = unique(burned), grazed = unique(grazed),
                     anthropogenic = unique(anthropogenic), habitat = unique(habitat), elevation = unique(elevation))
site.covars$management <- with(site.covars, managed + burned + grazed + anthropogenic) #combine multiple types of management
site.covars$management[site.covars$management > 1] <- 1
site.covars$management <- as.factor(site.covars$management)

# calculate productivity covariate using control and pretreatment data
# (not all sites have pretreatment biomass data)
site.covars.sub$massQC <- with(site.covars.sub,
                               ifelse(is.na(litter_mass) & is.na(standing_dead_mass) & is.na(live_mass) & is.na(unsorted_mass), 1, 0))
site.covars.sub$total_mass <- with(site.covars.sub,
                                   rowSums(site.covars.sub[,c("litter_mass", "standing_dead_mass", "live_mass", "unsorted_mass")], na.rm=TRUE))
site.covars.sub$total_mass <- with(site.covars.sub, ifelse(massQC == 1, NA, total_mass))
site.covars.sub$total_mass[site.covars.sub$site_code == "jena.de" & site.covars.sub$year_trt > 0] <- NA # drop because of difference in sampling intensity
pre.con.mass <- ddply(site.covars.sub[(site.covars.sub$trt == "Control" | site.covars.sub$year_trt == 0 ) , ], .(site_code),
                      summarize, Number = length(total_mass[! is.na(total_mass)]),
                      total_mass_CV = CV(total_mass), total_mass = mean(total_mass, na.rm = TRUE))

# climate variables
bioclim.vars <- ddply(site.covars.sub, .(site_code), summarize,
                      MAT = unique(MAT_v2), MAT_RANGE = unique(MAT_RANGE_v2), ISO = unique(ISO_v2),
                      TEMP_VAR = unique(TEMP_VAR_v2), MAX_TEMP = unique(MAX_TEMP_v2), MIN_TEMP = unique(MIN_TEMP_v2),
                      ANN_TEMP_RANGE = unique(ANN_TEMP_RANGE_v2), TEMP_WET_Q = unique(TEMP_WET_Q_v2),
                      TEMP_DRY_Q = unique(TEMP_DRY_Q_v2), TEMP_WARM_Q = unique(TEMP_WARM_Q_v2),
                      TEMP_COLD_Q = unique(TEMP_COLD_Q_v2), MAP = unique(MAP_v2), MAP_WET_M = unique(MAP_WET_M_v2),
                      MAP_DRY_M = unique(MAP_DRY_M_v2), MAP_VAR = unique(MAP_VAR_v2), MAP_WET_Q = unique(MAP_WET_Q_v2),
                      MAP_DRY_Q = unique(MAP_DRY_Q_v2), MAP_WARM_Q = unique(MAP_WARM_Q_v2), MAP_COLD_Q = unique(MAP_COLD_Q_v2),
                      RAIN_PET = unique(RAIN_PET), AI = unique(AI), PET = unique(PET), N_Dep = unique(N_Dep))

# calculate site-level species richness (gamma diversity)
temp <- ddply(data1, .(site_code, Taxon), summarize, max_cover = sum(max_cover))
S <- ddply(temp, .(site_code), summarize, S = length(Taxon))

# combine all site-level covariates together
site.covars <- merge(x = site.covars, y = pre.con.mass) %>%
  merge(y = bioclim.vars[ , c("site_code", "MAT", "TEMP_VAR", "ANN_TEMP_RANGE", "TEMP_WET_Q", "MAP", "MAP_VAR", "N_Dep")]) %>%
  merge(y = S)
site.covars$log.S <- log(site.covars$S)


# 4.0 DATA PROCESSING -----------------------------------------------------------
# 4.1 Create Plot-Year (UPYI) x Species matrix ----
spp.data <- matrify(data1[ , c("UPYI", "Taxon", "max_cover")]) # 4800 x 1563; species data for analysis

#add dummy variable with low cover
spp.data$dummy <- 0.001

# calculate plot-level species richness
S.data <- (rowSums(spp.data > 0) - 1)

#order by plot and year
spp.data <- spp.data[ order(rownames(spp.data)), ]

data <- unique(data1[, c("STYI", "UPYI", "UPI", "UBI", "year", "site_code", "block", "plot", "year_trt", "trt", "N", "P", "K", "Num.Fert")])
data <- data[which(data$UPYI %in% rownames(spp.data)), ]
data <- data[ order(rownames(spp.data)), ]

# 4.2 Calculate various dissimilarity matrices and extract SS ----
data.sub <- data[data$site_code %in% exptsites.sub$site_code , ]
part.distances <- data.frame()
site.summary <- data.frame()
subset.summary <- data.frame()
spatial.summary <- data.frame() # for looking at spatial variation over time (bray only)
treatment.summary <- data.frame() # for looking at temporal variation within treatments (bray only)
for(i in 1:length(sites[sites %in% unique(exptsites.sub$site_code)])) {
  print(i)
  temp <- c()
  site.data <- data[data$site_code == sites[sites %in% unique(exptsites.sub$site_code)][i], ]
  site.data <- site.data[ order(site.data$UPYI), ]
  site.spp.data <- spp.data[rownames(spp.data) %in% unique(site.data$UPYI), ]
  site.spp.data <- site.spp.data[ order(rownames(site.spp.data)), colSums(site.spp.data) > 0]
  
  #Abundance-based dissimilarity
  temp <- bray.part(site.spp.data)
  bray.diss <- as.matrix(temp$bray)
  prop.bray.bal.diss <- with(temp, as.matrix(bray.bal / bray))
  prop.bray.bal.diss[is.na(prop.bray.bal.diss)] <- 0
  
  #Incidence-based dissimilarity
  temp.pa <- beta.pair(decostand(site.spp.data, "pa"))
  sor.pa.diss <- as.matrix(temp.pa$beta.sor)
  prop.sim.pa.diss <- with(temp.pa, as.matrix(beta.sim / beta.sor))
  prop.sim.pa.diss[is.na(prop.sim.pa.diss)] <- 0  
  
  permanova.bray <- adonis(bray.diss ~ (block + year_trt + trt)^2, data = site.data, permutations = 9, method = "euc")
  permanova.prop.bal <- adonis(prop.bray.bal.diss ~ (block + year_trt + trt)^2, data = site.data, permutations = 9, method = "euc")
  permanova.sor <- adonis(sor.pa.diss ~ (block + year_trt + trt)^2, data = site.data, permutations = 9, method = "euc")
  permanova.prop.sim <- adonis(prop.sim.pa.diss ~ (block + year_trt + trt)^2, data = site.data, permutations = 9, method = "euc")
  
  temp.bray <- data.frame(term = c(attr(permanova.bray$terms, "term.labels"), "residual", "total"),
                          df = as.numeric(permanova.bray$aov.tab$Df),
                          SS_bray = as.double(permanova.bray$aov.tab$SumsOfSqs),
                          SS_prop_bal = as.double(permanova.prop.bal$aov.tab$SumsOfSqs),
                          SS_sor = as.double(permanova.sor$aov.tab$SumsOfSqs),
                          SS_prop_sim = as.double(permanova.prop.sim$aov.tab$SumsOfSqs))
  temp.bray$site_code <- as.character(sites[i])
  part.distances <- rbind(part.distances, temp.bray)
  print(as.character(sites[i]))
  
  # site averages (all plot-years)
  temp.site <- data.frame(site_code = as.character(sites[i]),
                     bray = mean(temp$bray),
                     bray.bal = mean(temp$bray.bal),
                     bray.gra = mean(temp$bray.gra),
                     prop_bal = mean(as.dist(prop.bray.bal.diss)),
                     sor = mean(temp.pa$beta.sor),
                     sor.turn = mean(temp.pa$beta.sim),
                     sor.nest = mean(temp.pa$beta.sne),
                     prop_sim = mean(as.dist(prop.sim.pa.diss)))
  site.summary <- rbind(site.summary, temp.site)

  # site averages (subsets of plot-years)
  #index each control plot separately
  control.plots <- unique(site.data$UPI[site.data$trt == "Control"])
  control.plot.dists <- data.frame()
  for(j in 1:length(control.plots)) {
    temp.dists <- data.frame(site_code = as.character(sites[i]),
                             UPI = control.plots[j],
                             control.bray.dist = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]],
                                                                         colnames(bray.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]]])),
                             control.prop_bal.dist = mean(as.dist(prop.bray.bal.diss[row.names(prop.bray.bal.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]],
                                                                             colnames(prop.bray.bal.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]]])),
                             control.sor.dist = mean(as.dist(sor.pa.diss[row.names(sor.pa.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]],
                                                                                 colnames(sor.pa.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]]])),
                             control.prop_sim.dist = mean(as.dist(prop.sim.pa.diss[row.names(prop.sim.pa.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]],
                                                                                      colnames(prop.sim.pa.diss) %in% site.data$UPYI[site.data$UPI == control.plots[j]]])))
    control.plot.dists <- rbind(control.plot.dists, temp.dists)
  }
  
  # site averages (each treatment separately)
  #index each plot within each treatment separately
  treatments <- unique(site.data$trt)
  treatment.dists <- data.frame()
  for(j in 1:length(treatments)) {
    treatment.plots <- unique(site.data$UPI[site.data$trt == treatments[j]])
    temp.dists <- c()
    for(k in 1:length(treatment.plots)) {
      temp.dists <- rbind(temp.dists, 
                          data.frame(UPI = treatment.plots[k],
                                     bray.dist = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$UPI == treatment.plots[k]],
                                                                        colnames(bray.diss) %in% site.data$UPYI[site.data$UPI == treatment.plots[k]]]))))
    }
    temp.treatment.summary <- data.frame(site_code = as.character(sites[i]), 
                                         trt = treatments[j],
                                         mean.bray.dist = mean(temp.dists$bray.dist),
                                         N.dists = length(temp.dists$bray.dist))
    treatment.summary <- rbind(treatment.summary, temp.treatment.summary)
  }
  
  #combine overall, pretreatment, and control subsets
  temp.subset <- data.frame(site_code = as.character(sites[i]),
                          All.bray = mean(as.dist(temp$bray)),
                          Pretreatment.bray = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$year_trt == 0],
                                                             colnames(bray.diss) %in% site.data$UPYI[site.data$year_trt == 0]])),
                          Control.bray = mean(control.plot.dists$control.bray.dist),
                          All.prop_bal = mean(as.dist(prop.bray.bal.diss)),
                          Pretreatment.prop_bal = mean(as.dist(prop.bray.bal.diss[row.names(prop.bray.bal.diss) %in% site.data$UPYI[site.data$year_trt == 0],
                                                                                  colnames(prop.bray.bal.diss) %in% site.data$UPYI[site.data$year_trt == 0]])),
                          Control.prop_bal = mean(control.plot.dists$control.prop_bal.dist),
                          All.sor = mean(as.dist(sor.pa.diss)),
                          Pretreatment.sor = mean(as.dist(sor.pa.diss[row.names(sor.pa.diss) %in% site.data$UPYI[site.data$year_trt == 0],
                                                                      colnames(sor.pa.diss) %in% site.data$UPYI[site.data$year_trt == 0]])),
                          Control.sor = mean(control.plot.dists$control.sor.dist),
                          All.prop_sim = mean(as.dist(prop.sim.pa.diss)),
                          Pretreatment.prop_sim = mean(as.dist(prop.sim.pa.diss[row.names(prop.sim.pa.diss) %in% site.data$UPYI[site.data$year_trt == 0],
                                                                                colnames(prop.sim.pa.diss) %in% site.data$UPYI[site.data$year_trt == 0]])),
                          Control.prop_sim = mean(control.plot.dists$control.prop_sim.dist))
  subset.summary <- rbind(subset.summary, temp.subset)

  #combine overall and spatial subsets
  temp.subset <- data.frame(site_code = as.character(sites[i]),
                            All.bray = mean(as.dist(temp$bray)),
                            Pretreatment.bray = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$year_trt == 0],
                                                                       colnames(bray.diss) %in% site.data$UPYI[site.data$year_trt == 0]])),
                            Y1.bray = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$year_trt == 1],
                                                                       colnames(bray.diss) %in% site.data$UPYI[site.data$year_trt == 1]])),
                            Y2.bray = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$year_trt == 2],
                                                                       colnames(bray.diss) %in% site.data$UPYI[site.data$year_trt == 2]])),
                            Y3.bray = mean(as.dist(bray.diss[row.names(bray.diss) %in% site.data$UPYI[site.data$year_trt == 3],
                                                                       colnames(bray.diss) %in% site.data$UPYI[site.data$year_trt == 3]])))
  spatial.summary <- rbind(spatial.summary, temp.subset)
  
  }
rownames(site.summary) <- site.summary$site_code
site.summary <- merge(x = site.summary, y = site.covars)
subset.summary <- merge(x = subset.summary, y = site.covars)

# 5.0 ANALYSIS -------------------------------------------------
# 5.0.1 Compare All vs spatial variation in different years ----
bray.summary.spatial <- merge(x = spatial.summary, y = site.covars) %>%
  gather(All.bray, Pretreatment.bray, Y1.bray, Y2.bray, Y3.bray, key = Subset, value = bray) %>%
  separate(Subset, into = c("Subset", "measure")) %>%
  dplyr::select(c(site_code, Subset, bray))
summary(lmer(bray ~ Subset + (1 | site_code), data = bray.summary.spatial[bray.summary.spatial$Subset != "All" , ]))

ggplot(data = bray.summary.spatial, aes(x = Subset, y = bray)) + 
  geom_violin(colour = "grey50", draw_quantiles = c(0.5)) + 
  geom_jitter(width = 0.1, height = 0, size = 1, shape = 21, colour = "black", fill = "dark grey") +
  lims(y = c(0, 0.81)) + theme_bw(base_size = 10) + labs(x = "Spatial Subset", y = "")
ggsave("graphs/Figure.S4A.all.years.png", width = 3.6, height = 3, units = "in", dpi = 600)

# 5.0.2 Compare All vs treatments over time ----
All.summary <- data.frame(site_code = spatial.summary$site_code, 
                          trt = "All",
                          mean.bray.dist = spatial.summary$All.bray,
                          N.dists = 4560)
Treatment.summary <- treatment.summary %>%
  merge(y = All.summary, all = TRUE) %>%
  mutate(trt = factor(trt, ordered = TRUE, levels = c("All", "Control", "N", "P", "K", "NP", "NK", "PK", "NPK")))
summary(lmer(mean.bray.dist ~ as.factor(as.character(trt)) + (1 | site_code), data = Treatment.summary[Treatment.summary$trt != "All" , ]))

ggplot(data = Treatment.summary, aes(x = trt, y = mean.bray.dist)) + 
  geom_violin(colour = "grey50", draw_quantiles = c(0.5)) + 
  geom_jitter(width = 0.1, height = 0, size = 1, shape = 21, colour = "black", fill = "dark grey") +
  lims(y = c(0, 0.81)) + theme_bw(base_size = 10) + labs(x = "Nutrient Addition Subset", y = "")
ggsave("graphs/Figure.S4B.all.treatments.png", width = 6, height = 3, units = "in", dpi = 600)

# 5.1 Compare subsets (All vs. Pretreatment vs. Control) ----
subset.names <- data.frame(Subset = c("All", "Control", "Pretreatment"),
                           Type = c("All", "Temporal", "Spatial"))

# total distance
bray.summary <- gather(subset.summary, All.bray, Pretreatment.bray, Control.bray, key = Subset, value = bray) %>%
  separate(Subset, into = c("Subset", "measure")) %>%
  dplyr::select(c(site_code, Subset, bray)) %>%
  merge(y = subset.names)
summary(lmer(bray ~ Type + (1 | site_code), data = bray.summary))
cld(glht(lmer(bray ~ Type + (1 | site_code), data = bray.summary), linfct = mcp(Type = "Tukey")), level = 0.05, decreasing = FALSE)
#Every subset different
ddply(bray.summary, .(Type, Subset), summarize, min = min(bray), mean = mean(bray), max = max(bray))

subset.letters <- data.frame(Subset = c(0.75, 1.75, 2.75),
                             y = c(0.22, 0.22, 0.22),
                             letters = c("c", "b", "a"))
subset.plot(data = bray.summary, x = "Type", y = "bray", lab.x.title = "Subset", lab.y.title = "Bray-Curtis Dissimilarity") + lims(y = c(0, 0.8))
standard.ggsave(filename = "graphs/Figure.1A.bray.subset.png")

# presence/absence data
sor.summary <- gather(subset.summary, All.sor, Pretreatment.sor, Control.sor, key = Subset, value = sor) %>%
  separate(Subset, into = c("Subset", "measure")) %>%
  dplyr::select(c(site_code, Subset, sor))  %>%
  merge(y = subset.names)
summary(lmer(sor ~ Type + (1 | site_code), data = sor.summary))
cld(glht(lmer(sor ~ Type + (1 | site_code), data = sor.summary), linfct = mcp(Type = "Tukey")), level = 0.05, decreasing = FALSE)
#Every subset different
ddply(sor.summary, .(Type, Subset), summarize, min = min(sor), mean = mean(sor), max = max(sor))

subset.letters <- data.frame(Subset = c(0.75, 1.75, 2.75),
                             y = c(0.07, 0.07, 0.07),
                             letters = c("c", "b", "a"))
subset.plot(data = sor.summary, x = "Type", y = "sor", lab.x.title = "Subset", lab.y.title = "Sorensen Dissimilarity") + lims(y = c(0, 0.8))
standard.ggsave(filename = "graphs/Figure.1B.sor.subset.png")

# proportion of dissimilarity due to balanced variation in abundance
prop_bal.summary <- gather(subset.summary, All.prop_bal, Pretreatment.prop_bal, Control.prop_bal, key = Subset, value = prop_bal) %>%
  separate(Subset, into = c("Subset", "measure"), extra = "merge") %>%
  dplyr::select(site_code, Subset, prop_bal) %>%
  merge(y = subset.names)
summary(lmer(prop_bal ~ Type + (1 | site_code), data = prop_bal.summary))
cld(glht(lmer(prop_bal ~ Type + (1 | site_code), data = prop_bal.summary), linfct = mcp(Type = "Tukey")), level = 0.05, decreasing = FALSE)
#Temporal different than others
ddply(prop_bal.summary, .(Type, Subset), summarize, min = min(prop_bal), mean = mean(prop_bal), max = max(prop_bal))

subset.letters <- data.frame(Subset = c(0.75, 1.75, 2.75),
                             y = c(58, 58, 58),
                             letters = c("b", "b", "a"))
subset.plot(data = prop_bal.summary, x = "Type", y = "prop_bal * 100", lab.x.title = "Subset", lab.y.title = "Balanced Variation (%)") + lims(y = c(0, 100))
standard.ggsave(filename = "graphs/Figure.1C.prop_bal.subset.png")

# proportion of presence/absence dissimilarity due to species turnover
prop_sim.summary <- gather(subset.summary, All.prop_sim, Pretreatment.prop_sim, Control.prop_sim, key = Subset, value = prop_sim) %>%
  separate(Subset, into = c("Subset", "measure"), extra = "merge") %>%
  dplyr::select(site_code, Subset, prop_sim) %>%
  merge(y = subset.names)
summary(lmer(prop_sim ~ Type + (1 | site_code), data = prop_sim.summary))
cld(glht(lmer(prop_sim ~ Type + (1 | site_code), data = prop_sim.summary), linfct = mcp(Type = "Tukey")), level = 0.05, decreasing = FALSE)
#Temporal different than Spatial and All
ddply(prop_sim.summary, .(Type, Subset), summarize, min = min(prop_sim), mean = mean(prop_sim), max = max(prop_sim))

subset.letters <- data.frame(Subset = c(0.75, 1.75, 2.75),
                             y = c(28, 28, 28),
                             letters = c("b", "b", "a"))
subset.plot(data = prop_sim.summary, x = "Type", y = "prop_sim * 100", lab.x.title = "Subset", lab.y.title = "Species Turnover (%)") + lims(y = c(0, 100))
standard.ggsave(filename = "graphs/Figure.1D.prop_sim.subset.png")

# compare different metrics to one another (Figure 5)
ggpairs(data = summary.all[summary.all$Type == "All",],
        lower = list(continuous = wrap("points", shape = 21, color = "black", fill = "dark grey"), combo = "dot_no_facet"),
        columns = c("bray", "sor", "prop_bal", "prop_sim"),
        columnLabels = c("Bray-Curtis Dissimilarity", "Sorensen Dissimilarity",
                       "Balanced Varation (%)", "Species Turnover (%)")) +
  theme_bw(base_size = 10)
standard.ggsave(filename = "graphs/Figure.5.png", height = 6, width = 6)

# plotting magnitude vs. turnover
summary.all.medians <- summary.all[summary.all$Subset == "All" , ] %>%
  mutate(BC.group = ifelse(bray < median(bray), "Low BC", "High BC")) %>%
  mutate(BalVar.group = ifelse(prop_bal < median(prop_bal), "Low bal var", "High bal var")) %>%
  mutate(BalVar.group2 = ifelse(prop_bal < median(prop_bal), "Low", "High")) %>%
  mutate(Abund.group = paste(BC.group, BalVar.group, sep = "_")) %>%
  mutate(Sor.group = ifelse(sor < median(sor), "Low Sor", "High Sor")) %>%
  mutate(SppTurn.group = ifelse(prop_sim < median(prop_sim), "Low spp turn", "High spp turn")) %>%
  mutate(SppTurn.group2 = ifelse(prop_sim < median(prop_sim), "Low", "High")) %>%
  mutate(Incid.group = paste(Sor.group, SppTurn.group, sep = "_"))
ddply(summary.all.medians, .(Abund.group), summarize, N = length(site_code))
ddply(summary.all.medians, .(Incid.group), summarize, N = length(site_code))
# many sites are not in same quadrant in both types of metrics

ggplot(data = summary.all.medians, aes(x = bray, y = prop_bal * 100)) +
  geom_vline(aes(xintercept = median(bray)), colour = "grey") +
  geom_hline(aes(yintercept = median(prop_bal)*100), colour = "grey") +
  geom_point(aes(fill = bray, shape = BalVar.group2), colour = "black") +
  scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "white", midpoint = median(summary.all.medians$bray)) +
  scale_shape_manual(values = c(24, 25)) +
  theme_bw(base_size = 7) +
  guides(fill = "none", shape = "none") +
  labs(x = "Dissimilarity", y = "Turnover (%)")
ggsave("graphs/Figure.S1A.Abundance.inset.png", width = 1.5, height = 1.5, units = "in", dpi = 600)

ggplot(data = summary.all.medians, aes(x = sor, y = prop_sim * 100)) +
  geom_vline(aes(xintercept = median(sor)), colour = "grey") +
  geom_hline(aes(yintercept = median(prop_sim)*100), colour = "grey") +
  geom_point(aes(fill = sor, shape = SppTurn.group2), colour = "black") +
  scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "white", midpoint = median(summary.all.medians$sor)) +
  scale_shape_manual(values = c(24, 25)) +
  theme_bw(base_size = 7) +
  guides(fill = "none", shape = "none") +
  labs(x = "Dissimilarity", y = "Turnover (%)")
ggsave("graphs/Figure.S1B.Incidence.inset.png", width = 1.5, height = 1.5, units = "in", dpi = 600)


# 5.2 Relate mean compositional dissimilarity to covariates ----

## Table S3 (Fit model for each explanatory variable separately)
#regression - everything but management
site.summary.gather <- gather(site.summary, log.S, total_mass, total_mass_CV, MAT, TEMP_VAR, ANN_TEMP_RANGE, TEMP_WET_Q, MAP, MAP_VAR, N_Dep, key = Variable, value = X) %>%
  gather(bray, prop_bal, sor, prop_sim, key = Response, value = Y) %>%
  dplyr::select(site_code, Variable, X, Response, Y)
individ.models <- dlply(site.summary.gather, .(Response, Variable), function(df) { 
  lm(Y ~ X, data = df)
})
(individ.models.P <- ldply(individ.models, function(x) round(summary(x)$coefficients[2, 4], 4)) %>%
    spread(key = Response, value = V1))

#ANOVA - management
site.summary.gather.management <- gather(site.summary, bray, prop_bal, sor, prop_sim, key = Response, value = Y) %>%
  dplyr::select(site_code, management, Response, Y)
management.models <- dlply(site.summary.gather.management, .(Response), function(df) { 
  aov(Y ~ management, data = df)
})
(management.models.P <- ldply(management.models, function(x) summary.aov(x)[[1]][["Pr(>F)"]][1]))

#combine all P-values in table
tableS3 <- rbind(individ.models.P,
                 c("management", round(t(management.models.P$V1), 4)))
tableS3$row.order <- c(6, 1, 8, 9, 4, 10, 5, 7, 2, 3, 11)
tableS3 <- tableS3[order(tableS3$row.order), c(1, 2, 5, 3, 4)]
write.csv(tableS3, "graphs/TableS3.csv")

## Model selection for each metric
summary(bray.res <- stepAIC.function(data = site.summary, y = "bray")) #total_mass_CV, log.S (r^2 = 0.2075)
summary(sor.res <- stepAIC.function(data = site.summary, y = "sor")) #log.S, ANN_TEMP_RANGE, total_mass_CV (r^2 = 0.4989)
summary(prop_bal.res <- stepAIC.function(data = site.summary, y = "prop_bal")) #log.S, MAP (r^2 = 0.3673)
summary(prop_sim.res <- stepAIC.function(data = site.summary, y = "prop_sim")) #log.S, total_mass, ANN_TEMP_RANGE (r^2 = 0.7088)

summary(scaled.bray.res <- stepAIC.function2(data = site.summary, y = "bray")) #total_mass_CV, log.S (r^2 = 0.2075)
summary(scaled.sor.res <- stepAIC.function2(data = site.summary, y = "sor")) #log.S, ANN_TEMP_RANGE, total_mass_CV (r^2 = 0.4989)
summary(scaled.prop_bal.res <- stepAIC.function2(data = site.summary, y = "prop_bal")) #log.S, MAP (r^2 = 0.3673)
summary(scaled.prop_sim.res <- stepAIC.function2(data = site.summary, y = "prop_sim")) #log.S, total_mass, ANN_TEMP_RANGE (r^2 = 0.7088)

# build single graphic that shows all variables and significant predictors
#combine coefficients from final models
coefficients <- rbind(data.frame(Coef = bray.res$coefficients, 
                                 Explan = row.names(data.frame(bray.res$coefficients)),
                                 Response = "bray"),
                      data.frame(Coef = prop_bal.res$coefficients,
                                 Explan = row.names(data.frame(prop_bal.res$coefficients)),
                                 Response = "prop_bal"),
                      data.frame(Coef = sor.res$coefficients,
                                 Explan = row.names(data.frame(sor.res$coefficients)),
                                 Response = "sor"),
                      data.frame(Coef = prop_sim.res$coefficients,
                                 Explan = row.names(data.frame(prop_sim.res$coefficients)),
                                 Response = "prop_sim"))

r.squared.values <- data.frame(R2 = c(summary(bray.res)$r.squared,
                                      summary(prop_bal.res)$r.squared,
                                      summary(sor.res)$r.squared,
                                      summary(prop_sim.res)$r.squared),
                               Response = c("bray", "prop_bal", "sor", "prop_sim"))

#merge files and flag significant terms
master <- site.summary %>%
  gather(bray, sor, prop_bal, prop_sim, key = Response, value = response) %>%
  gather(log.S, total_mass, ANN_TEMP_RANGE, total_mass_CV, MAP, key = Explan, value = explan) %>%
  dplyr::select(site_code, Response, response, Explan, explan) %>%
  mutate(Response = factor(Response, ordered = TRUE, levels = c("bray", "sor", "prop_bal", "prop_sim"))) %>%
  mutate(Explan = factor(Explan, ordered = TRUE,
                         levels = c("log.S", "ANN_TEMP_RANGE", "MAP", "total_mass", "total_mass_CV")))
sig.codes <- unique(master[,c("Response", "Explan")]) %>%
  merge(y = coefficients, all.x = TRUE, all.y = FALSE)
sig.codes$Sig.AIC <- ifelse(! is.na(sig.codes$Coef), "Sig", "NS")
sig.codes$Sig.AIC <- factor(sig.codes$Sig.AIC, ordered = TRUE, levels = c("Sig", "NS"))
master <- merge(x = master, y = sig.codes)

#adjust based on individual tests (tableS3)
tableS3.triplet <- tableS3 %>%
  gather(bray, sor, prop_bal, prop_sim, key = Response, value = P.value) %>%
  rename(Explan = Variable) %>%
  mutate(P.value = as.numeric(P.value))
master <- master %>%
  merge(y = tableS3.triplet) %>%
  mutate(Sig = ifelse(P.value <= 0.05, "Sig", "NS"))
master$Sig <- factor(master$Sig, ordered = TRUE, levels = c("Sig", "NS"))

master$response[master$Response %in% c("prop_bal", "prop_sim")] <- master$response[master$Response %in% c("prop_bal", "prop_sim")] * 100
                        
response.metrics <- c(bray = "Bray-Curtis Dissimilarity", sor = "Sorensen Dissimilarity",
                      prop_bal = "Balanced Variation (%)", prop_sim = "Species Turnover (%)")
explanatory.variables <- c(log.S = "Gamma Diversity", total_mass = "Productivity", total_mass_CV = "Local Variation",
                           ANN_TEMP_RANGE = "Annual Temp. Range", MAP = "Mean Ann. Precip.")
master.summary <- ddply(master, .(Explan, Response), summarize, explan = mean(explan), response = mean(response), Sig.AIC = max(Sig.AIC))
ggplot(data = master, aes(x = explan, y = response)) +
  geom_rect(data = master.summary, aes(fill = as.factor(Sig.AIC)), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  stat_smooth(aes(linetype = as.factor(Sig)), method = "lm", se = FALSE, size = 1, colour = "black") +
  geom_point(shape = 21, colour = "black", fill = "dark grey") +
  scale_fill_manual(values = c("white", "light grey")) +
  facet_grid(facets = Response ~ Explan, scales = "free", labeller = labeller(Response = response.metrics, Explan = explanatory.variables)) +
  theme_bw(base_size = 10) +
  labs(x = "Explanatory Variable", y = "Response Variable") + guides(linetype = FALSE, fill = FALSE)
ggsave("graphs/Figure.S2.png", width = 6.5, height = 6.5, units = "in", dpi = 600)


# 5.2.1 Partial correlation plot (code from Evan Batzer) ----
# partial residuals (Sequential partial correlations)
# Order of terms reflects addition of variables in stepwise model selection

bray_dat <- site.summary %>% dplyr::select(bray, total_mass_CV, log.S)
bray_runs <- rbind(data.frame(resid = (resid(lm(bray ~ 1, bray_dat))),
                              resp = "bray", var = bray_dat$total_mass_CV, varname = "total_mass_CV", order = 1),
                   data.frame(resid = (resid(lm(bray ~ total_mass_CV, bray_dat))),
                              resp = "bray", var = bray_dat$log.S, varname = "log.S", order = 2))

sor_dat <- site.summary %>% dplyr::select(sor, log.S, ANN_TEMP_RANGE, total_mass_CV)
sor_runs <- rbind(data.frame(resid = (resid(lm(sor ~ 1, sor_dat))),
                             resp = "sor", var = sor_dat$log.S, varname = "log.S", order = 1),
                  data.frame(resid = (resid(lm(sor ~ log.S, sor_dat))),
                             resp = "sor", var = sor_dat$ANN_TEMP_RANGE, varname = "ANN_TEMP_RANGE", order = 2),
                  data.frame(resid = (resid(lm(sor ~ log.S + ANN_TEMP_RANGE, sor_dat))),
                             resp = "sor", var = sor_dat$total_mass_CV, varname = "total_mass_CV", order = 3))

prop_bal_dat <- site.summary %>% dplyr::select(prop_bal, log.S, MAP)
prop_bal_runs <- rbind(data.frame(resid = (resid(lm(prop_bal ~ 1, prop_bal_dat))),
                                  resp = "prop_bal", var = prop_bal_dat$log.S, varname = "log.S", order = 1),
                       data.frame(resid = (resid(lm(prop_bal ~ log.S, prop_bal_dat))),
                                  resp = "prop_bal", var = prop_bal_dat$MAP, varname = "MAP", order = 2))

prop_sim_dat <- site.summary %>% dplyr::select(prop_sim, log.S, total_mass, ANN_TEMP_RANGE)
prop_sim_runs <- rbind(data.frame(resid = (resid(lm(prop_sim ~ 1, prop_sim_dat))),
                                  resp = "prop_sim", var = prop_sim_dat$log.S, varname = "log.S", order = 1),
                       data.frame(resid = (resid(lm(prop_sim ~ log.S, prop_sim_dat))),
                                  resp = "prop_sim", var = prop_sim_dat$total_mass, varname = "total_mass", order = 2),
                       data.frame(resid = (resid(lm(prop_sim ~ log.S + total_mass, prop_sim_dat))),
                                  resp = "prop_sim", var = prop_sim_dat$ANN_TEMP_RANGE, varname = "ANN_TEMP_RANGE", order = 3))

sigvals <- master %>% dplyr::select(Response, Explan, "Sig.AIC") %>% distinct() %>%
  mutate(Explan = case_when(Explan == "ANN_TEMP_RANGE" ~ "ANN_TEMP_RANGE",
                            Explan == "total_mass_CV" ~ "total_mass_CV",
                            TRUE ~ as.character(Explan))) %>%
  dplyr::rename("resp" = "Response",
                "varname" = "Explan",
                "Sig" = "Sig.AIC")

pc_dat <- bind_rows(bray_runs, sor_runs, prop_bal_runs, prop_sim_runs) %>%
  right_join(sigvals) %>% 
  mutate(resp = case_when(resp == "bray" ~ "Bray-Curtis Dissimilarity",
                          resp == "sor" ~ "Sorensen Dissimilarity",
                          resp == "prop_bal" ~ "Balanced Variation (%)",
                          resp == "prop_sim" ~ "Species Turnover (%)")) %>%
  replace_na(list(Sig = "Sig")) %>% 
  # Rescaling residuals
  mutate(resid = scale(resid)) %>%
  mutate(varname = case_when(varname == "log.S" ~ "Gamma Diversity",
                             varname == "ANN_TEMP_RANGE" ~ "Ann. Temp. Range",
                             varname == "MAP" ~ "Mean Ann. Precip.",
                             varname == "total_mass" ~ "Productivity",
                             varname == "total_mass_CV" ~ "Local Variation")) %>%
  mutate(varname = factor(varname, levels = c("Gamma Diversity",
                                              "Ann. Temp. Range",
                                              "Mean Ann. Precip.",
                                              "Productivity",
                                              "Local Variation"))) %>%
  mutate(resp = factor(resp, levels = c("Bray-Curtis Dissimilarity",
                                        "Sorensen Dissimilarity",
                                        "Balanced Variation (%)",
                                        "Species Turnover (%)")))  %>% 
  mutate(resid = case_when(is.na(resid) ~ 0,
                           TRUE ~ as.numeric(resid))) %>%
  mutate(siglabel = case_when(Sig == "NS" ~ "N.S.",
                              Sig == "Sig" ~ ""))

labdat = pc_dat %>%   
  ungroup() %>%
  dplyr::group_by(varname) %>%
  dplyr::summarise(meanval = mean(range(na.omit(var))))

graph.data <- left_join(pc_dat, labdat)
graph.data.summary <- ddply(graph.data, .(resp, varname, order), summarize, var = max(var), resid = mean(resid), Sig = max(Sig))
ggplot(data = graph.data, aes(x = var, y = resid)) +
  geom_rect(data = graph.data.summary, aes(fill = as.factor(Sig)), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  #geom_text(aes(label = siglabel, x = meanval, y = 0)) +
  geom_text(data = graph.data.summary, aes(x = var, y = -2, label = order), colour = "red", size = 5, hjust = "inward") +
  stat_smooth(method = "lm", se = FALSE, size = 1, colour = "black") +
  geom_point(shape = 21, colour = "black", fill = "dark grey") +
  scale_fill_manual(values = c("white", "light grey")) +
  facet_grid(facets = resp ~ varname, scales = "free_x") +
  ylim(-3, 3) +
  theme_bw(base_size = 10) +
  labs(x = "Explanatory Variable", y = "Scaled Residual") + guides(fill = FALSE)
ggsave("graphs/Figure.2.png", width = 6.5, height = 6.5, units = "in", dpi = 600)


# 5.3 PCA of variance components ----
part.distances$term <- gsub(":", ".", part.distances$term)
part.distances$term.red <- revalue(part.distances$term, c(block = "Block", block.trt = "Block.Nutrient", block.year_trt = "Block.Year",
                                                          residual = "Block.Year.Nutrient", trt = "Nutrient", year_trt = "Year", year_trt.trt = "Year.Nutrient"))
part.distances$term.abbrev <- revalue(part.distances$term, c(block = "B", block.trt = "BxN", block.year_trt = "BxY",
                                                          residual = "BxYxN", trt = "N", year_trt = "Y", year_trt.trt = "YxN"))
part.distances[part.distances < 0 ] <- 0

#total B-C compositional variance
var.comp_bray <- part.distances %>% dplyr::select(site_code, term.abbrev, SS_bray) %>% matrify() %>% dplyr::select(-total) #50 x 7
var.prop_bray <- var.comp_bray / rowSums(var.comp_bray) # calculate each variance term as proportion of total; 50 x 7

var.prop.bray.triplet <- var.prop_bray
var.prop.bray.triplet$site_code <- row.names(var.prop.bray.triplet)
var.prop.bray.triplet <- gather(var.prop.bray.triplet, B, BxN, BxY, BxYxN, N, Y, YxN, key = term.abbrev, value = prop.SS) %>%
  merge(y = unique(part.distances[ , c("term", "term.red", "term.abbrev")]), by = "term.abbrev") %>%
  mutate(term.red = factor(term.red, ordered = TRUE,
                           levels = c("Block", "Year", "Nutrient", "Block.Year", "Block.Nutrient", "Year.Nutrient", "Block.Year.Nutrient")))
var.prop.bray.triplet <- var.prop.bray.triplet[with(var.prop.bray.triplet, order(site_code, term.red)), ]

# separate pie chart for each site (Figure S3)
var.prop.bray.triplet2 <- merge(x = var.prop.bray.triplet, y = site.covars)
ggplot(data = var.prop.bray.triplet2, aes(x = factor(1), y = prop.SS, fill = term.red)) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") +
  facet_wrap(facets = ~ country * site_code, ncol = 9) + theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = NULL) + scale_x_discrete(breaks = NULL) + 
  scale_fill_manual(values = source.colours, 
                    guide = guide_legend(title = "Source"))
standard.ggsave("graphs/Figure.S3.bray.variance.piecharts.site_code.country.png", height = 8, width = 6.5)


# Create biplots
summary(var.bray.pca <- princomp(var.prop_bray), loadings = TRUE, cutoff = 0)
biplot(var.bray.pca)

pca.plot(var.bray.pca, x = "Comp.1", y = "Comp.2", expansion = 0.4, cutoff = 0,
         lab.x.title = "PC1 (58%)", lab.y.title = "PC2 (24%)", source.colours = source.colours)
standard.ggsave(filename = "graphs/Figure.3A.bray.PCA.png", height = 3)

var.prop1_bray <- dematrify(var.prop_bray)
colnames(var.prop1_bray) <- c("site_code", "term.abbrev", "prop.var")
(summary.bray <- ddply(var.prop1_bray, .(term.abbrev), summarize, prop.var =  round(100 * sum(prop.var) / nrow(exptsites.sub), 1)))

#total Sorensen compositional variance
var.comp_sor <- part.distances %>% dplyr::select(site_code, term.abbrev, SS_sor) %>%
  matrify() %>% dplyr::select(-total) #50 x 7
var.prop_sor <- var.comp_sor / rowSums(var.comp_sor) # calculate each variance term as proportion of total; 50 x 7
summary(var.sor.pca <- princomp(var.prop_sor), loadings = TRUE, cutoff = 0)
biplot(var.sor.pca)

pca.plot(var.sor.pca, x = "Comp.1", y = "Comp.2", expansion = 0.4, cutoff = 0,
         lab.x.title = "PC1 (56%)", lab.y.title = "PC2 (32%)", source.colours = source.colours)
standard.ggsave(filename = "graphs/Figure.3B.sor.PCA.png", height = 3)

var.prop1_sor <- dematrify(var.prop_sor)
colnames(var.prop1_sor) <- c("site_code", "term.abbrev", "prop.sor")
(summary.sor <- ddply(var.prop1_sor, .(term.abbrev), summarize, prop.sor = round(100 * sum(prop.sor) / nrow(exptsites.sub), 1)))

#proportion of compositional variance due to balanced variation in abundance
var.comp_prop.bal <- part.distances %>% dplyr::select(site_code, term.abbrev, SS_prop_bal) %>%
  matrify() %>% dplyr::select(-total) #50 x 7
var.prop_prop.bal <- var.comp_prop.bal / rowSums(var.comp_prop.bal) # calculate each variance term as proportion of total; 50 x 7
summary(var.prop_prop.bal.pca <- princomp(var.prop_prop.bal), loadings = TRUE, cutoff = 0)
biplot(var.prop_prop.bal.pca)

pca.plot(var.prop_prop.bal.pca, x = "Comp.1", y = "Comp.2", expansion = 0.1, cutoff = 0,
         lab.x.title = "PC1 (68%)", lab.y.title = "PC2 (17%)", source.colours = source.colours)
standard.ggsave(filename = "graphs/Figure.3C.prop_bal.PCA.png", height = 3)

var.prop1_prop.bal <- dematrify(var.prop_prop.bal)
colnames(var.prop1_prop.bal) <- c("site_code", "term.abbrev", "prop.bal")
(summary.prop.bal <- ddply(var.prop1_prop.bal, .(term.abbrev), summarize, prop.bal =  round(100 * sum(prop.bal) / nrow(exptsites.sub), 1)))

#proportion of compositional variance due to species turnover
var.comp_prop.sim <- part.distances %>% dplyr::select(site_code, term.abbrev, SS_prop_sim) %>%
  matrify() %>% dplyr::select(-total) #50 x 7
var.prop_prop.sim <- var.comp_prop.sim / rowSums(var.comp_prop.sim) # calculate each variance term as proportion of total; 50 x 7
summary(var.prop_prop.sim.pca <- princomp(var.prop_prop.sim), loadings = TRUE, cutoff = 0)
biplot(var.prop_prop.sim.pca)

pca.plot(var.prop_prop.sim.pca, x = "Comp.1", y = "Comp.2", expansion = 0.2, cutoff = 0,
         lab.x.title = "PC1 (50%)", lab.y.title = "PC2 (29%)", source.colours = source.colours)
standard.ggsave(filename = "graphs/Figure.3D.prop_sim.PCA.png", height = 3)

var.prop1_prop.sim <- dematrify(var.prop_prop.sim)
colnames(var.prop1_prop.sim) <- c("site_code", "term.abbrev", "prop.sim")
(summary.prop.sim <- ddply(var.prop1_prop.sim, .(term.abbrev), summarize, prop.sim = round(100 * sum(prop.sim) / nrow(exptsites.sub), 1)))

#combine partitioning summary information from all four metrics
summary.partitions <- merge(x = summary.bray, y = summary.sor) %>%
  merge(y = summary.prop.bal) %>%
  merge(y = summary.prop.sim) %>%
  gather(prop.var, prop.sor, prop.bal, prop.sim, key = Response, value = prop.SS)
summary.partitions <- merge(x = summary.partitions, 
                            y = unique(part.distances[ , c("term", "term.red", "term.abbrev")]), by = "term.abbrev")
summary.partitions$I.v.A <- ifelse(summary.partitions$Response %in% c("prop.var", "prop.bal"), "Abundance", "Incidence")
summary.partitions$Type.v.Aspect <- ifelse(summary.partitions$Response %in% c("prop.var", "prop.sor"), "Magnitude of Dissimilarity", "Relative Importance of Turnover")

metrics <- data.frame(Response = c("prop.bal", "prop.sim", "prop.sor", "prop.var"),
                      Metric = c("Balanced\nVariation (%)", "Species\nTurnover (%)", "Sorensen\nDissimilarity", "Bray-Curtis\nDissimilarity"),
                      Metric2 = c("Balanced\nVariation (%)", "Species\nTurnover (%)", "Incidence-\nbased", "Abundance-\nbased"))
summary.partitions <- merge(x = summary.partitions, y = metrics)
summary.partitions <- summary.partitions %>%
  mutate(Metric = factor(Metric, ordered = TRUE, levels = c("Bray-Curtis\nDissimilarity", "Sorensen\nDissimilarity", "Balanced\nVariation (%)", "Species\nTurnover (%)"))) %>%
  mutate(Metric2 = factor(Metric2, ordered = TRUE, levels = c("Abundance-\nbased", "Incidence-\nbased", "Balanced\nVariation (%)", "Species\nTurnover (%)"))) %>%
  mutate(Type.v.Aspect = factor(Type.v.Aspect, ordered = TRUE, levels = c("Magnitude of Dissimilarity", "Relative Importance of Turnover"))) %>%
  mutate(term.red = factor(term.red, ordered = TRUE, levels = c("Block", "Year", "Nutrient", "Block.Year", "Block.Nutrient", "Year.Nutrient", "Block.Year.Nutrient")))

ggplot(data = summary.partitions, aes(x = Metric, y = prop.SS, fill = factor(term.red))) +
  geom_bar(width = 0.9, stat = "identity", colour = "black") +
  facet_grid(facets = . ~ Type.v.Aspect, scales = "free_x") + theme_bw(base_size = 10) +
  theme(legend.position = "right") +
  labs(x = "", y = "Percent of Total Variation") +
  scale_fill_manual(values = source.colours, 
                    guide = guide_legend(title = "Source"))
standard.ggsave("graphs/Figure.4.variance.partitions.png", height = 4, width = 6.5)


# 6.0 SUPPLEMENTARY MATERIAL ----
# 6.1 Site Information (Table S1) ----
tableS1 <- site.covars[ , c("site_code", "S", "total_mass", "total_mass_CV", "MAT", "TEMP_VAR",
                            "ANN_TEMP_RANGE", "TEMP_WET_Q", "MAP", "MAP_VAR", "N_Dep", "management")]
tableS1$total_mass <- round(tableS1$total_mass, 0)
tableS1$total_mass_CV <- round(tableS1$total_mass_CV, 0)
tableS1$MAT <- round(tableS1$MAT, 1)
tableS1$TEMP_VAR <- round(tableS1$TEMP_VAR, 0)
tableS1$ANN_TEMP_RANGE <- round(tableS1$ANN_TEMP_RANGE, 0)
tableS1$TEMP_WET_Q <- round(tableS1$TEMP_WET_Q, 1)
tableS1$MAP <- round(tableS1$MAP, 0)
tableS1$MAP_VAR <- round(tableS1$MAP_VAR, 0)
tableS1$N_Dep <- round(tableS1$N_Dep, 1)
tableS1
write.csv(tableS1, "graphs/TableS1.csv")


# 6.2 World Map (Figure S1) ----
#Create world map of sites, coded by cluster group
map.dat.red <- map.dat[map.dat$site_code %in% exptsites.sub$site_code, ]

map.dat.red <- merge(x = map.dat.red, y = summary.all.medians, all.y = TRUE) %>%
  merge(y = site.summary)

WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

map.dat.red <- map.dat.red %>%
  mutate(BalVar.group2 = ifelse(BalVar.group == "High bal var", "High", "Low"),
         SppTurn.group2 = ifelse(SppTurn.group == "High spp turn", "High", "Low"))
ggplot(data = WorldData, aes(x = long, y = lat)) +
  geom_map(map = WorldData, aes(map_id = region), size = 0.5, fill = "grey", color = "grey") +
  geom_point(data = map.dat.red, aes(x = longitude, y = latitude, fill = bray, shape = BalVar.group2), colour = "black") +
  scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "white", midpoint = median(map.dat.red$bray)) +
  scale_shape_manual(values = c(24, 25)) +
  labs(x = NULL, y = NULL, fill = "Bray-Curtis\nDissimilarity", shape = "Balanced\nVariation (%)",
       title = "A) Abundance-based Metrics") +
  scale_y_continuous(breaks=c(), expand = c(0, 0)) +
  scale_x_continuous(breaks=c(), expand = c(0, 0)) +
  theme_bw(base_size = 9) +
  guides(fill = guide_colourbar(order = 1), shape = guide_legend(order = 2)) +
  theme(legend.position = c(0.07, 0.40))
ggsave("graphs/Figure.S1A.Abundance.png", height = 3.75, width = 6.5, units = "in", dpi = 600)

ggplot(data = WorldData, aes(x = long, y = lat)) +
  geom_map(map = WorldData, aes(map_id = region), size = 0.5, fill = "grey", color = "grey") +
  geom_point(data = map.dat.red, aes(x = longitude, y = latitude, fill = sor, shape = SppTurn.group2), colour = "black") +
  scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "white", midpoint = median(map.dat.red$sor)) +
  scale_shape_manual(values = c(24, 25)) +
  labs(x = NULL, y = NULL, fill = "Sorensen\nDissimilarity", shape = "Species\nTurnover (%)",
       title = "B) Incidence-based Metrics") +
  scale_y_continuous(breaks=c(), expand = c(0, 0)) +
  scale_x_continuous(breaks=c(), expand = c(0, 0)) +
  theme_bw(base_size = 9) +
  guides(fill = guide_colourbar(order = 1), shape = guide_legend(order = 2)) +
  theme(legend.position = c(0.07, 0.40))
ggsave("graphs/Figure.S1B.Incidence.png", height = 3.75, width = 6.5, units = "in", dpi = 600)


# 6.3 Appendix illustrating partitioning ----
appendix <- data.frame(Block = c(1,1,2,2),
                       Nutrient = c("N", "C", "N", "C"),
                       Plot = c(2,6,19, 11),
                       S = c(18, 20, 19, 28))
adonis(dist(appendix$S) ~ Block + Nutrient, data = appendix, permutations = 0)
summary(aov(appendix$S ~ Block + Nutrient, data = appendix))

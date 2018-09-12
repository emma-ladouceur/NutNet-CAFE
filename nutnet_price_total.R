

library(priceTools)
library(optparse)

# ------------------------------------------------------------------------------
# parsing arguments
# ------------------------------------------------------------------------------

parser <- OptionParser(
  usage = "Rscript %prog input output"
)

cli <- parse_args(parser, positional_arguments = 2)

# ------------------------------------------------------------------------------
# assign a few shortcuts
# ------------------------------------------------------------------------------

input  <- cli$args[1]
output <- cli$args[2]

# ------------------------------------------------------------------------------
# actual program
# ------------------------------------------------------------------------------

#how to enter each file name? 13 X (check submit script and change if this changes)
table <- readRDS(input)

group.vars <- c('plot','seed.rich','site','block')
treat.vars<-c('trt')
grouped.data <- table %>% group_by_(.dots=c(group.vars,treat.vars))

#takes a long time
res <- pairwise.price(grouped.data, species="species", func="biomass.sp")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp<-res
pp<-group.columns(pp,gps=c(group.vars,treat.vars),drop=T)

# Subset pairwise results:
#no longer relevant
#pp<-pp[pp$trt %in% c('Control Seeds', 'Control Control'),]

#second column is the experiment name, we
# assume all rows contain same value for more info see prep c
result<-list(
  Experiment=table[1,17],
  data=pp)

saveRDS(result, file = output)





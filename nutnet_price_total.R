

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

group.vars <- c('site_code','plot','block')
treat.vars<-c('trt_year')

grouped.data <- table %>% group_by_(.dots=c(group.vars,treat.vars))

#takes a long time
res <- pairwise.price(grouped.data, species="Taxon", func="Biomass_CalcSp")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp<-res
pp<-group.columns(pp,gps=c(group.vars,treat.vars), drop=T)


#second column is the experiment name, we
# assume all rows contain same value for more info see prep c
result<-list(
  site_code=table$site_code[1],
  data=pp)


saveRDS(result,file = output)





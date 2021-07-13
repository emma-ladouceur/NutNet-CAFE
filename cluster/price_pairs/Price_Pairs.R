

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


table <- readRDS(input)

group.vars <- c('site.year.id','plot','block')
treat.vars<-c('trt_year')

grouped.data <- table %>% group_by(.dots=c(group.vars,treat.vars))

res <- pairwise.price(grouped.data, species="Taxon", func="biomass.sp.full") # per species biomass as a response
#res <- pairwise.price(grouped.data, species="Taxon", func="max_cover") # with cover as a response


pp <- res
pp <- group.columns(pp,gps=c(group.vars,treat.vars), drop=T)

result<-list(
  site_code=table$site_code[1],
  data=pp)


saveRDS(result,file = output)



#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/survey.r')

stftn <- ddply(survey.2012(), .(agg.position, region), function(df){
  with(df,{
    data.frame(
      median = if(length(satisfaction) > 10) median(satisfaction) else NA,
      mad    = if(length(satisfaction) > 10) mad(satisfaction)    else NA
    )
  })
})

p <- ggplot(
  data.frame(lapply(na.omit(subset(stftn,region != "Other")), function(x) x[drop=TRUE])),
  aes(y    = median,
      ymin = median - mad,
      ymax = median + mad,
      x    = region,
      col  = region))

p <- p + facet_grid(agg.position ~ .)
p <- p + geom_crossbar(width=0.8,lwd=1.1)
p <- p + coord_flip()
p <- p + theme_bw()
p <- p + scale_x_discrete("",labels=rep("",4),breaks=1:4)
p <- p + scale_color_brewer("Region",palette="Set2")
p <- p + scale_y_continuous("Feeling of satisfaction (0-9)")
p <- p + opts(legend.position = "top")
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))

generate_plot_file(p,'satisfaction_and_position.png',height=1000)

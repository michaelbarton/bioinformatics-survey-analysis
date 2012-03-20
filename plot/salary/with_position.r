#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/survey.r')

slry <- ddply(survey.2012(), .(agg.position, filtered.region), function(df){
  with(df,{
    data.frame(
      median = if(length(salary) > 10) median(salary) else NA,
      mad    = if(length(salary) > 10) mad(salary)    else NA
    )
  })
})

data.frame(lapply(na.omit(subset(slry,filtered.region != "Other")), function(x) x[drop=TRUE]))

p <- ggplot(
  data.frame(lapply(na.omit(subset(slry,filtered.region != "Other")), function(x) x[drop=TRUE])),
  aes(y    = median,
      ymin = median - mad,
      ymax = median + mad,
      x    = agg.position,
      col  = filtered.region))

p <- p + geom_crossbar(position = position_dodge(width=0.90) ,lwd=1, width = 0.6)
p <- p + coord_flip()
p <- p + theme_bw()
p <- p + scale_x_discrete("")
p <- p + scale_color_brewer("Region",palette="Set2")
p <- p + theme_bw()
p <- p + scale_y_continuous("Annual Gross Income in Dollars")
p <- p + opts(legend.position = "top")
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))

generate_plot_file(p,'salary_and_position.png',height=960)

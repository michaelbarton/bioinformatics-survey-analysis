#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/survey.r')

stftn <- ddply(survey.2012(), .(agg.position, filtered.region), function(df){
  with(df,{
    data.frame(
      median = if(length(satisfaction) > 10) median(satisfaction) else NA,
      mad    = if(length(satisfaction) > 10) mad(satisfaction)    else NA
    )
  })
})

p <- ggplot(
  data.frame(lapply(na.omit(subset(stftn,filtered.region != "Other")), function(x) x[drop=TRUE])),
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
p <- p + scale_y_continuous("Feeling of satisfaction (0-9)")
p <- p + opts(legend.position = "top")
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))

generate_plot_file(p,'satisfaction_and_position.png',height=960)

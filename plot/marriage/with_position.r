#!/usr/bin/env Rscript

library(ggplot2)
library(plyr)

source('lib/generate_plot_file.r')
source('lib/survey.r')

p <- ggplot(
  ddply(subset(survey.2012(), ! is.na(position)),.(position),function(df){
    data.frame(married = mean(df$married, na.rm=TRUE) * 100)
  }),
  aes(y = married, x = position))

p <- p + geom_bar()
p <- p + theme_bw()
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Percent married or living with partner",
                            limits=c(0,100))
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))

generate_plot_file(p,'marriage_and_position.png',height=960)

#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/survey.r')

p <- ggplot(
  subset(survey.2012(), ! is.na(position)),
  aes( y = hours, x = position))

p <- p + stat_boxplot()
p <- p + theme_bw()
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Hours worked per week")
p <- p + opts(legend.position = "top")
p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))

generate_plot_file(p,'hours_and_position.png',height=960)

#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/plot_by_position.r')

generate_plot_file(
  plot_by_position('satisfaction',"Feeling of satisfaction (0-9)"),
  'satisfaction_and_position.png',
  height=1000)

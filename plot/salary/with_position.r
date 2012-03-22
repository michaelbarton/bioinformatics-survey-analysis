#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/plot_by_position.r')

generate_plot_file(
  plot_by_position(
    'salary',
    'Pre-Tax Annual Gross Income in Dollars'),
  'salary_and_position.png',
  height=1000)

#!/usr/bin/env Rscript

library(ggplot2)

source('lib/survey.r')
source('lib/generate_plot_file.r')

d <- na.omit(
       ddply(subset(survey.2012(),education == "PhD"), .(agg.sector, region), function(df){
         with(df,{
           data.frame(
             salary = if(length(salary) < 10) NA else salary,
             age    = age )})}))

p <- ggplot(d,aes(x=age, y=salary, col=factor(agg.sector)))
p <- p + geom_point()
p <- p + facet_grid(region ~ .)
p <- p + scale_color_brewer("Region",palette="Set2")
p <- p + theme_bw()
p <- p + opts(legend.position = "top")

generate_plot_file(p,'salary_with_age_and_region.png',height=1000)

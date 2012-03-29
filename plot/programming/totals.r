#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/survey.r')

languages <- survey.2012()$languages

raw.data <- gsub(".*[Ss]hell.*|.*[Bb]ash.*|sh|zsh.", "Shell",
              gsub("ruby","Ruby",
                 unlist(sapply(languages,strsplit,", "))))

counts <- subset(count(raw.data), freq > 5)
counts <- within(counts,{
  freq <- freq / length(languages) * 100
  x    <- factor(x,levels=x[order(counts$freq)])
})

p <- ggplot(counts, aes(x=x,y=freq))
p <- p + geom_bar()
p <- p + coord_flip()
p <- p + theme_bw()
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Programming Language Usage (%)")

generate_plot_file(p, 'programming_totals.png')

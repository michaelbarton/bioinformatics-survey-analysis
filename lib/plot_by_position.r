source('lib/survey.r')

plot_by_position <- function(variable, ylab=""){

  d <- ddply(survey.2012(), .(agg.position, region), function(df){
    o <- data.frame(
      median = if(length(df[,variable]) > 10) median(df[,variable]) else NA,
      mad    = if(length(df[,variable]) > 10) mad(df[,variable])    else NA
    )
  })

  p <- ggplot(
    data.frame(lapply(na.omit(subset(d,region != "Other")), function(x) x[drop=TRUE])),
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
  p <- p + scale_y_continuous(ylab)
  p <- p + opts(legend.position = "top")
  p <- p + opts(axis.text.x=theme_text(angle=-90, hjust=0))
  p
}

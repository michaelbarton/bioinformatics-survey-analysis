#!/usr/bin/env Rscript

library(plyr)
library(ggplot2)

source('lib/generate_plot_file.r')

coefs <- read.csv("./model_salary_estimates.csv")
covs  <- melt(dget("./model_salary_covariance.R"))

coefs <- within(coefs,{
  region <- rep("Northern America",length(X))
  region[grep('agg.regionEurope',X)] <- "Europe"

  X <- sub("agg.regionEurope:","",X)
  X[X == "agg.regionEurope"] <- "(Intercept)"
})


marginals <- ddply(coefs,.(X,region),function(x){
  with(x,{
    data.frame(

      estimate = Value + if(region == "Europe"){
          coefs$Value[coefs$region == "Europe" & coefs$X == X]
        } else { 0 },

      error = if(region != "Europe"){
        coefs$Std..Error[coefs$region != "Europe" & coefs$X == X]
      } else {
        int = if(X == "(Intercept)") "agg.regionEurope" else paste("agg.regionEurope",X,sep=":")

        sqrt(sum(coefs$Std..Error[coefs$X %in% X]) +
          2 * covs$value[covs$X1 == X & covs$X2 == int])
      }
    )
  })
})


variable.freq  <- count(marginals$X)
constant.terms <- marginals$X %in% variable.freq$x[variable.freq$freq == 1]
marginals <- within(marginals,{
  region[constant.terms] <- "None"
  region <- factor(region,c("None","Northern America","Europe"))


  X <- sapply(X,function(x){
    switch(x,
      size          = "Largest Grant Size ($)",
      publications  = "Total Publications",
      hours         = "Average Weekly Hours Worked",
      grants        = "Total Grants",
      genderMale    = "Male Gender",
      first         = "First Author Publications",
      corresponding = "Corresponding Author Publications",
      conferences   = "Average Conference Attendence",
      age           = "Age",

      "agg.positionPI / Management"       = "PI / Management",
      "agg.positionPost Doctoral (Staff)" = "Post Doctoral (Staff)",
      x
    )
  })
  X <- factor(X,rev(c(
      "Male Gender",
      "Average Conference Attendence",
      "Average Weekly Hours Worked",
      "(Intercept)",
      "Total Grants",
      "First Author Publications",
      "Corresponding Author Publications",
      "Largest Grant Size ($)",
      "Total Publications",
      "Age",
      "PI / Management",
      "Post Doctoral (Staff)"
      )))
})

# Specify constants across regions

p <- ggplot(marginals,
            aes(x    = factor(X),
                y    = estimate,
                ymin = estimate - error,
                ymax = estimate + error,
                col = region))

p <- p + geom_point()
p <- p + coord_flip()
p <- p + theme_bw()
p <- p + scale_color_hue("Interaction Term")
p <- p + scale_y_continuous("Estimated Regression Coefficient")
p <- p + scale_x_discrete("")
p <- p + geom_abline(v=0,lty=2,col="grey")
p <- p + opts(legend.position = "top")

generate_plot_file(p,'salary_model_effects.png',height=700)

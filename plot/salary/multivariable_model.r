#!/usr/bin/env Rscript

library(plyr)
library(ggplot2)

source('lib/generate_plot_file.r')

coefs <- read.csv("./model_salary_estimates.csv")

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
        } else 0
    )
  })
})


# Specify constants across regions
variable.freq  <- count(marginals$X)
constant.terms <- marginals$X %in% variable.freq$x[variable.freq$freq == 1]
marginals$region[constant.terms] <- ""

marginals$term.type                 <- "Region interaction"
marginals$term.type[constant.terms] <- "Constant"


p <- ggplot(marginals,
            aes(x   = X,
                y   = estimate,
                col = region))

p <- p + facet_wrap( ~ term.type, scales = "free",space="free",ncol=1)
p <- p + geom_point()
p <- p + coord_flip()
p <- p + theme_bw()
p <- p + scale_y_continuous("Estimated Regression Coefficient",
                            limits=c(-75000,150000))
p <- p + geom_abline(v=0,lty=2,col="grey")
p

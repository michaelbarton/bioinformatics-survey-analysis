#!/usr/bin/env Rscript

library(robustbase)
library(robust)

source('models/salary/data.r')

full.formula <- formula(salary ~

              # Identity variables
              agg.region    +
              gender        +
              age           +
              agg.position  +
              hours         +
              publications  +
              first         +
              corresponding +
              grants        +
              conferences   +
              size          +

              # Interaction identity variables
              agg.region:gender        +
              agg.region:age           +
              agg.region:agg.position  +
              agg.region:hours         +
              agg.region:publications  +
              agg.region:first         +
              agg.region:corresponding +
              agg.region:size          +
              agg.region:corresponding +
              agg.region:grants

)

full.model <- lmRob(full.formula, data = salary.regression.data())

model.1 <- step.lmRob(full.model)

summary(model.1)

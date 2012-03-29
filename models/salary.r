#!/usr/bin/env Rscript

library(robustbase)
library(robust)

source('models/salary/data.r')

full.formula <- formula(salary ~

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

              # Interaction with region
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

write.csv(summary(model.1)$coefficients, file="./model_salary_estimates.csv")
write.csv(summary(model.1)$cov,          file="./model_salary_covariance.csv")

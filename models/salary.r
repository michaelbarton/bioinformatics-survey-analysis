#!/usr/bin/env Rscript

library(ggplot2)
library(MASS)

source('lib/survey.r')
source('lib/generate_plot_file.r')

# Filter for academics from US/Europe with PhD
academics <- subset(survey.2012(),
                    position    != "PhD Student"      &
                    position    != "Staff Technician" &
                    education   == "PhD"              &
                    sector      == "Academia"         &
                    (agg.region == "Europe"      | agg.region == "America, Northern"))

# Select regression variables
academics <- academics[
  c("salary",
    "agg.region",
    "gender",
    "age",
    "agg.position",
    "hours",
    "publications",
    "first",
    "corresponding",
    "grants",
    "size")]

scaled.data <- within(na.omit(academics),{
  # 1/10 of log10  of grant size
  size <- floor(log10(size + 1)) / 10

  # Scale numeric variables
  grants        <- scale(grants)
  publications  <- scale(publications)
  first         <- scale(first)
  corresponding <- scale(corresponding)
  age           <- scale(age)
  hours         <- scale(hours)
  salary        <- scale(salary)

  # Factors
  agg.region   <- factor(agg.region)
  agg.position <- factor(agg.position)
  gender       <- factor(gender)
})

full.model <- lm(salary ~

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
              size          +

              # Quadratic terms
              I(age^2)           +
              I(hours^2)         +
              I(publications^2)  +
              I(first^2)         +
              I(corresponding^2) +
              I(grants^2)        +
              I(size^2)          +

              # Interaction of region with variables
              agg.region:gender        +
              agg.region:age           +
              agg.region:agg.position  +
              agg.region:hours         +
              agg.region:publications  +
              agg.region:first         +
              agg.region:corresponding +
              agg.region:grants        +
              agg.region:size          +

              # Interaction of region with quadratic variables
              agg.region:I(age^2)           +
              agg.region:I(hours^2)         +
              agg.region:I(publications^2)  +
              agg.region:I(first^2)         +
              agg.region:I(corresponding^2) +
              agg.region:I(grants^2)        +
              agg.region:I(size^2),

              scaled.data)


model.1 <- step(full.model)
summary(model.1)

model.2 <- update(model.1, ~ .
                  - size
                  - I(publications^2)
                  - I(size^2)
                  - I(grants^2)
                  - agg.region:I(grants^2)
                  - agg.region:I(publications^2)
                  - agg.region:size)

summary(model.2)

anova(model.1,model.2)

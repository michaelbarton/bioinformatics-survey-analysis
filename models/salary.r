#!/usr/bin/env Rscript

library(ggplot2)
library(MASS)

source('lib/survey.r')
source('lib/generate_plot_file.r')

# Filter for academics from US/Europe with PhD
academics <- subset(survey.2012(),
                    agg.degree  != "Other"            &
                    position    != "PhD Student"      &
                    position    != "Staff Technician" &
                    education   == "PhD"              &
                    sector      == "Academia"         &
                    (agg.region == "Europe" | agg.region == "America, Northern"))

# Select regression variables
academics <- academics[
  c("salary",
    "agg.degree",
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

  # Quadratic variables
  square.grants        <- scale(grants^2)
  square.publications  <- scale(publications^2)
  square.first         <- scale(first^2)
  square.corresponding <- scale(corresponding^2)
  square.age           <- scale(age^2)
  square.hours         <- scale(hours^2)

  log.size <- floor(log10(size + 1)) / 10

  # Identity
  size          <- scale(size)
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
  agg.degree   <- factor(agg.degree)
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

              # Quadratic variables
              square.age           +
              square.hours         +
              square.publications  +
              square.first         +
              square.corresponding +
              square.grants        +
              log.size             +

              # Interaction of region with identity variables
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
              agg.region:square.age           +
              agg.region:square.hours         +
              agg.region:square.publications  +
              agg.region:square.first         +
              agg.region:square.corresponding +
              agg.region:square.grants        +
              agg.region:log.size,

              scaled.data)


model.1 <- step(full.model)
summary(model.1)

model.2 <- update(model.1, ~ . - 1
                  - size
                  - gender
                  - square.age
                  - square.grants
                  - square.publications
                  - log.size
                  - agg.region:agg.position
                  - agg.region:log.size
                  - agg.region:square.grants)

summary(model.2)

anova(model.1,model.2)

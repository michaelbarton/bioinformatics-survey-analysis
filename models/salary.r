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

summary(scaled.data)


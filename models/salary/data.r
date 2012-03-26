#!/usr/bin/env Rscript

source('lib/survey.r')

salary.regression.data <- function(){

  # Filter for academics from US/Europe with PhD
  # Didn't filter for degree background as this has no effect on model
  # prediction

  academics <- subset(survey.2012(),
                      position    != "PhD Student"      &
                      position    != "Staff Technician" &
                      education   == "PhD"              &
                      sector      == "Academia"         &
                      (agg.region == "Europe" | agg.region == "America, Northern"))

  # Select regression variables
  academics <- academics[
    c("salary",
      "agg.region",
      "gender",
      "age",
      "agg.position",
      "hours",
      "conferences",
      "publications",
      "first",
      "corresponding",
      "grants",
      "size")]

  scaled.data <- within(na.omit(academics),{
    # Factors
    agg.region   <- factor(agg.region)
    agg.position <- factor(agg.position)
    gender       <- factor(gender)
  })

  scaled.data
}

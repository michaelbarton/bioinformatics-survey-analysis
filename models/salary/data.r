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

    # Try to normalise skewed variables
    corresponding <- log(corresponding+1)
    first         <- log(first+1)
    grants        <- grants^(1/4)
    publications  <- log(publications+1)
    size          <- log(size+1) / 10

    log.size <- floor(log10(size + 1)) / 10

    # Quadratic variables
    # Grants, grant size, corresponding, and conferences not included as very
    # skewed
    square.publications  <- scale(publications^2)
    square.first         <- scale(first^2)
    square.age           <- scale(age^2)
    square.hours         <- scale(hours^2)

    # Identity
    size          <- scale(size)
    grants        <- scale(grants)
    publications  <- scale(publications)
    first         <- scale(first)
    corresponding <- scale(log(corresponding+1))
    age           <- scale(age)
    hours         <- scale(hours)
    salary        <- scale(salary)
    conferences   <- scale(conferences)

    # Factors
    agg.region   <- factor(agg.region)
    agg.position <- factor(agg.position)
    gender       <- factor(gender)
  })

  # Remove outliers
  scaled.data[-c(12,13,30,78,84),]
}

#!/usr/bin/env Rscript

source('lib/survey.r')

salary.regression.data <- function(){

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

    # Try to normalise skewed variables
    corresponding <- log(corresponding+1)
    first         <- log(first+1)
    grants        <- grants^(1/4)
    publications  <- log(publications+1)
    size          <- log(size+1) / 10

    log.size <- floor(log10(size + 1)) / 10

    # Quadratic variables
    # Grants and grant size not included as very skewed
    square.publications  <- scale(publications^2)
    square.first         <- scale(first^2)
    square.corresponding <- scale(corresponding^2)
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

    # Factors
    agg.region   <- factor(agg.region)
    agg.position <- factor(agg.position)
    agg.degree   <- factor(agg.degree)
    gender       <- factor(gender)
  })

  # Remove outliers
  scaled.data[-c(10,11,71,76,108),]
}

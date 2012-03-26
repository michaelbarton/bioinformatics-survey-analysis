survey.2012 <- function(){
  s <- survey('data/2012/data/survey.csv')
  within(s,{

    position         <- factor(position)
    levels(position) <- sapply(levels(position),function(x){
      switch(x,
       "Staff Technician"                     = "Staff Technician",
       "Under-graduate student"               = "Under-graduate",
       "Masters Student"                      = "Masters Student",
       "PhD Student"                          = "PhD Student",
       "Post Doctoral Scientist (Researcher)" = "Post Doctoral (Rsrch)",
       "Post Doctoral Scientist (Staff)"      = "Post Doctoral (Staff)",
       "Principal Investigator / Lab Head / Management" =
         "PI / Management",
       "Senior Principal Investigator / Professor / Senior Management" =
         "Senior PI / Management",
        NA)
    })

    age <- 2011 - year

    agg.position <- position
    levels(agg.position)[levels(agg.position) == "Senior PI / Management"] <- "PI / Management"

    agg.sector <- factor(sector)
    levels(agg.sector)[levels(agg.sector) == "Industry - Other"] <- "Industry"
    levels(agg.sector)[levels(agg.sector) == "Industry - Pharma"] <- "Industry"

    agg.degree <- factor(sapply(degree,function(x){
      switch(x,
        "Biology"                = identity(x),
        "Computing Science"      = identity(x),

        "Molecular Genetics"     =,
        "Molecular Biology"      =,
        "Microbiology"           =,
        "Quantitative Biology"   =,
        "Bioscience"             =,
        "biotechnology"          =,
        "genetics"               =,
        "Genetics"               =,
        "quantitative genetics"  =,
        "biochemistry"           =,
        "Biochemistry"           = "Biology",

        "Computer Engineering"   = ,
        "Informatin Systems"     = ,
        "Information Technology" = "Computing Science",
        "Other"
      )
    }))


    # Merge european regions together
    agg.region <- sub("Europe, .*","Europe",region)

    # Assume empty grant fields are 0
    grants[grants == "" | is.na(grants)] <- 0
    size[is.na(size)] <- 0

    # Fill empty variables
    first[first == ""]                 <- NA
    publications[publications == ""]   <- NA
    corresponding[corresponding == ""] <- NA
    gender[gender == ""] <- NA

    publications  <- as.numeric(publications)
    first         <- as.numeric(first)
    corresponding <- as.numeric(corresponding)
    grants        <- as.numeric(grants)
    age           <- as.numeric(age)
    hours         <- as.numeric(hours)
    conferences   <- as.numeric(conferences)

    married <- sapply(married, function(x) switch(x, Yes = 1, No = 0, NA))

    salary  <- to.numeric.midpoint(salary)


    filtered.region <- factor(region)
    levels(filtered.region) <- sapply(levels(filtered.region),function(name){
      if(length(filtered.region[filtered.region == name]) < 30) "Other" else name
    })

  })
}

survey <- function(file){
  within(read.csv(file, stringsAsFactors = FALSE),{
    time <- format.date.string(time, "%m/%d/%Y %H:%M:%S")
  })
}

format.date.string <- function(x,expr){
  as.Date(x, expr)
}

to.numeric.midpoint <- function(x){
  sapply(strsplit(x,' - '), function(s) sum(as.numeric(gsub(',','',s))) / 2)
}

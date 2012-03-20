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
       "Post Doctoral Scientist (Researcher)" = "Post Doctoral (Research)",
       "Post Doctoral Scientist (Staff)"      = "Post Doctoral (Staff)",
       "Principal Investigator / Lab Head / Management" =
         "PI / Management",
       "Senior Principal Investigator / Professor / Senior Management" =
         "Senior PI / Management",
        NA)
    })

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

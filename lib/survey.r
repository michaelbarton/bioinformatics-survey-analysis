survey.2012 <- function(){
  s <- survey('data/2012/data/survey.csv')
  within(s,{
    position[position == ""] <- NA
    position <- factor(position, levels=c(
     "Under-graduate student",
     "Masters Student",
     "PhD Student",
     "Post Doctoral Scientist (Researcher)",
     "Post Doctoral Scientist (Staff)",
     "Principal Investigator / Lab Head / Management",
     "Senior Principal Investigator / Professor / Senior Management",
     "Staff Technician"))

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

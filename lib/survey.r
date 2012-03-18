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

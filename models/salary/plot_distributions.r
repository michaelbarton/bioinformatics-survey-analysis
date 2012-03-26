#!/usr/bin/env Rscript

source('models/salary/data.r')

d <- salary.regression.data()

plots <- sapply(names(d),function(name){
   var = d[,name]
   if(typeof(var) == "double"){
     png(paste("salary_model_",name,".png",sep=""))
     plot(density(var),xlab=name)
   }
})

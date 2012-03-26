#!/usr/bin/env Rscript

source('models/salary/data.r')

d <- salary.regression.data()

plots <- sapply(names(d),function(name){
   var = d[,name]
   if(typeof(var) == "double"){
     outliers <- head(order(var^2,decreasing=TRUE))

     png(paste("salary_model_",name,".png",sep=""))
     plot(var)
     text(outliers,var[outliers],outliers,pos=4)
   }
})

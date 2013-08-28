library(shiny)
library(ggplot2)
library(googleVis)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
#C:/Users/Sahir/Dropbox/Thesis/sahir R code/
#Import and manipulate data to include ageexit times, and make 1st observation survival prob=1
cumbase<-read.csv("cumbase_strat_99.csv", header=T)
cumbase<-cumbase[,-3]
acases99<-read.csv("acases_99.csv", header=T)
regions<-read.table("regions.txt", header=T, sep=",")
k<-rbind(acases99,acases99,acases99,acases99,acases99,acases99,acases99,acases99)
RS_EXIT<-k$AGEEXIT
cumbase<-cbind(cumbase,RS_EXIT)
cumbase$surv0<-exp(-1*cumbase$RISK)
cumbase$area<-sapply(cumbase$AREA, function(x) regions[which(x == regions$Number),1] )

#ontario life table data
lifetable<-read.table("lifetable.txt", header=T)
lifedata<-data.frame(lifetable$Age,lifetable$px)
names(lifedata)<-c("time", "surv")

#hazard ratios
beta_age<-0.0392083643
beta_cirs<-0.1352950048



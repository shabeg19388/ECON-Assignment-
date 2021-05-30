#Name- Shabeg Singh Gill 
#Roll No-2019388 
#Branch- CSSS

#directory will change based on where you have stored the file
setwd("~/Desktop/Shabeg_2019388")

#QUESTION-1

#Reading csv file 
data=read.csv("dataset.csv")

#(a)
mean(data$GER)
#(b)
mean(data$EL)
#(c)
mean(data$DW)
#(d)
mean(data$BT)
#(e)
mean(data$GT)
#(f)
mean(data$CA)
#(g)
var(data$GER)
var(data$EL)
var(data$DW)
var(data$BT)
var(data$GT)
var(data$CA)

#new table1 for storing values
avg_GER<- c(mean(data$GER))
avg_EL<- c(mean(data$EL))
avg_DW<- c(mean(data$DW))
avg_BT<- c(mean(data$BT))
avg_GT<- c(mean(data$GT))
avg_CA<-c(mean(data$CA))
table_mean <-data.frame(avg_GER, avg_EL, avg_DW, avg_BT, avg_GT, avg_CA)

#new table2 for storing values 
var_GER<-c(var(data$GER))
var_EL<-c(var(data$EL))
var_DW<-c(var(data$DW))
var_BT<-c(var(data$BT))
var_GT<-c(var(data$GT))
var_CA<-c(var(data$CA))
table_var<-data.frame(var_GER, var_EL, var_DW, var_BT, var_GT, var_CA)

print(table_mean)
print(table_var)

#(h)
hist(data$GER, main="Histogram of GER")
hist(data$DW, main="Histogram of DW")
hist(data$BT- data$GT, main="Histogram of Diff")

#(i)- IN PDF





#Question-2
library(dplyr)
library(tidytext)
library(tidyverse)

#added new column LIT to csv file 
#sorting
data_sorted=data[with(data, order(LIT)),]

low<-subset(data_sorted,data_sorted$LIT>0 & data_sorted$LIT<=72.87)
medium<-subset(data_sorted, data_sorted$LIT>74 & data_sorted$LIT<=81.42)
high<-subset(data_sorted, data_sorted$LIT>=82)

#(a)

Category<-c('Low', 'Medium', 'High')
Mean_GER<-c(mean(low$GER),mean(medium$GER), mean(high$GER) )
Mean_BT<-c(mean(low$BT), mean(medium$BT),mean(high$BT) )
Mean_GT<- c(mean(low$GT),mean(medium$GT), mean(high$GT) )
Mean_DW<-c(mean(low$DW), mean(medium$DW),mean(high$DW) )
Mean_EL<- c(mean(low$EL), mean(medium$EL), mean(high$EL))
Mean_LIT<- c(mean(low$LIT), mean(medium$LIT), mean(high$LIT))
Category_Mean<-data.frame(Category, Mean_GER, Mean_BT, Mean_GT,Mean_DW, Mean_EL, Mean_LIT )
print(Category_Mean)

#(b)
#Gross Enrollment ratio is lower in those states where the literacy rate level is low. Low group has the lowest GER, followed by 
#Medium and High Group.Also, the relation among the three groups is that enrollment is higher where the infrastructure is better. 

#(c)

Z<-c('Others', 'Others', 'Others', 'North-East', 'North-East',  'North-East', 'Others', 'Others', 'Others', 'Others', 'Others', 'Others', 'Southern', 'Southern', 'Southern', 'Southern', 'Southern', 'UT', 'UT', 'UT', 'Others', 'Others', 'Others', 'Others', 'Others', 'Others', 'Others', 'Others', 'Others', 'North-East', 'North-East', 'North-East', 'Southern', 'Southern', 'Southern', 'North-East','North-East', 'North-East', 'Southern','Southern', 'Southern','Others', 'Others', 'Others', 'Others', 'Others', 'Others', 'UT', 'UT', 'UT', 'Others', 'Others', 'Others', 'North-East', 'North-East', 'North-East','Others', 'Others', 'Others', 'North-East', 'North-East', 'North-East', 'North-East', 'North-East', 'North-East', 'Southern', 'Southern', 'Southern', 'North-East', 'North-East', 'North-East','Southern', 'Southern', 'Southern', 'North-East', 'North-East', 'North-East', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'UT', 'North-East', 'North-East', 'North-East', 'UT', 'UT', 'UT', 'North-East', 'North-East', 'North-East', 'UT', 'UT', 'UT', 'Southern', 'Southern', 'Southern' )
data_sorted$Zone <-Z
options(max.print=1000000)

#table for North-East Zone
NorthEast_Zone<-subset(data_sorted, data_sorted$Zone=='North-East')
#table for UT Zone
UT_Zone <-subset(data_sorted, data_sorted$Zone=='UT')
#table for Southern Zone
Southern_Zone<-subset(data_sorted, data_sorted$Zone=='Southern')
#table for Others Zone
Others_Zone<-subset(data_sorted, data_sorted$Zone=='Others')

#BT Mean and variance for all Zones 
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_BT<-c(mean(NorthEast_Zone$BT),mean(UT_Zone$BT), mean(Southern_Zone$BT), mean(Others_Zone$BT))
Variance_BT<-c(var(NorthEast_Zone$BT), var(UT_Zone$BT),var(Southern_Zone$BT),var(Others_Zone$BT) )
BT_table<- data.frame(Zone, Mean_BT, Variance_BT)
print(BT_table)

#EL Mean and variance for all Zones 
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_EL<-c(mean(NorthEast_Zone$EL),mean(UT_Zone$EL), mean(Southern_Zone$EL), mean(Others_Zone$EL))
Variance_EL<-c(var(NorthEast_Zone$EL), var(UT_Zone$EL),var(Southern_Zone$EL),var(Others_Zone$EL) )
EL_table<- data.frame(Zone, Mean_EL, Variance_EL)
print(EL_table)

#CA Mean and variance for all Zones 
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_CA<-c(mean(NorthEast_Zone$CA),mean(UT_Zone$CA), mean(Southern_Zone$CA), mean(Others_Zone$CA))
Variance_CA<-c(var(NorthEast_Zone$CA), var(UT_Zone$CA),var(Southern_Zone$CA),var(Others_Zone$CA) )
CA_table<- data.frame(Zone, Mean_CA, Variance_CA)
print(CA_table)

#GT Mean and variance for all Zones
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_GT<-c(mean(NorthEast_Zone$GT),mean(UT_Zone$GT), mean(Southern_Zone$GT), mean(Others_Zone$GT))
Variance_GT<-c(var(NorthEast_Zone$GT), var(UT_Zone$GT),var(Southern_Zone$GT),var(Others_Zone$GT) )
GT_table<- data.frame(Zone, Mean_GT, Variance_GT)
print(GT_table)

#DW Mean and Variance for all Zones 
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_DW<-c(mean(NorthEast_Zone$DW),mean(UT_Zone$DW), mean(Southern_Zone$DW), mean(Others_Zone$DW))
Variance_DW<-c(var(NorthEast_Zone$DW), var(UT_Zone$DW),var(Southern_Zone$DW),var(Others_Zone$DW) )
DW_table<- data.frame(Zone, Mean_DW, Variance_DW)
print(DW_table)

#GER Mean and Variance of all zones
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_GER<-c(mean(NorthEast_Zone$GER),mean(UT_Zone$GER), mean(Southern_Zone$GER), mean(Others_Zone$GER))
Variance_GER<-c(var(NorthEast_Zone$GER), var(UT_Zone$GER),var(Southern_Zone$GER),var(Others_Zone$GER) )
GER_table<- data.frame(Zone, Mean_GER, Variance_GER)
print(GER_table)

#LIT Mean and variance for all Zones 
Zone<-c('NorthEast', 'UT', 'Southern', 'Others')
Mean_LIT<-c(mean(NorthEast_Zone$LIT),mean(UT_Zone$LIT), mean(Southern_Zone$LIT), mean(Others_Zone$LIT))
Variance_LIT<-c(var(NorthEast_Zone$LIT), var(UT_Zone$LIT),var(Southern_Zone$LIT),var(Others_Zone$LIT) )
LIT_table<- data.frame(Zone, Mean_LIT, Variance_LIT)
print(LIT_table)





#Question-3 

#(a)
regression <- lm(data$GER ~ data$GT + data$DW + data$BT + data$EL + data$CA)
summary(regression)
#(i)
regression$coefficients
#(ii)
coef(summary(regression))[, "Std. Error"]**2
#(iii)
(summary(regression)$sigma)**2

#(b)

#adding two dummy variables, LOW and HIGH
data$HIGH<-0 
data$MED<-0
data[data$STATE%in%(c(high$STATE)),]$HIGH<-1
data[data$STATE %in%(c(medium$STATE)),]$MED<-1

#incorporating dummy
d1=lm(data$GER~ data$GT + data$DW + data$BT +data$EL +data$CA+ data$HIGH + data$MED)
summary(d1)
#(i)
d1$coefficients
#(ii)
coef(summary(d1))[, "Std. Error"]**2
#(iii)
(summary(d1)$sigma)**2

#(c)

#adding 3 dummmy variables, North-East, Southern, UT
data$NES=0
data$SS=0
data$UTS=0
data[data$STATE%in%(c(NorthEast_Zone$STATE)),]$NES=1
data[data$STATE %in%(c(Southern_Zone$STATE)),]$SS=1
data[data$STATE%in%(c(UT_Zone$STATE)),]$UTS=1

#incorporating dummy
d2=lm(data$GER~data$GT +data$BT +data$DW +data$EL +data$CA +data$NES +data$SS +data$UTS)
summary(d2)
#(i)
d2$coefficients
#(ii)
coef(summary(d2))[, "Std. Error"]**2
#(iii)
(summary(d2)$sigma)**2




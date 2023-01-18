rm(list=ls())
library(openxlsx)
###Input 
CRD = read.xlsx("size_callus.xlsx", 1)

#as.character(CRD$treat)
#as.numeric(CRD$ex)
#Fitting of linear model
model <-lm(CRD$ex~ CRD$treat)

#Obtains R Square and other statistics of fitted model
summary <-summary(model)
summary

#Carryout ANOVA
anova <-anova(model)
anova

#Below codes are used to obtain plots of fitted vs Residuals and Normal QQ plots
#par(mfrow=c(1,2))
#plot(model, which=1)
#plot(model, which=2)

#Load the package
library(agricolae)

#Carry out LSD test
LSD <-LSD.test(CRD$ex,CRD$treat,anova$`Df`[2],anova$`Mean Sq`[2])
LSD

#Generate the txt file of analysis 
sink("crd_LSD_sizecallus_analysis.txt")
print("ANOVA of CRD")
print(anova)
print("LSD ANALYSIS")
print(LSD$statistics)
print(LSD$groups)
sink()


###duncan
df.residual(model)
deviance(model)
df=df.residual(model)
MSerror=deviance(model)/df
MSerror
compare.trt=duncan.test(CRD$ex,CRD$treat,df,MSerror,group=TRUE,alpha=0.05)
compare.trt

#Generate the txt file of analysis
sink("crd_duncan_toplength.txt")
print("ANOVA of CRD")
print(anova)
print("duncan ANALYSIS")
print(compare.trt$statistics)
print(compare.trt$groups)
sink()




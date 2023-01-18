library(lme4)
library(openxlsx)
rm(list=ls())

line.blup <- NULL
heritability <- NULL

### LOCATION
setwd("E:/Desktop/MSc/Thesis_analysis/BLUP")

### READ FILE FOR BLUP 
C <- read.xlsx("BLUP_Puree_CA_Crop23_10082022.xlsx")
#C2<- read.xlsx("BLUP_Puree_abL_Crop23_10082022.xlsx")
### CHECK FILE
head(C)
attach(C)
C[,4]
C2[,4]

# control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4),
#                     check.nobs.vs.nlev = "ignore",
#                     check.nobs.vs.rankZ = "ignore",
#                     check.nobs.vs.nRE="ignore")
# varcomp <- lmer(x[,i]~(1|LINE)+(1|LOC)+(1|LINE:LOC),data=x,control=control) 
# #isSingular(varcomp, tol = 1e-4)

varcomp <- lmer(C[,3] ~ (1|ACC) + (1|YEAR) + (1|ACC:YEAR), data=C)
varcomp2 <-lmer(C[,3] ~ (1|ACC) + (1|YEAR), data=C)

#########################################
# -Denote nesting using %in%
# -Denote interactions using a colon or asterix between terms

summary(varcomp)

f <- fixef(varcomp)
r <- ranef(varcomp)$ACC
blup <- f+r
colnames(blup) <- c("h")
write.table(blup, "BLUP_Puree_h_Result.csv", quote=F,sep=",")

#checking overfitted model
isSingular(varcomp, tol= 1e-4)

### SEPARATE VARIANCE FOR HERITABILITY
var.trans <- lme4::VarCorr(varcomp)
var <- data.frame(Groups=c('ACC', 'YEAR', 'ACC:YEAR','Residual'),
                  Variance=c(as.numeric(var.trans$ACC),as.numeric(var.trans$YEAR), as.numeric(var.trans$`ACC:YEAR`),attr(var.trans,'sc')^2),check.names=F)

Gvar<-as.numeric(as.character(var$Variance))[var$Groups%in%'ACC']
Evar<-as.numeric(as.character(var$Variance))[var$Groups%in%'YEAR']
#Gvar2=as.numeric(var.trans3$ACC)
E2var=as.numeric(var.trans$`ACC:YEAR`)
#E2var<-as.numeric(as.character(var$variance))[var$Groups%in%'ACC:YEAR']
evar<-as.numeric(as.character(var$Variance))[var$Groups%in%'Residual']

############NORMAL HERITABILITY CALCULATION

h2 <- c(Gvar,Evar,E2var,evar,Gvar/(Gvar+Evar+(evar/3)+(E2var/3)))
h2


######### CULLIS HERITABILITY CALCULATION USING LME4 variables

### LOCATION SOURCE FOR CULLIS FUNCTION
source("E:/MSc/Analysis_Part/BLUP_Traits/Cullis_H2.R")

####Cullis_H2 <- function(model, geno_label = "GENO") 

h2_Cullis = Cullis_H2(varcomp, geno_label="ACC")

h2_Cullis

h2_both <- c(h2,h2_Cullis)
h2_both

heritability <- rbind(heritability, h2_both)
heritability
#dim(heritability)

colnames(heritability) <- c("G variance","E variance", "E2 variance","residual", "h2", "avsed", "H2_Cullis")
heritability

#write.table(line.blup,"TA-BLUP.csv",row.names=F,sep=",")
write.table(heritability,"heritability_L_Puree.csv",row.names=F,col.names=T,sep=",")


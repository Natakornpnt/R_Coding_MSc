
rm(list=ls())

library(rrBLUP)

## LOCATION
setwd("E:/Desktop/MSc/Thesis_analysis/BLUP/GS/GS_ST_Intercept")


time.begin=Sys.time()
time.begin

Markers <- as.matrix(read.table(file="Genotype/Genotype_GS_ST_265_Markers.txt"), header=F)
#head(Markers)
Pheno <-as.matrix(read.table(file ="Transform_Phenotype/Phenotype_ST_256indiv_allPheno_no_outliers_transformed1_GS.txt", header=TRUE))
Pheno <-as.matrix(read.table(file ="Transform_Phenotype/sciencetific_number_traits.txt", header = TRUE))
#head(Pheno)
#Pheno <- Pheno[,-1]
as.numeric(Pheno[-1,])
#n <- nrow(Pheno)
#n
mean(complete.cases(Pheno))
dim(Markers)
dim(Pheno)


impute=A.mat(Markers,max.missing=0.5,impute.method="EM",return.imputed=T)
Markers_impute=impute$imputed
#head(Markers_impute)
dim(Markers_impute)

#write.table(Markers_impute, "Genotype_GS_ST_265_Markers_EM.csv", sep=",", quote=F, col.names=F, row.names=F)


#######
traits = 55
cycles = 1000
accuracy60 = matrix(nrow=cycles, ncol=traits)

for(n in c(1: traits)){
#5=brix
#41=log10FW
for(r in c(1: cycles)){

#test for 265 individuals 
train= as.matrix(sample(1:265, 199))  ##train
test = setdiff(1:265, train)          ##valid
#test1 = as.matrix(sample(test1,27))    ##

Pheno_train=Pheno[train,]
m_train=Markers_impute[train,]
Pheno_valid=Pheno[test,]
m_valid=Markers_impute[test,]

phe01=(Pheno_train[,n])
phe01_answer<-mixed.solve(phe01, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
phe01a = phe01_answer$u
e = as.matrix(phe01a)
pred_phe01_valid =  m_valid %*% e
pred_phe01=c(pred_phe01_valid[,1])+ c(phe01_answer$beta)
pred_phe01
phe01_valid = Pheno_valid[,n]
accuracy60[r,n] <-cor(pred_phe01_valid, phe01_valid, use="complete" )
}
}

write.table(accuracy60, "55traits_1000reps_transformed_GS.txt", sep="\t", quote=F, col.names=F, row.names=T)


time.finish=Sys.time()                                          
time.total=difftime(time.finish, time.begin)
time.total

############################################################

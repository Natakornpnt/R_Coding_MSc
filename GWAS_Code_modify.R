# MLMM for CS_INRA 090914

###############################
#         load data           #
###############################

rm(list=ls())

#location "C://Users//Administrator//Desktop//GWAS//GWAS_Source//"

source("C://Users//Administrator//Desktop//GWAS//GWAS_Source//emma.r")
source("C://Users//Administrator//Desktop//GWAS//GWAS_Source//mlmm_cof_mod.r")
source("C://Users//Administrator//Desktop//GWAS//GWAS_Source//mlmm.r")
load("C://Users//Administrator//Desktop//GWAS//GWAS_Source//example_data_bis.Rdata")

library(matrixcalc)
library(corpcor)


#Location C://Users//Administrator//Desktop//GWAS//Fruit_Weight
C:\Users\Administrator\Desktop\GWAS_CC\Firmness_Crop2_GxP
snp_info = read.table("C://Users//Administrator//Desktop//GWAS_CC//Firmness_Crop2_GxP//Firmness_C2_snp_info_FW.txt", sep="")
dim(snp_info)

ls()

##################
# Load real data #
##################

### GENOTYPES ###
X = read.table("C://Users//Administrator//Desktop//GWAS//GWAS_CC//Firmness_Crop2_GxP//Genotype_GWAS//rrBLUP_FIR_GxP_Crop2_result.txt", sep="", header = T, stringsAsFactors = F)
dim(X)
class(X)
X = as.matrix(X)
class(X)
dim(X)


### KINSHIP MATRIX ###
K = read.table("C://Users//Administrator//Desktop//GWAS//GWAS_CC//Firmness_Crop2_GxP//ibs//Firmness_GxP_C2_ibs.txt", sep="", header = T, stringsAsFactors = F)
#K <- K[ ,-1]       # data frame leaving out first column
dim(K)
K = as.matrix(K)
class(K)
is.positive.definite(K)
K.mod = make.positive.definite(K)
is.positive.definite(K.mod)
class(K.mod)

### STRUCTURE ###

MDS = read.table("C://Users//Administrator//Desktop//GWAS//GWAS_CC//Firmness_Crop2_GxP//structure//343ind_structure.txt", sep="", header = T, stringsAsFactors = F)
dim(MDS)
class(MDS)
MDS = as.matrix(MDS[,2:4])
dim(MDS)
class(MDS)
MDS

#########################
# Run the MLMM Analysis #
#########################


#########
## FW2 ##
#########
Y.mal = read.table("C://Users//Administrator//Desktop//GWAS//GWAS_CC//Firmness_Crop2_GxP//Firmness_Phenotype_GxP_C2.txt", sep="\t", header = T, stringsAsFactors = F)
dim(Y.mal)
class(Y.mal)
Y.mal = as.numeric(Y.mal[,2])
class(Y.mal)
Y.mal

mlmm_cof(Y.mal, X, MDS, K.mod, 10, 100)
mygwas.FW2.MDS = mlmm_cof

par(mfrow = c(3,1))
hist(Y.mal, main = "Fruit_Weight", col="grey", xlab="Fruit_Weight")
plot_opt_GWAS(mygwas.FW2.MDS, opt = c('mbonf'), snp_info, 0.5)
plot_opt_GWAS(mygwas.FW2.MDS, opt = c('extBIC'), snp_info, 0.5)

qqplot_fwd_GWAS(mygwas.FW2.MDS, 3)

par(mfrow = c(2,1))
qqplot_opt_GWAS(mygwas.FW2.MDS,'extBIC')
qqplot_opt_GWAS(mygwas.FW2.MDS,'mbonf')

res = mygwas.FW2.MDS$opt_extBIC$out

write.table(res, "MLMM_cof_6287SNP_FW2_MDS.txt", sep="\t") # , header = T, stringsAsFactors = F)


### Plot RSS
plot_step_RSS(mygwas.FW2.MDS)

## PLOTS FORM THE FORWARD TABLE
par(mfrow = c(2,2))
plot_step_table(mygwas.FW2.MDS, type=c('h2','maxpval','BIC','extBIC'))



#########
## LOC ##
#########
LOC = read.table("Y_SSC.txt", sep="\t", header = T, stringsAsFactors = F)
dim(Y.mal)
class(Y.mal)
Y.mal = as.numeric(Y.mal[,3])
class(Y.mal)
Y.mal

mygwas.SSC.MDS = mlmm_cof(Y.mal, X, MDS, K.mod, 10, 100)

par(mfrow = c(3,1))
hist(Y.mal, main = "Soluble Solid Content", col="grey", xlab="SSC")
plot_opt_GWAS(mygwas.SSC.MDS, opt = c('mbonf'), snp_info, 0.5)
abline(h=3, col="red", lty=3)
plot_opt_GWAS(mygwas.SSC.MDS, opt = c('extBIC'), snp_info, 0.5)
abline(h=3, col="red", lty=3)

qqplot_fwd_GWAS(mygwas.SSC.MDS, 3)

par(mfrow = c(2,1))
qqplot_opt_GWAS(mygwas.SSC.MDS,'extBIC')
qqplot_opt_GWAS(mygwas.SSC.MDS,'mbonf')

res = mygwas.SSC.MDS$opt_extBIC$out

write.table(res, "MLMM_cof_6287SNP_SSC_MDS.txt", sep="\t") # , header = T, stringsAsFactors 


### Plot GWAS in a region of interest

# plot_fwd_region(mygwas,step,snp_info,pval_filt,chrom,pos1,pos2)
par(mfrow=c(1,2))
plot_fwd_region(mygwas.SSC.MDS, 2, snp_info, 5, 5, 500000, 64916620)
plot_fwd_region(mygwas.SSC.MDS, 2, snp_info, 5, 6, 100000, 46000000)


#########
## SUG ##
#########
Y.mal = read.table("Y_SC.txt", sep="\t", header = T, stringsAsFactors = F)
dim(Y.mal)
class(Y.mal)
Y.mal = as.numeric(Y.mal[,2])
class(Y.mal)
Y.mal

mygwas.SUG.MDS = mlmm_cof(Y.mal, X, MDS, K.mod, 10, 100)

par(mfrow = c(3,1))
hist(Y.mal, main = "Sugar Content", col="grey", xlab="SUG")
plot_opt_GWAS(mygwas.SUG.MDS, opt = c('mbonf'), snp_info, 0.5)
abline(h=3, col="red", lty=3)
plot_opt_GWAS(mygwas.SUG.MDS, opt = c('extBIC'), snp_info, 0.5)
abline(h=3, col="red", lty=3)

qqplot_fwd_GWAS(mygwas.SUG.MDS, 3)

par(mfrow = c(2,1))
qqplot_opt_GWAS(mygwas.SUG.MDS,'extBIC')
qqplot_opt_GWAS(mygwas.SUG.MDS,'mbonf')

res = mygwas.SUG.MDS$opt_extBIC$out

write.table(res, "MLMM_cof_6287SNP_SUG_MDS.txt", sep="\t") # , header = T, stringsAsFactors 


### Plot GWAS in a region of interest

# plot_fwd_region(mygwas,step,snp_info,pval_filt,chrom,pos1,pos2)
par(mfrow=c(1,1))
plot_fwd_region(mygwas.SUG.MDS, 2, snp_info, 5, 5, 500000, 64916620)


#########
## TA ##
#########
Y.mal = read.table("Y_TA.txt", sep="\t", header = T, stringsAsFactors = F)
dim(Y.mal)
class(Y.mal)
Y.mal = as.numeric(Y.mal[,2])
class(Y.mal)
Y.mal

mygwas.TA.MDS = mlmm_cof(Y.mal, X, MDS, K.mod, 10, 100)

par(mfrow = c(3,1))
hist(Y.mal, main = "TA", col="grey", xlab="TA")
plot_opt_GWAS(mygwas.TA.MDS, opt = c('mbonf'), snp_info, 0.5)
abline(h=3, col="red", lty=3)
plot_opt_GWAS(mygwas.TA.MDS, opt = c('extBIC'), snp_info, 0.5)
abline(h=3, col="red", lty=3)

qqplot_fwd_GWAS(mygwas.TA.MDS, 3)

par(mfrow = c(2,1))
qqplot_opt_GWAS(mygwas.TA.MDS,'extBIC')
qqplot_opt_GWAS(mygwas.TA.MDS,'mbonf')

res = mygwas.TA.MDS$opt_extBIC$out

write.table(res, "MLMM_cof_6287SNP_TA_MDS.txt", sep="\t") # , header = T, stringsAsFactors 


### Plot GWAS in a region of interest

# plot_fwd_region(mygwas,step,snp_info,pval_filt,chrom,pos1,pos2)
par(mfrow=c(1,1))
plot_fwd_region(mygwas.TA.MDS, 2, snp_info, 5, 4, 35288, 64047061)
plot_fwd_region(mygwas.TA.MDS, 2, snp_info, 5, 3,  71076,64772420)



##############
## FIRMNESS ##
##############
Y.mal = read.table("Y_Firm.txt", sep="\t", header = T, stringsAsFactors = F)
dim(Y.mal)
class(Y.mal)
Y.mal = as.numeric(Y.mal[,2])
class(Y.mal)
Y.mal

mygwas.FIR.MDS = mlmm_cof(Y.mal, X, MDS, K.mod, 10, 100)

par(mfrow = c(3,1))
hist(Y.mal, main = "Fruit Firmness", col="grey", xlab="Firmness")
plot_opt_GWAS(mygwas.FIR.MDS, opt = c('mbonf'), snp_info, 0.5)
abline(h=3, col="red", lty=3)
plot_opt_GWAS(mygwas.FIR.MDS, opt = c('extBIC'), snp_info, 0.5)
abline(h=3, col="red", lty=3)

qqplot_fwd_GWAS(mygwas.FIR.MDS, 3)

par(mfrow = c(2,1))
qqplot_opt_GWAS(mygwas.FIR.MDS,'extBIC')
qqplot_opt_GWAS(mygwas.FIR.MDS,'mbonf')

res = mygwas.FIR.MDS$opt_extBIC$out

write.table(res, "MLMM_cof_6287SNP_FIR_MDS.txt", sep="\t") # , header = T, stringsAsFactors 

##############
## pH ##
##############
Y.mal = read.table("Y_pH.txt", sep="\t", header = T, stringsAsFactors = F)
dim(Y.mal)
class(Y.mal)
Y.mal = as.numeric(Y.mal[,2])
class(Y.mal)
Y.mal

mygwas.pH.MDS = mlmm_cof(Y.mal, X, MDS, K.mod, 10, 100)

par(mfrow = c(3,1))
hist(Y.mal, main = "pH", col="grey", xlab="pH")
plot_opt_GWAS(mygwas.pH.MDS, opt = c('mbonf'), snp_info, 0.5)
abline(h=3, col="red", lty=3)
plot_opt_GWAS(mygwas.pH.MDS, opt = c('extBIC'), snp_info, 0.5)
abline(h=3, col="red", lty=3)

qqplot_fwd_GWAS(mygwas.pH.MDS, 3)

par(mfrow = c(2,1))
qqplot_opt_GWAS(mygwas.pH.MDS,'extBIC')
qqplot_opt_GWAS(mygwas.pH.MDS,'mbonf')

res = mygwas.FIR.MDS$opt_extBIC$out

write.table(res, "MLMM_cof_6287SNP_pH_MDS.txt", sep="\t") # , header = T, stringsAsFactors 





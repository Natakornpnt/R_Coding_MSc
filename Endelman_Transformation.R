### HOW TO PREPARE INPUT FILE FOR rrBLUP ####
# see refernce "Genomic prediction with rrBLUP 4_Endelman.pdf"

# example input is just the genotypes, NO HEADINGS (e.g. AA,AT,TT)
# one row represents one marker
# each column represents each individual
# the output file will be transposed-version of the input file (row=indivs, column=markers)
#############################################################
#Genomic prediction with rrBLUP 4_Endelman

setwd("E:/Desktop/MSc/Thesis_analysis/BLUP/GS")

rm(list=ls())

GBS<-read.csv("Pre_rrBLUP_Fleshy_abL.csv",header=F,as.is=F)

#GBS<-read.csv("D://Desktop//GS_test_FW_Crop2//Pre_rrBLUP_prepare_FW_Crop2.csv", header=F,as.is=F)

dim(GBS)

parse.GBS <-function(x) {
  unique.x <-unique(x)
  alleles<-setdiff(unique.x,union("H","N"))
  y <-rep(0,length(x))
  y[which(x==alleles[1])] <- -1
  y[which(x==alleles[2])] <-1
  y[which(x=="N")] <-NA
  return(y)
}
X <-apply(GBS[],1,parse.GBS)
dim(X)

write.table(X, "Genotype_Endelman_Format_fleshy_abL_351.txt", sep="\t", quote=F, col.names=T, row.names=T)



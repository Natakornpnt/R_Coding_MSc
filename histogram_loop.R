library(tidyverse)

Pheno <-as.matrix(read.table(file ="Phenotype_ST_256indiv_allPheno_no_outliers_transformed1_GS.txt", header=TRUE))

#col_name <- colnames(Pheno)
col_name <- data.frame(col_name = colnames(Pheno))
col_name[1,]
col_name
for (i in 1:3){
  Pheno_1col <- Pheno[,i]
  hist_plot <- ggplot(Pheno_1col, aes(col_name[i,])+
                        geom_histogram())
}

for (i in 1:55) {
  png(filename= paste("hist/hist_", col_name[i,],".png", sep= ""),width=850, height=500)
  Pheno_1col <- Pheno[,i]
  hist_i <- hist(Pheno_1col, main=paste("Histogram ", col_name[i,]), xlab = paste(col_name[i,]), col = "gold")
  print(hist_i)
  dev.off()
}

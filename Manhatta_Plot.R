
rm(list=ls())

library(qqman)

par(mfrow = c(1,1))
#### sig line = 5
QQ_M <-read.table("mlmm_mbonf_LOC_Crop2.txt", head = T)
manhattan(QQ_M, main = "Manhattan Plot Locule_Number_Crop2 (mbonf)", ylim =c(0,8),col = c("blue","red","green"), suggestiveline = F, genomewideline = F, annotatePval = 1.1773E-05 , annotateTop = F )
#manhattan(subset(FW_Crop2, CHR == 11))
abline (h = 4.929112257, col = c("cyan"), lwd = 2)
qq(QQ_M$P)

#### sig line = 4
QQ_M <-  read.table("qqMAN_3traits.txt", head = T)
manhattan(QQ_M, main = "Manhattan Plot Weight_Crop1", ylim =c(0,8),col = c("blue","red","green"), suggestiveline = F, genomewideline = F, annotatePval = 0.000119246 , annotateTop = F )
#manhattan(subset(FW_Crop2, CHR == 11))
abline (h = 3.923554858, col = c("yellow"), lwd = 2)
qq(QQ_M$P)

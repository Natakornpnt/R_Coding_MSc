

rm(list=ls())

#setwd("D://Desktop//GS_LSMEAN")

#ls()
#12traits_1000cycles_285_test_1kreps.txt
ACC = (read.table(file ="Y_TA_1kreps.txt", header=F))

#ACC_Boxplot1 = as.matrix(ACC,)
ACC_Boxplot1<- ACC[,c(2,3,4,5,6)]
ACC_Boxplot2<- ACC[,c(7,8,9,10,11)]
ACC_Boxplot3<- ACC[,c(12,13,14,15,16)]
ACC_Boxplot4<- ACC[,c(17,18,19,20,21)]
ACC_Boxplot5<- ACC[,c(22,23,24,25,26)]
ACC_Boxplot6<- ACC[,c(27,28,29,30,31)]
ACC_Boxplot7<- ACC[,c(32,33,34,35,36)]

#### Boxplot 1
bw.data1 <-boxplot(ACC_Boxplot1, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y1-Y5", names = c("Y1","Y2","Y3", "Y4", "Y5"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0.5,1))
## For bw.data1
Y1 = as.matrix(ACC_Boxplot1[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot1[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot1[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot1[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot1[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

####################################################################
#### Boxplot 2
bw.data2 <-boxplot(ACC_Boxplot2, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y6-Y10", names = c("Y6","Y7","Y8", "Y9", "Y10"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0.2,1))

## For bw.data2
Y1 = as.matrix(ACC_Boxplot2[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot2[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot2[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot2[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot2[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

#################################################################

#### Boxplot3

bw.data3 <-boxplot(ACC_Boxplot3, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y11-Y15", names = c("Y11","Y12","Y13", "Y14", "Y15"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

##For bw.data3
Y1 = as.matrix(ACC_Boxplot3[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot3[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot3[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot3[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot3[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

#############################################################

#### Boxplot 4
bw.data4 <-boxplot(ACC_Boxplot4, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y16-20", names = c("Y16","Y17","Y18", "Y19", "Y20"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

##For bw.data4
Y1 = as.matrix(ACC_Boxplot4[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot4[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot4[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot4[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot4[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

#####################################################################

#### Boxplot 5
bw.data5 <-boxplot(ACC_Boxplot5, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y21-25", names = c("Y21","Y22","Y23", "Y24", "Y25"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.4,1))

##For bw.data5
Y1 = as.matrix(ACC_Boxplot5[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot5[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot5[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot5[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot5[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

####################################################################

#### Boxplot 6

bw.data6 <-boxplot(ACC_Boxplot6, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y26-30", names = c("Y26","Y27","Y28", "Y29", "Y30"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.4,1))

##For bw.data6
Y1 = as.matrix(ACC_Boxplot6[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot6[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot6[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot6[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot6[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

#######################################################################

#### Boxplot 7

bw.data7 <-boxplot(ACC_Boxplot7, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Y31-35", names = c("Y31","Y32","Y33", "Y34", "Y35"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.2,1))

##For bw.data7
Y1 = as.matrix(ACC_Boxplot7[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot7[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot7[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot7[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot7[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

#####################################################################################


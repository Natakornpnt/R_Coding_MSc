
library(openxlsx)
rm(list=ls())

### Set directory
setwd("E:/MSc/Analysis_Part/GS/GS_ST_Intercept_mydata/Transform_Phenotype/result")
#setwd("E:/Desktop/MSc/Thesis_analysis/BLUP/GS/GS_ST_Intercept/results")

### File
ACC = read.xlsx("55traits_1000reps_transformed_GS.xlsx") 


#Boxplot 1 = FW, FIR, LOC
ACC_Boxplot1 <- ACC[,c(1, 2, 3)]

### Boxplot 2 = skin color
ACC_Boxplot2 <- ACC[,c(4, 5, 6)]

##BOxplot 3 = fleshy color
ACC_Boxplot3 <- ACC[,c(7, 8, 9)]

#Boxplot 4 = Puree color with C* h*
ACC_Boxplot4 <- ACC[,10:14]

#Boxplot 5 =  Puree CA pH TSS, respectively
ACC_Boxplot5 <- ACC[,15:17]

#Boxplot 6 = Y1 - Y5
ACC_Boxplot6 <- ACC[, 18:22]

#Boxplot 7 = Y6 - Y10
ACC_Boxplot7 <- ACC[, 23:27]

#Boxplot 8 = Y11 - Y15
ACC_Boxplot8 <- ACC[, 28:32]

#Boxplot 9 = Y16 - Y20
ACC_Boxplot9 <- ACC[, 33:37]

#Boxplot 10 = Y21 - Y25
ACC_Boxplot10 <- ACC[, 38:42]

#Boxplot 11 = Y26 - Y30
ACC_Boxplot11 <- ACC[, 43:47]

#Boxplot 12 = Y31 - Y35
ACC_Boxplot12 <- ACC[, 48:52]

#Boxplot 13 = X1 - X3
ACC_Boxplot13 <- ACC[, 53:55]



## For bw.data1

bw.data1 <-boxplot(ACC_Boxplot1, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP FW FIR LOC", names = c("FW_log10","FIR","LOC_log10"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))
Y1 = as.matrix(ACC_Boxplot1[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot1[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot1[,3])
mean(Y3)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)

## For bw.data2

bw.data2 <-boxplot(ACC_Boxplot2, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Skin color", names = c("Ls","as","bs"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

Y1 = as.matrix(ACC_Boxplot2[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot2[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot2[,3])
mean(Y3)
points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)


## For bw.data3

bw.data3 <-boxplot(ACC_Boxplot3, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Fleshy color", names = c("Lf", "af", "bf"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

Y1 = as.matrix(ACC_Boxplot3[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot3[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot3[,3])
mean(Y3)
points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)



##For bw.data4

bw.data4 <-boxplot(ACC_Boxplot4, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP Puree color ", names = c("Lp_inv","ap","bp_inv", "Cp_noOut_inv", "hp"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.4,1))

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


##For bw.data5

bw.data5 <-boxplot(ACC_Boxplot5, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of BLUP CA pH TSS", names = c("CA_noOut","pH_noOut_inv","TSS"),xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.5,1))

Y1 = as.matrix(ACC_Boxplot5[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot5[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot5[,3])
mean(Y3)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)


##For bw.data6
bw.data6 <-boxplot(ACC_Boxplot6, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of Y1 - Y5 ",xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

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

##For bw.data7
bw.data7 <-boxplot(ACC_Boxplot7, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of Y6 - Y10 ",xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

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

##For bw.data8
bw.data8 <-boxplot(ACC_Boxplot8, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of Y11 - Y15 ",xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

Y1 = as.matrix(ACC_Boxplot8[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot8[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot8[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot8[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot8[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

##For bw.data9
bw.data9 <-boxplot(ACC_Boxplot9, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                   main = "Boxplot of mean Accuracy of Y16 - Y20 ",xlab = "Traits",
                   ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))


Y1 = as.matrix(ACC_Boxplot9[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot9[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot9[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot9[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot9[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

##For bw.data10
bw.data10 <-boxplot(ACC_Boxplot10, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                    main = "Boxplot of mean Accuracy of Y21 - Y25 ",xlab = "Traits",
                    ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.4,1))

Y1 = as.matrix(ACC_Boxplot10[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot10[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot10[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot10[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot10[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

##For bw.data11
bw.data11 <-boxplot(ACC_Boxplot11, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                    main = "Boxplot of mean Accuracy of Y26 - Y30 ",xlab = "Traits",
                    ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.5,1))

Y1 = as.matrix(ACC_Boxplot11[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot11[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot11[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot11[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot11[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

##For bw.data12
bw.data12 <-boxplot(ACC_Boxplot12, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                    main = "Boxplot of mean Accuracy of Y31 - Y35 ",xlab = "Traits",
                    ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(-0.5,1))

Y1 = as.matrix(ACC_Boxplot12[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot12[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot12[,3])
mean(Y3)
Y4 = as.matrix(ACC_Boxplot12[,4])
mean(Y4)
Y5 = as.matrix(ACC_Boxplot12[,5])
mean(Y5)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y4), x = 4, col = "red2", pch = 19 , cex = 1.3)
points(mean(Y5), x = 5, col = "red2", pch = 19 , cex = 1.3)

##For bw.data13
bw.data13 <-boxplot(ACC_Boxplot13, col =  c("palegreen","lightgoldenrodyellow","lightcyan"), 
                    main = "Boxplot of mean Accuracy of BLUP X1 X2 X3",xlab = "Traits",
                    ylab = "mean_accuracy", pch = 16,cex.axis = 1,cex.lab=1.5, cex.main = 2, ylim=c(0,1))

Y1 = as.matrix(ACC_Boxplot13[,1])
mean(Y1)
Y2 = as.matrix(ACC_Boxplot13[,2])
mean(Y2)
Y3 = as.matrix(ACC_Boxplot13[,3])
mean(Y3)

points(mean(Y1), x = 1, col = "red2", pch = 19, cex = 1.3)
points(mean(Y2), x = 2 ,col = "red2", pch = 19, cex = 1.3)
points(mean(Y3), x = 3, col = "red2", pch = 19 , cex = 1.3)



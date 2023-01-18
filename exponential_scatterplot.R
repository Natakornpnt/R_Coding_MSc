rm(list=ls())

library(tidyverse)
library(caret)
library(openxlsx)
library(car)
windowsFonts(A = windowsFont("Times New Roman")) 

#puree_txt<-read.table("divided_lyc_puree.txt", header = TRUE)
puree <-read.xlsx("divided_lyc_puree.xlsx")
attach(puree)
attach(puree_txt)
head(puree)
dim(puree)

######Test R square
puree_txt_L_linear<-lm(lycopene_1g ~ L_puree, data=puree_txt)
puree_L_linear<-lm(lycopene_1g ~ L_puree, data=puree)
summary(puree_txt_L_linear)
summary(puree_L_linear)

L_exp <-lm (log(lycopene_1g) ~ L_puree, data = puree)
L_txt_exp<-lm (log(lycopene_1g) ~ L_puree, data = puree_txt)
summary(L_exp)
summary(L_txt_exp)
#########################################################

##############
# lycopene best fit
# L* = exp
# a* = exp
# b* = exp
# a*2 = exp
# a*4 = exp
# a/b = exp
# (a*/b*)2 = linear
# (a*/b*)2.5 = linear
# C* = exp
# h* = exp
# sqrt - h* = exp
# TCI = exp
#############

#L_puree lycopene exponential
L_exp = lm (log(lycopene_1g) ~ L_puree, data = puree)
L_exp_R = round(summary(L_exp)$r.squared,3)
summary(L_exp)
L_exp_R

L_factor_exp = round(L_exp$coefficients["L_puree"], 3)
L_factor_exp

L_coeff_exp = round(L_exp$coefficients["(Intercept)"], 3)
L_coeff_exp_e = exp(L_coeff_exp)
L_coeff_exp
L_coeff_exp_e

plot(lycopene_1g ~ L_puree, pch = 19, col = "black", family = "A",xlab = "L*"  ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(L_coeff_exp)*(exp(L_factor_exp)^x),15,35, add =TRUE, col="blue", lwd = 5)
text(28, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=L_factor_exp, B= L_coeff_exp_e)), family ="A")
text(28,18, labels = substitute ("R"*""^2* " = " *X, list(X=L_exp_R)), family="A")

######################################################################################

#a_puree lycopene exponential
a_exp <-lm (log(lycopene_1g) ~ a_puree, data = puree)
a_linear<-lm(lycopene_1g~ a_puree, data = puree)
summary(a_linear)
a_exp_R = round(summary(a_exp)$r.squared,3)
summary(a_exp)
a_exp_R

a_factor_exp = round(a_exp$coefficients["a_puree"], 3)
a_factor_exp

a_coeff_exp = round(a_exp$coefficients["(Intercept)"], 3)
a_coeff_exp_e = round(exp(a_coeff_exp), 3)
a_coeff_exp
a_coeff_exp_e


plot(lycopene_1g ~ a_puree, pch = 19, col = "black", family = "A", xlab = "a*" ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(a_coeff_exp)*(exp(a_factor_exp)^x),4,18, add =TRUE, col="blue", lwd = 5)
text(8, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=a_factor_exp, B= a_coeff_exp_e)), family ="A")
text(8,18, labels = substitute ("R"*""^2* " = " *X, list(X=a_exp_R)), family="A")

######################################################################################

#b_puree lycopene exponential
b_exp <-lm (log(lycopene_1g) ~ b_puree, data = puree)
b_linear<-lm(lycopene_1g~ b_puree, data = puree)
summary(b_linear)
b_exp_R = round(summary(b_exp)$r.squared,3)
summary(b_exp)
b_exp_R

b_factor_exp = round(b_exp$coefficients["b_puree"], 3)
b_factor_exp

b_coeff_exp = round(b_exp$coefficients["(Intercept)"], 3)
b_coeff_exp_e = round(exp(b_coeff_exp), 3)
b_coeff_exp
b_coeff_exp_e

plot(lycopene_1g ~ b_puree, pch = 19, col = "black", family = "A", xlab = "b*" ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(b_coeff_exp)*(exp(b_factor_exp)^x),10,40, add =TRUE, col="blue", lwd = 5)
text(27.5, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=b_factor_exp, B= b_coeff_exp_e)), family ="A")
text(27.5,18, labels = substitute ("R"*""^2* " = " *X, list(X=b_exp_R)), family="A")

######################################################################################

#a power2 puree lycopene exponential
ap2_exp <-lm (log(lycopene_1g) ~ ap2, data = puree)
ap2_linear<-lm(lycopene_1g~ ap2, data = puree)
summary(ap2_linear)
ap2_exp_R = round(summary(ap2_exp)$r.squared,3)
summary(ap2_exp)
ap2_exp_R

ap2_factor_exp = round(ap2_exp$coefficients["ap2"], 3)
ap2_factor_exp

ap2_coeff_exp = round(ap2_exp$coefficients["(Intercept)"], 3)
ap2_coeff_exp_e = round(exp(ap2_coeff_exp), 3)
ap2_coeff_exp
ap2_coeff_exp_e


plot(lycopene_1g ~ ap2, pch = 19, col = "black", family = "A", xlab = substitute ("a*" ^2) ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(ap2_coeff_exp)*(exp(ap2_factor_exp)^x),0,250, add =TRUE, col="blue", lwd = 5)
text(75, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=ap2_factor_exp, B= ap2_coeff_exp_e)), family ="A")
text(75,18, labels = substitute ("R"*""^2* " = " *X, list(X=ap2_exp_R)), family="A")

######################################################################################

#a power4 puree lycopene exponential
ap4_exp <-lm (log(lycopene_1g) ~ ap4, data = puree)
ap4_linear<-lm(lycopene_1g~ ap4, data = puree)
summary(ap4_linear)
ap4_exp_R = round(summary(ap4_exp)$r.squared,3)
summary(ap4_exp)
ap4_exp_R

ap4_factor_exp = round(ap4_exp$coefficients["ap4"], 10)
ap4_factor_exp

ap4_coeff_exp = round(ap4_exp$coefficients["(Intercept)"], 3)
ap4_coeff_exp_e = round(exp(ap4_coeff_exp), 3)
ap4_coeff_exp
ap4_coeff_exp_e


plot(lycopene_1g ~ ap4, pch = 19, col = "black", family = "A", xlab = substitute ("a*" ^4) ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(ap4_coeff_exp)*(exp(ap4_factor_exp)^x),-10000,70000, add =TRUE, col="blue", lwd = 5)
text(15000, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=ap4_factor_exp, B= ap4_coeff_exp_e)), family ="A")
text(15000,18, labels = substitute ("R"*""^2* " = " *X, list(X=ap4_exp_R)), family="A")

######################################################################################

#a/b puree lycopene exponential
a_b_exp <-lm (log(lycopene_1g) ~ a_b, data = puree)
a_b_linear<-lm(lycopene_1g~ a_b, data = puree)
summary(a_b_linear)
a_b_exp_R = round(summary(a_b_exp)$r.squared,3)
summary(a_b_exp)
a_b_exp_R

a_b_factor_exp = round(a_b_exp$coefficients["a_b"], 3)
a_b_factor_exp

a_b_coeff_exp = round(a_b_exp$coefficients["(Intercept)"], 3)
a_b_coeff_exp_e = round(exp(a_b_coeff_exp), 3)
a_b_coeff_exp
a_b_coeff_exp_e


plot(lycopene_1g ~ a_b, pch = 19, col = "black", family = "A", xlab = ("(a*/b*)") ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(a_b_coeff_exp)*(exp(a_b_factor_exp)^x),0,5, add =TRUE, col="blue", lwd = 5)
text(0.5, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=a_b_factor_exp, B= a_b_coeff_exp_e)), family ="A")
text(0.5,18, labels = substitute ("R"*""^2* " = " *X, list(X=a_b_exp_R)), family="A")

######################################################################################

#a/b power 2 puree lycopene exponential
a_bp2_exp <-lm (log(lycopene_1g) ~ a_bp2, data = puree)
a_bp2_linear<-lm(lycopene_1g~ a_bp2, data = puree)
summary(a_bp2_linear)
a_bp2_exp_R = round(summary(a_bp2_exp)$r.squared,3)
summary(a_bp2_exp)
a_bp2_exp_R

a_bp2_factor_exp = round(a_bp2_exp$coefficients["a_bp2"], 3)
a_bp2_factor_exp

a_bp2_coeff_exp = round(a_bp2_exp$coefficients["(Intercept)"], 3)
a_bp2_coeff_exp_e = round(exp(a_bp2_coeff_exp), 3)
a_bp2_coeff_exp
a_bp2_coeff_exp_e


plot(lycopene_1g ~ a_bp2, pch = 19, col = "black", family = "A", xlab = substitute ("(a*/b*)" ^2) ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(a_bp2_coeff_exp)*(exp(a_bp2_factor_exp)^x),0,5, add =TRUE, col="blue", lwd = 5)
text(1.2, 10, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=a_bp2_factor_exp, B= a_bp2_coeff_exp_e)), family ="A")
text(1.2, 8, labels = substitute ("R"*""^2* " = " *X, list(X=a_bp2_exp_R)), family="A")

######################################################################################

#a/b power 2.5 puree lycopene exponential
a_bp2.5_exp <-lm (log(lycopene_1g) ~ a_bp2.5, data = puree)
a_bp2.5_linear<-lm(lycopene_1g~ a_bp2.5, data = puree)
summary(a_bp2.5_linear)
a_bp2.5_exp_R = round(summary(a_bp2.5_exp)$r.squared,3)
summary(a_bp2.5_exp)
a_bp2.5_exp_R

a_bp2.5_factor_exp = round(a_bp2.5_exp$coefficients["a_bp2.5"], 3)
a_bp2.5_factor_exp

a_bp2.5_coeff_exp = round(a_bp2.5_exp$coefficients["(Intercept)"], 3)
a_bp2.5_coeff_exp_e = round(exp(a_bp2.5_coeff_exp), 3)
a_bp2.5_coeff_exp
a_bp2.5_coeff_exp_e

plot(lycopene_1g ~ a_bp2.5, pch = 19, col = "black", family = "A", xlab = substitute ("(a*/b*)" ^2.5) ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(a_bp2.5_coeff_exp)*(exp(a_bp2.5_factor_exp)^x),-2,2, add =TRUE, col="blue", lwd = 5)
text(1.25, 10, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=a_bp2.5_factor_exp, B= a_bp2.5_coeff_exp_e)), family ="A")
text(1.25, 8, labels = substitute ("R"*""^2* " = " *X, list(X=a_bp2.5_exp_R)), family="A")

######################################################################################

#Chroma puree lycopene exponential
Chroma_exp <-lm (log(lycopene_1g) ~ Chroma, data = puree)
Chroma_linear<-lm(lycopene_1g~ Chroma, data = puree)
summary(Chroma_linear)
Chroma_exp_R = round(summary(Chroma_exp)$r.squared,3)
summary(Chroma_exp)
Chroma_exp_R

Chroma_factor_exp = round(Chroma_exp$coefficients["Chroma"], 3)
Chroma_factor_exp

Chroma_coeff_exp = round(Chroma_exp$coefficients["(Intercept)"], 3)
Chroma_coeff_exp_e = round(exp(Chroma_coeff_exp), 3)
Chroma_coeff_exp
Chroma_coeff_exp_e


plot(lycopene_1g ~ Chroma, pch = 19, col = "black", family = "A", xlab = ("C*") ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(Chroma_coeff_exp)*(exp(Chroma_factor_exp)^x),15,35, add =TRUE, col="blue", lwd = 5)
text(30, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=Chroma_factor_exp, B= Chroma_coeff_exp_e)), family ="A")
text(30,18, labels = substitute ("R"*""^2* " = " *X, list(X=Chroma_exp_R)), family="A")

######################################################################################

#hue puree lycopene exponential
hue_exp <-lm (log(lycopene_1g) ~ hue, data = puree)
hue_linear<-lm(lycopene_1g~ hue, data = puree)
summary(hue_linear)
hue_exp_R = round(summary(hue_exp)$r.squared,3)
summary(hue_exp)
hue_exp_R

hue_factor_exp = round(hue_exp$coefficients["hue"], 3)
hue_factor_exp

hue_coeff_exp = round(hue_exp$coefficients["(Intercept)"], 3)
hue_coeff_exp_e = round(exp(hue_coeff_exp), 3)
hue_coeff_exp
hue_coeff_exp_e


plot(lycopene_1g ~ hue, pch = 19, col = "black", family = "A", xlab = ("h*") ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(hue_coeff_exp)*(exp(hue_factor_exp)^x),35,85, add =TRUE, col="blue", lwd = 5)
text(70, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=hue_factor_exp, B= hue_coeff_exp_e)), family ="A")
text(70,18, labels = substitute ("R"*""^2* " = " *X, list(X=hue_exp_R)), family="A")

######################################################################################

#sqrt_h puree lycopene exponential
sqrt_h_exp <-lm (log(lycopene_1g) ~ sqrt_h, data = puree)
sqrt_h_linear<-lm(lycopene_1g~ sqrt_h, data = puree)
summary(sqrt_h_linear)
sqrt_h_exp_R = round(summary(sqrt_h_exp)$r.squared,3)
summary(sqrt_h_exp)
sqrt_h_exp_R

sqrt_h_factor_exp = round(sqrt_h_exp$coefficients["sqrt_h"], 3)
sqrt_h_factor_exp

sqrt_h_coeff_exp = round(sqrt_h_exp$coefficients["(Intercept)"], 3)
sqrt_h_coeff_exp_e = round(exp(sqrt_h_coeff_exp), 3)
sqrt_h_coeff_exp
sqrt_h_coeff_exp_e


plot(lycopene_1g ~ sqrt_h, pch = 19, col = "black", family = "A", xlab = expression(sqrt("h*")) ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(sqrt_h_coeff_exp)*(exp(sqrt_h_factor_exp)^x),5,10, add =TRUE, col="blue", lwd = 5)
text(8, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=sqrt_h_factor_exp, B= sqrt_h_coeff_exp_e)), family ="A")
text(8,18, labels = substitute ("R"*""^2* " = " *X, list(X=sqrt_h_exp_R)), family="A")

######################################################################################

#TCI puree lycopene exponential
TCI_exp <-lm (log(lycopene_1g) ~ TCI, data = puree)
TCI_linear<-lm(lycopene_1g~ TCI, data = puree)
summary(TCI_linear)
TCI_exp_R = round(summary(TCI_exp)$r.squared,3)
summary(TCI_exp)
TCI_exp_R

TCI_factor_exp = round(TCI_exp$coefficients["TCI"], 3)
TCI_factor_exp

TCI_coeff_exp = round(TCI_exp$coefficients["(Intercept)"], 3)
TCI_coeff_exp_e = round(exp(TCI_coeff_exp), 3)
TCI_coeff_exp
TCI_coeff_exp_e


plot(lycopene_1g ~ TCI, pch = 19, col = "black", family = "A", xlab = ("TCI") ,ylab = expression(paste("lycopene ( ", mu, "g/g)")))
curve(exp(TCI_coeff_exp)*(exp(TCI_factor_exp)^x),10,100, add =TRUE, col="blue", lwd = 5)
text(25, 20, labels = substitute("y = " *B *"*e" ^A *"" ^x, list(A=TCI_factor_exp, B= TCI_coeff_exp_e)), family ="A")
text(25, 18, labels = substitute ("R"*""^2* " = " *X, list(X=TCI_exp_R)), family="A")

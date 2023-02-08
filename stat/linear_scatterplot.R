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

########################################################################################
#L_puree lycopene

L_linear = lm (lycopene_1g ~ L_puree, data = puree)
L_R = round(summary (L_linear)$r.squared, 3)
summary(L_linear)
L_R
L_factor = round(L_linear$coefficients["L_puree"], 3)
L_factor
L_coeff = round(L_linear$coefficients["(Intercept)"], 3)
L_coeff

plot(lycopene_1g ~ L_puree, pch = 19, col = "black", family = "A", xlab = "L*", ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ L_puree), col = "red", lwd = 5)
text (28, 20, labels = substitute ("y = " *A *" " *B *"" *"x", list(A=L_coeff, B=L_factor)), family ="A")
text(28,18, labels = substitute ("R"*""^2* " = " *X, list(X=L_R)), family="A")

########################################################################################
#a_puree lycopene

a_linear = lm (lycopene_1g ~ a_puree, data = puree)
a_R = round(summary (a_linear)$r.squared, 3)
summary(a_linear)
a_R
a_factor = round(a_linear$coefficients["a_puree"], 3)
a_factor
a_coeff = round(a_linear$coefficients["(Intercept)"], 3)
a_coeff

plot(lycopene_1g ~ a_puree, pch = 19, col = "black", family = "A", xlab = "a*", ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ a_puree), col = "red", lwd = 5)
text (6, 20, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=a_coeff, B=a_factor)), family ="A")
text(6, 18, labels = substitute ("R"*""^2* " = " *X, list(X=a_R)), family="A")

########################################################################################
#b_puree lycopene

b_linear = lm (lycopene_1g ~ b_puree, data = puree)
b_R = round(summary (b_linear)$r.squared, 3)
summary(b_linear)
b_R
b_factor = round(b_linear$coefficients["b_puree"], 3)
b_factor
b_coeff = round(b_linear$coefficients["(Intercept)"], 3)
b_coeff

plot(lycopene_1g ~ b_puree, pch = 19, col = "black", family = "A", xlab = "b*", ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ b_puree), col = "red", lwd = 5)
text (30, 20, labels = substitute ("y = " *A *" " *B *"" *"x", list(A=b_coeff, B=b_factor)), family ="A")
text(30, 18, labels = substitute ("R"*""^2* " = " *X, list(X=b_R)), family="A")

########################################################################################
#ap2_puree lycopene

ap2_linear = lm (lycopene_1g ~ ap2, data = puree)
ap2_R = round(summary (ap2_linear)$r.squared, 3)
summary(ap2_linear)
ap2_R
ap2_factor = round(ap2_linear$coefficients["ap2"], 3)
ap2_factor
ap2_coeff = round(ap2_linear$coefficients["(Intercept)"], 3)
ap2_coeff

plot(lycopene_1g ~ ap2, pch = 19, col = "black", family = "A", xlab = substitute("a*" ^2), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ ap2), col = "red", lwd = 5)
text (50, 20, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=ap2_coeff, B=ap2_factor)), family ="A")
text(50, 18, labels = substitute ("R"*""^2* " = " *X, list(X=ap2_R)), family="A")
########################################################################################
#ap4_puree lycopene

ap4_linear = lm (lycopene_1g ~ ap4, data = puree)
ap4_R = round(summary (ap4_linear)$r.squared, 3)
summary(ap4_linear)
ap4_R
ap4_factor = round(ap4_linear$coefficients["ap4"], 5)
ap4_factor
ap4_coeff = round(ap4_linear$coefficients["(Intercept)"], 3)
ap4_coeff

plot(lycopene_1g ~ ap4, pch = 19, col = "black", family = "A", xlab = substitute("a*" ^4), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ ap4), col = "red", lwd = 5)
text (50000, 10, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=ap4_coeff, B=ap4_factor)), family ="A")
text(50000, 8, labels = substitute ("R"*""^2* " = " *X, list(X=ap4_R)), family="A")

########################################################################################
#a_b_puree lycopene

a_b_linear = lm (lycopene_1g ~ a_b, data = puree)
a_b_R = round(summary (a_b_linear)$r.squared, 3)
summary(a_b_linear)
a_b_R
a_b_factor = round(a_b_linear$coefficients["a_b"], 3)
a_b_factor
a_b_coeff = round(a_b_linear$coefficients["(Intercept)"], 3)
a_b_coeff

plot(lycopene_1g ~ a_b, pch = 19, col = "black", family = "A", xlab = substitute("(a*/b*)"), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ a_b), col = "red", lwd = 5)
text (0.4, 20, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=a_b_coeff, B=a_b_factor)), family ="A")
text(0.4, 18, labels = substitute ("R"*""^2* " = " *X, list(X=a_b_R)), family="A")

########################################################################################
#a_bp2_puree lycopene

a_bp2_linear = lm (lycopene_1g ~ a_bp2, data = puree)
a_bp2_R = round(summary (a_bp2_linear)$r.squared, 3)
summary(a_bp2_linear)
a_bp2_R
a_bp2_factor = round(a_bp2_linear$coefficients["a_bp2"], 3)
a_bp2_factor
a_bp2_coeff = round(a_bp2_linear$coefficients["(Intercept)"], 3)
a_bp2_coeff

plot(lycopene_1g ~ a_bp2, pch = 19, col = "black", family = "A", xlab = substitute("(a*/b*)"^2), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ a_bp2), col = "red", lwd = 5)
text (1.2, 10, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=a_bp2_coeff, B=a_bp2_factor)), family ="A")
text(1.2, 8, labels = substitute ("R"*""^2* " = " *X, list(X=a_bp2_R)), family="A")
head(puree)

########################################################################################
#a_bp2.5_puree lycopene

a_bp2.5_linear = lm (lycopene_1g ~ a_bp2.5, data = puree)
a_bp2.5_R = round(summary (a_bp2.5_linear)$r.squared, 3)
summary(a_bp2.5_linear)
a_bp2.5_R
a_bp2.5_factor = round(a_bp2.5_linear$coefficients["a_bp2.5"], 3)
a_bp2.5_factor
a_bp2.5_coeff = round(a_bp2.5_linear$coefficients["(Intercept)"], 3)
a_bp2.5_coeff

plot(lycopene_1g ~ a_bp2.5, pch = 19, col = "black", family = "A", xlab = substitute("(a*/b*)"^2.5), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ a_bp2.5), col = "red", lwd = 5)
text (1.25, 10, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=a_bp2.5_coeff, B=a_bp2.5_factor)), family ="A")
text(1.25, 8, labels = substitute ("R"*""^2* " = " *X, list(X=a_bp2.5_R)), family="A")

########################################################################################
#Chroma_puree lycopene

Chroma_linear = lm (lycopene_1g ~ Chroma, data = puree)
Chroma_R = round(summary (Chroma_linear)$r.squared, 3)
summary(Chroma_linear)
Chroma_R
Chroma_factor = round(Chroma_linear$coefficients["Chroma"], 3)
Chroma_factor
Chroma_coeff = round(Chroma_linear$coefficients["(Intercept)"], 3)
Chroma_coeff

plot(lycopene_1g ~ Chroma, pch = 19, col = "black", family = "A", xlab = substitute("C*"), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ Chroma), col = "red", lwd = 5)
text (31, 20, labels = substitute ("y = " *A *" " *B *"" *"x", list(A=Chroma_coeff, B=Chroma_factor)), family ="A")
text(31, 18, labels = substitute ("R"*""^2* " = " *X, list(X=Chroma_R)), family="A")

########################################################################################
#hue_puree lycopene

hue_linear = lm (lycopene_1g ~ hue, data = puree)
hue_R = round(summary (hue_linear)$r.squared, 3)
summary(hue_linear)
hue_R
hue_factor = round(hue_linear$coefficients["hue"], 3)
hue_factor
hue_coeff = round(hue_linear$coefficients["(Intercept)"], 3)
hue_coeff

plot(lycopene_1g ~ hue, pch = 19, col = "black", family = "A", xlab = substitute("h*"), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ hue), col = "red", lwd = 5)
text (70, 20, labels = substitute ("y = " *A *" " *B *"" *"x", list(A=hue_coeff, B=hue_factor)), family ="A")
text(70, 18, labels = substitute ("R"*""^2* " = " *X, list(X=hue_R)), family="A")

########################################################################################
#sqrt_h_puree lycopene

sqrt_h_linear = lm (lycopene_1g ~ sqrt_h, data = puree)
sqrt_h_R = round(summary (sqrt_h_linear)$r.squared, 3)
summary(sqrt_h_linear)
sqrt_h_R
sqrt_h_factor = round(sqrt_h_linear$coefficients["sqrt_h"], 3)
sqrt_h_factor
sqrt_h_coeff = round(sqrt_h_linear$coefficients["(Intercept)"], 3)
sqrt_h_coeff

plot(lycopene_1g ~ sqrt_h, pch = 19, col = "black", family = "A", xlab = expression(sqrt("h*")), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ sqrt_h), col = "red", lwd = 5)
text (8.25, 20, labels = substitute ("y = " *A *" " *B *"" *"x", list(A=sqrt_h_coeff, B=sqrt_h_factor)), family ="A")
text(8.25, 18, labels = substitute ("R"*""^2* " = " *X, list(X=sqrt_h_R)), family="A")

########################################################################################
#TCI_puree lycopene

TCI_linear = lm (lycopene_1g ~ TCI, data = puree)
TCI_R = round(summary (TCI_linear)$r.squared, 3)
summary(TCI_linear)
TCI_R
TCI_factor = round(TCI_linear$coefficients["TCI"], 3)
TCI_factor
TCI_coeff = round(TCI_linear$coefficients["(Intercept)"], 3)
TCI_coeff

plot(lycopene_1g ~ TCI, pch = 19, col = "black", family = "A", xlab = substitute("TCI"), ylab = expression(paste("lycopene ( ", mu, "g/g)")))#, main ="L")
abline(lm(lycopene_1g ~ TCI), col = "red", lwd = 5)
text (25, 20, labels = substitute ("y = " *A *" + " *B *"" *"x", list(A=TCI_coeff, B=TCI_factor)), family ="A")
text(25, 18, labels = substitute ("R"*""^2* " = " *X, list(X=TCI_R)), family="A")

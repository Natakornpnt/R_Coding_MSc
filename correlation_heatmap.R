#install.packages("lattice")
library(lattice)
library(tidyverse)
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")
names(wf[wf=="TT Times New Roman"])

dat<-read.table("Correlation_Heatmap_ls_BT.txt", header =T)
head(dat)
A=windowsFonts(A = windowsFont("Times New Roman"))
corr_mat<-round(cor(dat),2)
head(corr_mat)
corr_mat[lower.tri(corr_mat)]<-NA

#install.packages("reshape2")
library(reshape2)

melted_corr_mat<-melt(corr_mat)

ggcorplot(corr_mat)

library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1,
                                   y=Var2,
                                   fill=value)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

ggplot(data = melted_corr_mat , aes(Var1, ordered(Var2, levels =     rev(sort(unique(Var2)))))) +   
  geom_tile(aes(fill = value))+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_text(aes(Var1, ordered(Var2, levels =   rev(sort(unique(Var2)))), label = value),
            color = "white", size = 4)+
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))
#############################
ggplot_heatmap <-ggplot(data = melted_corr_mat , aes(Var1, ordered(Var2, levels =     rev(sort(unique(Var2)))))) +   
          geom_tile(aes(fill = value))+
          #scale_x_discrete(guide = guide_axis(angle = 45))+
          geom_text(aes(Var1, ordered(Var2, levels =   rev(sort(unique(Var2)))), label = value),
                      color = "black", size = 4, family = "serif")+
          scale_fill_gradient2(low = "red",
                      high = "blue",
                      mid="white", midpoint =0, limit=c(-1,1), space = "Lab", name ="Pearson\nCorrelation")+
          theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
          coord_fixed()+
          theme(text=element_text(size=16, family ="serif"))+
          #theme_minimal()+
          labs(x = NULL, y = NULL) + 
          theme(panel.background = element_blank())
print(ggplot_heatmap)
#####################################################
install.packages("plotly")
library(plotly)
install.packages("ggcorrplot")
library(ggcorrplot)

corr_mat <- round(cor(dat),3)
p_mat <- cor_pmat(dat)

corr_mat <- ggcorrplot(
  corr_mat, hc.order = TRUE, type = "lower",
  outline.col = "white",
  p.mat = p_mat
)

ggplotly(corr_mat)
#####################################################

data = dat
C<-dat %>% 
  cor()
C<- C %>%
  as.data.frame()%>%
mutate(variable =row.names(.))

C_Long <- pivot_longer(C, values_drop_na = TRUE) %>%
  mutate(Variable = factor(Variable),
         name = factor(name)) %>%
  
  mutate(Variable = factor(Variable, levels = rev(levels(.$Variable))))

ggplot(C_Long,
       # Our x and y axis are Variable and name
       # And we want to fill each cell with the value
       aes(x = Variable, y = name, fill = value))+
  # geom_tile to draw the graph
  geom_tile() +
  # Color the graph as we like
  # Here our negative correlations are red, positive are blue
  # gradient2 instead of gradient gives us a "mid" color which we can make white
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation")

#loading library
library(readxl)
library(tidyverse)
library(data.table)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)

#load data obligasi
data_obligasi <- read_excel("Data-EWS40Issuer.xlsx")

#pca
pca <- prcomp(data_obligasi[,3:24], scale = TRUE)

#hitung eigen value & tentukan berapa dimensi yang akan digunakan
eig.val<-get_eigenvalue(pca)
fviz_eig(pca)

res.pca <- PCA(data_obligasi[,3:24],  graph = FALSE)
get_eig(res.pca)


#data di reduksi jadi 6 variabel aja --> lihat dari eigen value nya mendekati 1
final.pca<-PCA(data_obligasi[,3:24],ncp=6)

#fff<-final.pca$ind$coord
#fff$rating<-data_obligasi[,5]

#hasil reduksi dari 27 data jadi 5, jadi kek begini
fff<-as.data.frame(final.pca$ind$coord)
fff$rating<-as.data.frame.array(data_obligasi[,25])

fff<-as.data.frame(as.matrix(fff))

#terus di GLM wae
general_lin<-glm(rating ~ Dim.1 + Dim.2 + Dim.3+Dim.4+Dim.5+Dim.6, data = fff)
predict(general_lin, type="response")

summary(general_lin)

predicted_category<-ceiling(general_lin[["fitted.values"]])

df <- data.frame(data_obligasi,predicted_category)
df<-df[order(-df$Rating), ]
df<-data.frame(df,c(1:40))

rownames(df) <- NULL
#ggplot(df)

# ggplot(data=df,aes(x=Cod_2,y=predicted_category))+
#   geom_point(aes(y=predicted_category), colour="red")
# 
# ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = as.factor(cyl))) + 
#   geom_point()
# 
# ggplot(data = df, mapping = aes(x = Issuer, y = predicted_category, color = as.factor(predicted_category))) + 
#   geom_point()
# ggg<-filter(df,Cod_2==5)

ggplot(data = df, mapping = aes(x = as.factor(c.1.40.), y = predicted_category, color = as.factor(Rating))) + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x ="Nomor Obligasi", y = "Predicted Rating",color = "Initial Rating")+
  geom_point(aes(size = 2))

#ffff<-

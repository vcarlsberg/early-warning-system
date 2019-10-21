#loading library
library(readxl)
library(tidyverse)
library(data.table)
library(factoextra)
library(FactoMineR)
library(ggplot2)

#load data obligasi
data_obligasi <- read_excel("Data_Obligasi.xlsx")

#pca
pca <- prcomp(data_obligasi[,6:27], scale = TRUE)

#hitung eigen value & tentukan berapa dimensi yang akan digunakan (dhi. pake 5-7 berdasarkan eigenvalue yang mendekati 1 (pake 7) or pareto 80:20 (pake 5))
eig.val<-get_eigenvalue(pca)
fviz_eig(pca)

res.pca <- PCA(data_obligasi[,6:27],  graph = FALSE)
get_eig(res.pca)


#data di reduksi jadi 5 variabel aja
final.pca<-PCA(data_obligasi[,6:27],ncp=5)

#fff<-final.pca$ind$coord
#fff$rating<-data_obligasi[,5]

#hasil reduksi dari 27 data jadi 5, jadi kek begini
fff<-as.data.frame(final.pca$ind$coord)
fff$rating<-as.data.frame.array(data_obligasi[,5])

fff<-as.data.frame(as.matrix(fff))

#terus di GLM wae
general_lin<-glm(fff$rating~
                   fff$Dim.1+fff$Dim.2+fff$Dim.3+fff$Dim.4+fff$Dim.5)
summary(general_lin)

predicted_category<-ceiling(general_lin[["fitted.values"]])

df <- data.frame(data_obligasi,predicted_category)
df<-df[order(-df$Cod_2), ]

#ggplot(df)

ggplot(data=df,aes(x=Cod_2,y=predicted_category))+
  geom_point()
+
  labs(y="Transaction count")

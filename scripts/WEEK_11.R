#load and prepare the dataset
data(iris)
# use only numeric variables
iris_num<-iris[,1:4]
#compute correlation matrix
corr_matrix<-cor(iris_num)
corr_matrix
#basic correlation heatmap (base R)
heatmap(
  corr_matrix,
  col=colorRampPalette(c("blue","white","red"))(100),
  main="Correlation heatmap of iris dataset"
)
#heatmap with correlation values
heatmap(
  corr_matrix,
  col=colorRampPalette(c("blue","white","red"))(100),
  Rowv=NA,
  Colv=NA,
  scale="none",
  main="Correlation heatmap of iris dataset"
)

text(
  rep(1:ncol(corr_matrix),each=nrow(corr_matrix)),
  rep(nrow(corr_matrix):1,ncol(corr_matrix)),
  round(corr_matrix,2)
)

#correlation using 'corrplot' (Most popular)
#install package
install.packages("corrplot")
library(corrplot)
#basic correlogram
corrplot(corr_matrix)
#coloured circle correlogram
corrplot(
  corr_matrix,
  method="circle",
  type="upper",
  tl.col="black"
)
#correlogram with correlation numbers
corrplot(
  corr_matrix,
  method="number",
  type="upper"
)

#displays correlation coefficients directly.
#mixed correlogram
corrplot.mixed(
  corr_matrix,
  lower="circle",
  upper="number"
)
# ggplot2-style correlogram
#convert correlation matrix to long format
library(reshape2)
library(ggplot2)
corr_long<-melt(corr_matrix)
ggplot(corr_long,aes(Var1,Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(
    low="blue",
    mid="white",
    high="red",
    midpoint=0
  )+
  theme_minimal()+
  labs(title="Correlogram of iris dataset")

#correlogram with values
ggplot(corr_long,aes(Var1,Var2,fill=value))+
  geom_tile()+
  geom_text(aes(label=round(value,2)))+
  scale_fill_gradient2(
    low="blue",
    mid="white",
    high="red",
    midpoint=0
  )+
  theme_minimal()

#advanced correlogram using GGally
install.packages("GGally")
library(GGally)
ggcorr(
  iris_num,
  label=TRUE,
  label_round=2
)

#advanced scatter correlogram
library(GGally)
ggpairs(
  iris,
  columns=1:4,
  aes(color=Species)
)
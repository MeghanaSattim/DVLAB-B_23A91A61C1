#load and understand the dataset
data(diamonds)
data(package=.packages(all.available=TRUE))
library(ggplot2)
data(diamonds)
str(diamonds)
dim(diamonds)
?diamonds

# very basic scatter
plot(diamonds$carat,diamonds$price)



#Imporved Scatter
plot(diamonds$carat,diamonds$price,col=rgb(0,0,1,0.1),pch=16,
     main="Scatter Plot:Carat vs Price")
#Hexbin Using Baser
install.packages('hexbin')
library(hexbin)
hb<-hexbin(diamonds$carat,diamonds$price,xbins=40)
plot(hb,main="Hexbin Plot")

#Basic Hexbin
ggplot(diamonds,aes(x=carat,y=price))+
  geom_hex()

#Labeled Hexbin plot
ggplot(diamonds,aes(carat,price))+
  geom_hex()+
  labs(title="Hexagon Binning:Diamomd Structure",x="Carat",y="Price")+
  theme_minimal()
#Control Hexagon density
ggplot(diamonds,aes(carat,price))+geom_hex(bins=10)+theme_minimal()

#Add color palette(Gradient)
ggplot(diamonds,aes(carat,price))+
  geom_hex(bins=40)+
  scale_fill_gradient(low="lightgreen",high="red")+
  theme_minimal()
#Color meaning:
#Light->fewer Diamonds
#Dark->dense region
#PROGESSIONAL PALATTE(Viridis)
ggplot(diamonds,aes(carat,price))+geom_hex(bins=35)+scale_fill_viridis_c()+
  theme_minimal()
#ADD LEGEND TITLE
ggplot(diamonds,aes(carat,price))+geom_hex(bins=20)+
  scale_fill_viridis_c(name="Count")+
  labs(title="Density Structure of Diamonds",x="Carat",y="Price")+
  theme_minimal()
#FACETED HEXBIN
ggplot(diamonds,aes(carat,price))+geom_hex()+scale_fill_viridis_c()+
  facet_wrap(~cut)+
  theme_minimal()
#LOG SCALE HEXBIN
ggplot(diamonds,aes(carat,price))+geom_hex()+scale_fill_viridis_c()+
  scale_y_log10()+
  theme_minimal()



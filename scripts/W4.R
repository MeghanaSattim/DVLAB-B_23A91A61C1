data(iris)
#Understand the data
str(iris)
View(iris)
class(iris)
?iris

#Basic box plot
boxplot(iris$Sepal.Length)

#Add Title & Labels
boxplot(iris$Sepal.Length,
        main="Box plot for Sepal Length",
        ylab="Sepal_Length",
        col='skyblue')


#Grouped Box plot(by Species)
boxplot(Sepal.Length~Species,
        data=iris,
        main="Sepal Length by Species",
        xlab="Species",
        ylab="Sepal Length",
        col=c('pink','green','skyblue'))

#Multi-Variable Box Plot
boxplot(iris$Sepal.Length,
        main="Box plot for Sepal Length",
        ylab="Sepal_Length",
        col='skyblue')
        boxplot(iris[,1:4],
                main="Multi Variable Box Plot",
                col=c('pink','green','skyblue','orange'))
        
        
#Using customer color Palette
species_colors<-c("setosa"="red",
                  "Versicolor"="steelblue",
                  "virginca"="green")


boxplot(
  Sepal.Length~Species,
  data = iris,
  col = species_colors,
  main = "Sepal Length bt Species (Custom Color Palatte)"
)





library(ggplot2)
ggplot(iris,
       aes(x=Species,y=Sepal.Length))+
  geom_boxplot()+
  theme_minimal()

#Using Manual Color Palettes
ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_boxplot()+
  scale_fill_manual(
    values=c(
      "setosa" = "red",
      "versicolor" ="steelblue",
      "virginica" = "green"
    )
  )+theme_minimal()


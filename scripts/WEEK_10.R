#load and understand the dataset
data(iris)
str(iris)
#BASIC 3D SCATTER PLOT
#install necessary packages
install.packages("scatterplot3d")
library(scatterplot3d)
# basic 3d plot
scatterplot3d(
  iris$Sepal.Length,
  iris$Sepal.Width,
  iris$Petal.Length
)
#labelled 3d scatter plot
scatterplot3d(
  iris$Sepal.Length,
  iris$Sepal.Width,
  iris$Petal.Length,
  main="3D Scatter plot of iris data",
  xlab="Sepal length",
  ylab="Sepal width",
  zlab="Petal length",
  pch=16
)
#coloured 3d scatter plot by species
colors<-as.numeric(iris$Species)
scatterplot3d(
  iris$Sepal.Length,
  iris$Sepal.Width,
  iris$Petal.Length,
  color=colors,
  pch=16,
  main="3D scatter plot by species"
)

legend(
  "topright",
  legend=levels(iris$Species),
  col=1:3,
  pch=16
) # now species clusters become visible

#3D plot using 'cloud()' (Lattice)
#load lattice package
library(lattice)
#basic 3D cloud plot
cloud(
  Petal.Length~Sepal.Length*Sepal.Width,
  data=iris
)

#coloured cloud plot
cloud(
  Petal.Length~Sepal.Length*Sepal.Width,
  data=iris,
  groups=Species,
  auto.key=TRUE,
  main="3D Cloud plot of iris data"
)

#multi-panel scatter using 'xyplot()'
#although 'xyplot()' is not true 3D, it allows multi-dimensional comparision
xyplot(
  Petal.Length~Sepal.Length|Species,
  data=iris,
  pch=16,
  col="blue",
  layout=c(3,1)
)

#Intercative 3D plot using plotly (Advanced)
#Install plotly:
install.packages("plotly")
library(plotly)
#Interactive 3D scatter:
plot_ly(
  iris,
  x=~Sepal.Length,
  y=~Sepal.Width,
  z=~Petal.Length,
  color=~Species,
  type="scatter3d",
  mode="markers"
)

#3D plot with size encoding
plot_ly(
  iris,
  x=~Sepal.Length,
  y=~Sepal.Width,
  z=~Petal.Length,
  color=~Species,
  size=~Petal.Width,
  type="scatter3d",
  mode="markers"
)
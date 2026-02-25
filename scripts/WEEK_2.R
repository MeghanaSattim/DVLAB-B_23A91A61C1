#Load the data set
AirPassengers

#Verify the dataset
class(AirPassengers)
View(AirPassengers)
#Convert the dataset to dataFrame
ap_df<-data.frame(
  year=time(AirPassengers),
  passengers=as.numeric(AirPassengers)
)

ap_df_months<-data.frame(
  year=floor(time(AirPassengers)),
  month=cycle(time(AirPassengers)),
  passengers=as.numeric(AirPassengers)
)
ap_df_months
#Basic plot
plot(AirPassengers)

#Plot with title,Axis labels and colors
plot(AirPassengers,
     main="Air Passengers Trend Analysis",
     xlab="Months",
     ylab="No.of Passengers",
     col="red")


#Changing the line width
plot(AirPassengers,
     type='l',
     lwd=2,
     main="Air Passengers Trend Analysis",
     xlab="Months",
     ylab="No.of Passengers",
     col="red")

points(AirPassengers,
       type='o',
       pch=17,
       col="blue")
grid()

library(ggplot2)
ggplot(ap_df,
       aes(x=year,y=passengers))+
    geom_line(color='darkgreen',linewidth=0.5)+
    labs(
      title="Trend Analysis of Air Passengers",
      caption="Using Air Passengers Dataset",
      subtitle="From 1949-1960",
      x="Months",
      y="No.of Passengers"
    )+
    geom_point(color='red')+
    geom_smooth(se=FALSE,color='orange')
    theme_minimal()





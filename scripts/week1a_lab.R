#Accessing the information about the dataset
?VADeaths
#Checking the type of the Dataset
class(VADeaths)
#accessing the help for unknown functions or classes
help(class)
#converting given dataset into DataFrames
va_df<-as.data.frame(VADeaths)
#checking to confirm if the datasets is changed to DataFrames
class(va_df)
#creating a view of the Dataset in the form of a Table
View(va_df)
#creating a new column in the dataframe
va_df$AgeGroups<-row.names(VADeaths)
View(va_df)
va_df
#Changing the data into "long" Format using "reshape "function
va_long=reshape(va_df,
                direction="long",
                varying=colnames(VADeaths),
                v.names="DeathRate",
                timevar="Population",
                times=colnames(VADeaths)
                
)
View(va_long)
#Building a basic Histogram
png("Basic_hist.png")
hist(va_long$DeathRate)

#Change the Title and labels
hist(va_long$DeathRate,
     main="Basic Histogram of VA Deaths",
     xlab="VA DeathRates",
     ylab="Frequency",
     labels=TRUE)

#Change the color and border of his histogram
hist(va_long$DeathRate,
     main="Basic Histogram of VA Deathrates",
     xlab="VA Deathrates",
     ylab="Frequency",
     labels=TRUE,
     col="pink",
     border="blue")


#Changing the number of bins/bin-width

hist(va_long$DeathRate,
     breaks=5,
     main="Basic Histogram of VA Deathrates",
     xlab="VA Deathrates",
     ylab="Frequency",
     labels=TRUE,
     col="pink",
     border="blue"
)
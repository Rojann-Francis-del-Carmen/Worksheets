#Rojann Francis del Carmen
#Worksheet 5

#1)
#a.  Plot the data using a bar graph. Write the codes and copy the result.
year2019_2020 <- c(80, 75, 70, 60)
barplot(year2019_2020)

#b. Using the same table, label the barchart with
#Title = ” Enrollment of BS Computer Science
#horizontal axis = “Curriculum Year” and
#vertical axis = “number of students”

barplot(year2019_2020, 
        main = "Enrollment of BS Computer Science",
        xlab = "Curriculum Year",
        ylab = "number of students", names.arg= c("1st", "2nd", "3rd", "4th"))

#2)

#a. Create a table for the above scenario.
#Write the codes and its result.

MonthlyIncome_Dejesus <- data.frame(Food = ("60%"), Electricity = ("10%"),
                    Savings = ("5%"), Other_miscellaneous_expenses = ("25%"))
MonthlyIncome_Dejesus

#b.  Plot the data using a pie chart. Add labels, colors and legend.
#Write the codes and its result.

Monthlyincome_Dejesus <- c(60, 10, 5, 25)
Monthlyincome_Dejesus

pie(Monthlyincome_Dejesus,
    main = "cost",
    col = rainbow(length(Monthlyincome_Dejesus)),
    labels = c("Food", "Electricity", "Savings", "Other miscellaneous expenses"))
    legend("topright", c("Food", "Electricity", "Savings", "Other miscellaneous 
                         expenses"),
    cex = 0.5, fill= rainbow(length(Monthlyincome_Dejesus)))

#3)
data(mtcars)

#a.  Create a simple histogram specifically for mpg (miles per gallon) variable.
#Use $ to select the mpg only. Write the codes and its result.

simple_histog <- (mtcars$mpg)
hist(simple_histog,)

#b. Colored histogram with different number of bins.
#hist(mtcars$mpg, breaks=12, col="red")
#Note: breaks= controls the number of bins

hist(simple_histog, breaks=12, col="red")

#c. Add a Normal Curve
#x <- mtcars$mpg
#h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon",
#xfit<-seq(min(x),max(x),length=40)
#yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#yfit <- yfit*diff(h$mids[1:2])*length(x)
#lines(xfit, yfit, col="blue", lwd=2)

#Copy the result.

histo <-hist(simple_histog, breaks = 10, col = "red", xlab = "Miles Per Gallon",
        main = "Histogram with Normal Curve")
xfit <-seq(min(simple_histog),max(simple_histog),length = 40)
yfit <-dnorm(xfit,mean = mean(simple_histog),sd = sd(simple_histog))
yfit <- yfit*diff(histo$mids[1:2])*length(simple_histog)
lines(xfit, yfit, col = "blue", lwd = 2)

#4)
data(iris)
View(iris)

#a. Write the codes and its result

dset1<- subset(iris, Species == "setosa")
dset2<- subset(iris, Species == "versicolor")
dset3<- subset(iris, Species == "virginica")
dset1
dset2
dset3

#b. Get the mean for every characteristics of each species using colMeans().

setosa <- colMeans(dset1[sapply(dset1,is.numeric)])
versicolor <- colMeans(dset2[sapply(dset2,is.numeric)])
virginica <- colMeans(dset3[sapply(dset3,is.numeric)])
setosa
versicolor 
virginica

#c. Combine all species by using rbind()

species <- rbind(setosa, versicolor, virginica)
species

#d.  From the data in 4-c: Create the barplot().
#Write the codes and its result.

barplot(species, beside = TRUE, col= c("red", "green", "blue"),
        main = "Iris Data",
        xlab = "Characteristics",
        ylab = "Mean Scores", names.arg= c("Sepal.Length", "Sepal.Width",
                                           "Petal.Length", "Petal.Width "))




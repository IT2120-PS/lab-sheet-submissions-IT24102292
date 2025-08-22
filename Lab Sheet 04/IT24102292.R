getwd()
setwd("D:/IT24102292/Desktop/IT24102292")
branch_data <- read.table("D:/IT24102292/Desktop/Exercise.txt", header=TRUE, sep="\t")
head(branch_data)
branch_data <- read.table("Exercise.txt", header=TRUE, sep="\t")
head(branch_data)
str(branch_data)
boxplot(branch_data$sales, main="Boxplot of Sales", ylab="Sales")
fivenum(branch_data$advertising)
IQR(branch_data$advertising)
Q1 <- quantile(x,0.25)
find_outliers <- function(x) {
  Q1 <- quantile(x,0.25)
  Q3 <- quantile(x,0.75)
  IQR_value <- IQR(x)
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}
outliers_years <- find_outliers(branch_data$years)
outliers_years
getwd()
setwd("D:\IT24102292/Desktop\IT24102292")
##part 1
setwd("D:/IT24102292/Desktop/IT24102292")
data <- read.table("DATA 4.txt", header=TRUE, sep = " ")
fix(data)
attach(data)
##part 2
boxplot(x1,main="Box plot for Team Attendence", outline=TRUE,outpch=8,horizontal=TRUE)
data <- read.csv("data 3.csv",header=TRUE)
##part 2
##part a
boxplot(x1,main="Box plot for Team Attendence", outline=TRUE,outpch=8,horizontal=TRUE)
boxplot(x2,main="Box plot for Team Salary", outline=TRUE,outpch=8,horizontal=TRUE)
boxplot(x3,main="Box plot for Team Years", outline=TRUE,outpch=8,horizontal=TRUE)
hist(x1,ylab="Frequency",xlab="Team Attendence", main="Histogram for Team Attendence")
hist(x2,ylab="Frequency",xlab="Team Salary", main="Histogram for Team Salary")
hist(x3,ylab="Frequency",xlab="Team Years", main="Histogram for Team Years")
stem(x1)
stem(x2)
stem(x3)
##part b
mean(x1)
mean(x2)
mean(x3)
median(x1)
median(x2)
median(x3)
sd(x1)
sd(x2)
sd(x3)
##part 3
get.mode <- function(y) {
  counts <- table(x3)
  names(counts[counts == max(counts)])
}
get.mode(x3)
table(x3)
maxcounts
max(counts
    counts == max(counts)
    counts[counts == max(counts)]
    names(counts[counts == max(counts)])
    ##part 4
    get.outliers <-function(z) {
      q1 <- quantile(z)[2]
      q3 <- quantile(z)[4]
      iqr <- q3 - q1
      ub <- Q3 + 1.5 * iqr
      lb <- q1 - 1.5 * iqr
      print(paste("Upper Bound = ", ub))
      print(paste("Lower Bound = ", lb))
      print(paste("Outliers: ", paste(sort(z[z<lb | z>ub]), collapse = ",")))
    }
    get.outliers(x1)
    print(paste("Outliers:", paste(sort(z[z<lb | z>ub]), collapse=",")))
    
    
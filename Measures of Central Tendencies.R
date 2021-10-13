##########################################################
####### Lab 4 # Measures of Central Tendencies #######
####### Copyright - HURU School 2021 #######
##########################################################

# Import Libraries
install.packages("ggpubr")

install.packages('tidyverse') # We imported this so that we can get the glimpse function
library(tidyverse)
library(ggpubr) # ---> Publication ready plots

# Import Data for flight weight and toonage haul coorelation - 

flight_load <- read.csv(file ="flight_data.csv",stringsAsFactors = FALSE)

flight_load
class(flight_load$Weight)
glimpse(flight_load)

#################################################################
################# Checking normality visually ###################
#################################################################
library("ggpubr")

ggdensity(flight_load$Tonnage, 
          main = "Toonage after Treatment ",
          xlab = "Toonage")

hist(flight_load$Tonnage)

#################################################################
################# Visual inspection, described in the previous section, is usually unreliable. It’s possible to use a significance test comparing the sample distribution to a normal one in order to ascertain whether data show or not a serious deviation from normality.There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk’s test. ###################
#################################################################
###### Mean ####################################################################################################################

# Converts Tonnage NAs into zeros

flight_load$Tonnage[is.na(flight_load$Tonnage)]=0

flight_load

# Here we calculated the mean - caveat on interpreting the mean because this is a skewed distribution of data

# Mean

mean(flight_load$Tonnage) # Average Tonnage that the flights can haul

# median 

median(flight_load$Tonnage)# Median Tonnage that the flights can haul

# mode - using a different dataset - for the most frequent observation

# Create a function - remember you can create functions in R
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create the vector with numbers using the concatenate function
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)

# Calculate the mode using the user function.
result <- getmode(v)
print(result)

# Create the vector with characters.
charv <- c("o","it","the","it","it")

# Calculate the mode using the user function.
result <- getmode(charv)
print(result)

###############################################################################
### Salaries Sample data ######################################################
emp_salaries <- read.csv('Employee Salaries.csv')
glimpse(emp_salaries)

class(emp_salaries$Salary)

ggdensity(emp_salaries$Salary, 
          main = "Employee Salary US$ ",
          xlab = "US $")

hist(emp_salaries$Salary)

#This salary of this organization is skewed to the right and thus to know
#the salary of an average employee, the median would give the best answer

#Mean
mean(emp_salaries$Salary)

#Median

median(emp_salaries$Salary)

################################################################################
###Ordinal data (Job.Satisfaction column)#####
#Mode
print(getmode(emp_salaries$Job.Satisfaction))

#Median
#Convert the satisfaction level to numbers
satisfaction <- data.frame(
  Job.Satisfaction = c("Very Dissatisfied","Dissatisfied",
                       "Neither satisfied nor dissatisfied","Satisfied",
                       "Very Satisfied"),
  Satisfaction = 1:5)

View(satisfaction)


#Merge the two datasets
emp_salaries = merge(emp_salaries, satisfaction, on = "Job.Satisfaction")

glimpse(emp_salaries)

view(emp_salaries)
median(emp_salaries$Satisfaction)

sort(emp_salaries$Satisfaction)

################################################################################
################ Nominal data (Phone.Type column) ##############################
#Mode
print(getmode(emp_salaries$Phone.Type))
Mode(emp_salaries$Phone.Type)

mean(emp_salaries$Salary)
sd(emp_salaries$Salary)


#Range######################################################################

x = c(5,2,7,9,4)
range(x)

y = c(5,2,NA,9,4)
range(y,na.rm = FALSE)

range(y,na.rm = TRUE)

x = c("c", "r", "e", "a", "g", "e", "r")
range(x)

######There are several quartiles of an observation variable. 
#The first quartile, or lower quartile, is the value that cuts off the first 25% of the data when it is sorted in ascending order. 
#The second quartile, or median, is the value that cuts off the first 50%. 
#The third quartile, or upper quartile, is the value that cuts off the first 75%.


duration = flight_load$Weight
quantile(duration) 

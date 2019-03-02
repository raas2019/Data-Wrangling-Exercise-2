getwd()
setwd("C:/Users/rober/Documents")
getwd()


Titanic <- read.csv(file.choose())

Titanic
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)



# Goal 1. Port of embarkation
#Find the missing values and replace them with S

Titanic <- Titanic %>% 
  mutate(embarked = gsub("^$", "S", embarked))
Titanic

# Converting the original data set to dply tbl_df data frame

Titanic <- tbl_df(Titanic)
Titanic

# creating head. tail. structuere, glimpse and summary of the dataframe

head(Titanic)
tail(Titanic)
str(Titanic)
glimpse(Titanic)
summary(Titanic)

# another way of embarkation
Titanic$embarked
Titanic$embarked[is.na(Titanic$embarked)] <- 'S'
Titanic


# Goal 2.  Age
# a: Calculate the mean of the Age column and use that value to populate the missing values


Titanic$age[is.na(Titanic$age)] <- mean(Titanic$age, na.rm = TRUE)
Titanic$age
mean(Titanic$age)
dplyr::filter(Titanic, is.na(age))
Titanic

# b: I would choose Linear Regression method to find the missing values: To begin, several predictors of the variable with missing values are 
#     identified using a correlation matrix. The best predictors are selected and used as independent 
#     variables in a regression equation. The variable with missing data is used as the dependent variable.


# Goal 3.  Lifeboat
# there are a lot of missing values in the boat column. Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'

Titanic <- Titanic %>% 
  mutate(boat = gsub("^$", "NA", boat))
Titanic$boat

#  unique boat list
Titanic %>% select(boat) %>% unique()

# or other way

dplyr::filter(Titanic, is.na(boat))


# Goal 4. Cabin
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

Titanic <- Titanic %>% rowwise() %>% mutate(has_cabin_number = ifelse(is.na(cabin),0,1))
glimpse(Titanic)


#--------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)


Titanic <-data.frame(Titanic)
Titanic
Titanic <-select(Titanic,survived,pclass,sex,age)
Titanic
Titanic <-na.omit(Titanic)
Titanic
head(Titanic)
str(Titanic)


# 2 - Plot 1 . Use ggplot() for the first instruction

#--------with posiotion "dodge2"

ggplot(Titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge2")

#------- with position "staack"

ggplot(Titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "stack")



# 3 - Plot 2, add facet_grid() layer.

#---------------------Next to each other, horizontal

ggplot(Titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge2") +
  facet_grid (.~survived)

#---------------------Top of each other, vertical

ggplot(Titanic, aes(x = pclass, fill = sex)) +
  geom_bar(position = "dodge2") +
  facet_grid (survived~.)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)


# 5 - using ggplot2 creating boxplot

ggplot(Titanic, aes(x = pclass,y=age, fill = sex)) +
  geom_boxplot(outlier.size = 0.8)
  



# 5 - Plot 3, but use the position object from instruction 4

# vertical

ggplot(Titanic, aes(x = pclass,y= age, color = sex)) +
  geom_point(position = posn.jd, size =1.5, alpha=0.5) +
  facet_grid(.~survived)

# horizontal

ggplot(Titanic, aes(x = pclass,y= age, color = sex)) +
  geom_point(position = posn.jd, size =1.5, alpha=0.5) +
  facet_grid(survived~.)

#-------------------------------------------------------------------



plot(Titanic)



plot(Titanic$pclass, Titanic$age,
     pch=16, col= "blue")


plot(Titanic$sex, Titanic$age,
     pch=16, col= "blue")





     

















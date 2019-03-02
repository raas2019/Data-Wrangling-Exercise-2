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
library(titanic)
library(tidyr)
library(dplyr)

# titanic is avaliable in your workspace
# 1 - Check the structure of titanic
titanic <-data.frame(titanic_train)
titanic <-select(titanic,Survived,Pclass,Sex,Age)
titanic <-na.omit(titanic)
head(titanic)
str(titanic)


# 2 - Use ggplot() for the first instruction
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge") +
  facet_grid(.~Survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
ggplot(titanic, aes(x = Pclass,y= Age, color = Sex)) +
  geom_point(position = posn.jd, size =3, alpha=0.5) +
  facet_grid(.~Survived)

#-------------------------------------------------------------------

























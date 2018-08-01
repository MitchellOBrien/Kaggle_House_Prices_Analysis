# Kaggle_House_Prices_Analysis
My R markdown for the Housing Prices Kaggle competition.


---
title: "Housing_Competition"
author: "Mitchell O'Brien"
date: "July 27, 2018"
output: 
  html_document:
    keep_md: true
---

```{r setup, include=FALSE, cache=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```


#Introduction
Ask a home buyer to describe their dream house, and they probably won't begin with the height of the basement ceiling or the proximity to an east-west railroad. But this playground competition's dataset proves that much more influences price negotiations than the number of bedrooms or a white-picket fence.

With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this competition challenges you to predict the final price of each home.


#Foreword on my Analysis
This is my first Kaggle competition. A lot of decisions I made, and procedures I followed were based on the book Applied Predictive Modeling. As I progressed in my analysis, it became apparent that decisions I made in coding, were not efficient. For example, at the beginning, I kept the training set and test set completely separate. Other competitors made note of which were training and which were test before combining them into one data frame. There were other instances similar to this one, where my code was somewhat inefficient. However, it provided a learning experience for the next data set I analyze.

 
```{r, include=FALSE}
library(plyr)
library(dplyr)
library(ggplot2)
library(car)
library(caret)
library(e1071)
library(gridExtra)
library(readr)


setwd("C:/Users/Mitchell/Desktop/Summer Projects/Housing Project")
train<-read.csv("train.csv", header = TRUE, stringsAsFactors = F)
test<-read.csv("test.csv", header = TRUE, stringsAsFactors = F)
```


#Data Cleaning and Variable Coding
I started this competition by thoroughly reading the data description and becoming familiar with what each variable represented. The goal was to become acquainted with how the data was coded and any possible issues in my analysis.  

Next, I was curious about how many missing values or NA's I had in the data set. I found that there were a few variables with many missing values. To investigate, I checked the data data description for added information on the troubling variables. The problem was that the csv was written with "NA" being used as none. For example the variable PoolQC had 1453 missing values. Reason being, that a lot of the homes didn't have a pool, so they were entered as NA. I had to change each similar variable to something other than NA.
```{r}
sort(apply(train, 2, function(x){sum(is.na(x))}), decreasing = TRUE)
sort(apply(test, 2, function(x){sum(is.na(x))}), decreasing = TRUE)
```



##MSSubClass
MSSubClass is a factor variable. However, the way it was entered into the csv is problematic. A number is used to represent a category. I changed this to match the data description.
```{r}
train$MSSubClass<-as.factor(train$MSSubClass)
train$MSSubClass<-revalue(train$MSSubClass, c("20"="1-STORY 1946 & NEWER ALL STYLES",
                                              "30"="1-STORY 1945 & OLDER",
                                              "40"="1-STORY W/FINISHED ATTIC ALL AGES",
                                              "45"="1-1/2 STORY - UNFINISHED ALL AGES",
                                              "50"="1-1/2 STORY FINISHED ALL AGES",
                                              "60"="2-STORY 1946 & NEWER",
                                              "70"="2-STORY 1945 & OLDER",
                                              "75"="2-1/2 STORY ALL AGES",
                                              "80"="SPLIT OR MULTI-LEVEL",
                                              "85"="SPLIT FOYER",
                                              "90"="DUPLEX - ALL STYLES AND AGES",
                                              "120"="1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
                                              "150"="1-1/2 STORY PUD - ALL AGES",
                                              "160"="2-STORY PUD - 1946 & NEWER",
                                              "180"="PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
                                              "190"="2 FAMILY CONVERSION - ALL STYLES AND AGES"))


test$MSSubClass<-as.factor(test$MSSubClass)
test$MSSubClass<-revalue(test$MSSubClass, c("20"="1-STORY 1946 & NEWER ALL STYLES",
                                            "30"="1-STORY 1945 & OLDER",
                                            "40"="1-STORY W/FINISHED ATTIC ALL AGES",
                                            "45"="1-1/2 STORY - UNFINISHED ALL AGES",
                                            "50"="1-1/2 STORY FINISHED ALL AGES",
                                            "60"="2-STORY 1946 & NEWER",
                                            "70"="2-STORY 1945 & OLDER",
                                            "75"="2-1/2 STORY ALL AGES",
                                            "80"="SPLIT OR MULTI-LEVEL",
                                            "85"="SPLIT FOYER",
                                            "90"="DUPLEX - ALL STYLES AND AGES",
                                            "120"="1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
                                            "150"="1-1/2 STORY PUD - ALL AGES",
                                            "160"="2-STORY PUD - 1946 & NEWER",
                                            "180"="PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
                                            "190"="2 FAMILY CONVERSION - ALL STYLES AND AGES"))

```

##MSZoning
I made MSZoning a factor variable and dealt with the missing value.
```{r}
train$MSZoning[is.na(train$MSZoning)]<-"Undefined"
test$MSZoning[is.na(test$MSZoning)]<-"Undefined"

train$MSZoning<-as.factor(train$MSZoning)
test$MSZoning<-as.factor(test$MSZoning)
```

##LotFrontage
LotFrontage has 259 missing values. One way to handle this would be to omit each observation with a missing value for LotFrontage. However, a lot of information would be lost if I chose this path. Instead, I imputed the mean LotFrontage for each missing value.
```{r}
train$LotFrontage<-ifelse(is.na(train$LotFrontage), mean(train$LotFrontage, na.rm=TRUE), train$LotFrontage)
test$LotFrontage<-ifelse(is.na(test$LotFrontage), mean(test$LotFrontage, na.rm=TRUE), test$LotFrontage)


```

##Street
I made Street a factor variable. Due to the difference in the count for Grvl and Pave, I figured Street would get thrown out later during modeling. But, I thought best to keep it for now.
```{r}
train%>%
  group_by(Street)%>%
  summarise(n = n())

train$Street<-as.factor(train$Street)
test$Street<-as.factor(test$Street)
```

##Alley
Alley was one of the variables that had NA entered when the intention was no alley. There were1 369 missing values. I recoded, and made alley a factor variable.  

I was curious if different levels of Alley yielded different SalePrices. It appears that Paved and no Alley had higher average SalePrices than Grvl. I found it interesting to look into, but likely not a strong influence on SalePrice.
```{r}
train$Alley[is.na(train$Alley)]<-"no"
train$Alley<-as.factor(train$Alley)

test$Alley[is.na(test$Alley)]<-"no"
test$Alley<-as.factor(test$Alley)

ggplot(train, aes(Alley, SalePrice))+
  geom_point()+
  geom_point(stat = "summary", fun.y = "mean", size = 6, col = "red")+
  ggtitle("SalePrice by Alley")+
  theme(plot.title = element_text(hjust = .5))

```

##LotShape
I made LotShape a factor variable. 
```{r}
train$LotShape<-as.factor(train$LotShape)
test$LotShape<-as.factor(test$LotShape)
```

##LandContour
I made LandContour a factor variable.
```{r}
train$LandContour<-as.factor(train$LandContour)
test$LandContour<-as.factor(test$LandContour)
```

##Utilities
Since there was near zero variance in Utilities, no information is provided by this variable. I decided to drop Utilities from my analysis.
```{r}
table(train$Utilities)
table(test$Utilities)

train$Utilities<-NULL
test$Utilities<-NULL

```

##LotConfiguration
I made LotConfiguration a factor variable.
```{r}
train$LotConfig<-as.factor(train$LotConfig)
test$LotConfig<-as.factor(test$LotConfig)
```


##LandSlope
To me, LandSlope has a ranking of it's categories. Thus, I made LandSlope as an ordinal variable by coding it as an integer.
```{r}
train$LandSlope<-as.integer(revalue(train$LandSlope, c(Gtl=0, Mod=1, Sev=2)))
test$LandSlope<-as.integer(revalue(test$LandSlope, c(Gtl=0, Mod=1, Sev=2)))
```

##Neighborhood
I decided that Neighborhood is a factor variable. I guessed that Neighborhood has a great influence on SalePrice. I held off on investigating until the exploratory analysis part. 
```{r}
train$Neighborhood<-as.factor(train$Neighborhood)
test$Neighborhood<-as.factor(test$Neighborhood)
```

##Condition1 and Condition2
I decided that both Condition1 and Condition2 are factor variables.
```{r}
train$Condition1<-as.factor(train$Condition1)
test$Condition1<-as.factor(test$Condition1)

train$Condition2<-as.factor(train$Condition2)
test$Condition2<-as.factor(test$Condition2)
```

##BldgType
I went back and forth on making this variable a factor or ordinal. There is a degree of ranking among the categories. But, in the end, I decided to make it a factor. Perhaps I could go back, make it ordinal, and check if my model performs better in a later analysis. 
```{r}
train$BldgType<-as.factor(train$BldgType)
test$BldgType<-as.factor(test$BldgType)
```

##HouseStyle
Similar to BldgType, there is a degree of ranking among the categories. However the rankings were necessarily clear, so I decided to make HouseStyle a factor variable.
```{r}
train$HouseStyle<-as.factor(train$HouseStyle)
test$HouseStyle<-as.factor(test$HouseStyle)
```

##OverallQual and OverallCond
OverallQual and OverallCond are clearly a ordinal variables. They are already coded as integers, so I did not need to change anything about them.  


##YearBuilt and YearRemodAdd
YearBuilt  and YearRemodAdd were already coded as integers, so there was nothing for me to change.  

##RoofStyle
I decided that RoofStyle would be a factor variable.
```{r}
train$RoofStyle<-as.factor(train$RoofStyle)
test$RoofStyle<-as.factor(test$RoofStyle)

```

##RoofMat1
I decided that RoofStyle would be a factor variable.
```{r}
train$RoofMatl<-as.factor(train$RoofMatl)
test$RoofMatl<-as.factor(test$RoofMatl)
```

##Exterior1st and Exterior2nd
I decided to make Exterior1st and Exterior2nd factor variables. There were missing values in the test set though that needed to be addressed first. 
```{r}
#Exterior1st is factor
test$Exterior1st[is.na(test$Exterior1st)]<-"Undefined"


train$Exterior1st<-as.factor(train$Exterior1st)
test$Exterior1st<-as.factor(test$Exterior1st)

#Exterior2nd is factor
test$Exterior2nd[is.na(test$Exterior2nd)]<-"Undefined"


train$Exterior2nd<-as.factor(train$Exterior1st)
test$Exterior2nd<-as.factor(test$Exterior2nd)
```

##MasVnrType
As with other variables in this dataset, the lack of MasVnrType was coded as NA. I changed this to "no" so no problems arise. After the change, I made MasVnrType a factor variable.
```{r}
train$MasVnrType[is.na(train$MasVnrType)]<-"no"
train$MasVnrType<-as.factor(train$MasVnrType)

test$MasVnrType[is.na(test$MasVnrType)]<-"no"
test$MasVnrType<-as.factor(test$MasVnrType)
```


##MasVnrArea
There were a few missing values in MasVnrArea. I decided to impute the mean for each of these. A more careful approach would be to check what the value of MasVnrType was and enter 0 for Area when Type is "no".
```{r}
train$MasVnrArea<-ifelse(is.na(train$MasVnrArea), mean(train$MasVnrArea, na.rm = TRUE), train$MasVnrArea)

test$MasVnrArea<-ifelse(is.na(test$MasVnrArea), mean(test$MasVnrArea, na.rm = TRUE), test$MasVnrArea)
```

##Exterior Quality and Condition
Both are clearly ordinal.
```{r}

train$ExterQual<-as.integer(revalue(train$ExterQual, c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$ExterQual<-as.integer(revalue(test$ExterQual, c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


train$ExterCond<-as.integer(revalue(train$ExterCond, c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$ExterCond<-as.integer(revalue(test$ExterCond, c("Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))

```

##Foundation
I made foundation a factor variable.
```{r}
train$Foundation<-as.factor(train$Foundation)
test$Foundation<-as.factor(test$Foundation)
```

##BsmtQual
Similar to other Basement variables, I had to change the way no basement was coded. I made BsmtQual ordinal since the was a ranking to the categories. Intuitively, I thought that having a nice basement would have an impact on SalePrice. To test my intuition, I created a plot with the mean filled in red. There appeared not to be a huge difference in SalePrice of homes with no basement, a poor basement, or a typical basement. However, there is a jump from typical to good, and an even bigger jump from good to excellent. I included counts for added information. 
```{r}
train$BsmtQual[is.na(train$BsmtQual)]<-"no"

test$BsmtQual[is.na(test$BsmtQual)]<-"no"


train$BsmtQual<-as.integer(revalue(train$BsmtQual,c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$BsmtQual<-as.integer(revalue(test$BsmtQual, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


  
ggplot(train, aes(BsmtQual, SalePrice))+
  geom_point()+
  geom_point(stat = "summary", fun.y = "mean", size = 6, col = "red")+
  geom_text(stat = "count", aes(label = ..count..), y = 700000, col = "blue" )+
  ggtitle("SalePrice by BsmtQual")+
  theme(plot.title = element_text(hjust = .5))
```

##BsmtCond
I had to change the coding of no basement. After that, I made BsmtCond an ordinal variable. As with BsmtQual, I wanted to further investigate. It appeared the overwhelming number of basements have a condition score 3 with a count of 1311. I noticed a dip in price from no basement to poor basement, but, that is because of the small number of observations for poor basements. Other than that, higher condition basements receive a higher average SalePrice. Exactly what one would expect. 

```{r}
train$BsmtCond[is.na(train$BsmtCond)]<-"no"
#train$BsmtCond<-as.factor(train$BsmtCond)

test$BsmtCond[is.na(test$BsmtCond)]<-"no"
#test$BsmtCond<-as.factor(test$BsmtCond)

train$BsmtCond<-as.integer(revalue(train$BsmtCond,c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$BsmtCond<-as.integer(revalue(test$BsmtCond, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


ggplot(train, aes(BsmtCond, SalePrice))+
  geom_point()+
  geom_point(stat = "summary", fun.y = "mean", size = 6, col = "red")+
  geom_text(stat = "count", aes(label = ..count..), y = 700000, col = "blue" )+
  ggtitle("SalePrice by BsmtCond")+
  theme(plot.title = element_text(hjust = .5))

```


##BsmtExposure
BsmtExposure could have been an ordinal variable. But, for simplicity, I made it a factor variable. I was curious about how Exposure might impact SalePrice. I found that SalePrice increases with the degree of BsmtExposure. 
```{r}
train$BsmtExposure[is.na(train$BsmtExposure)]<-"nope"
train$BsmtExposure<-as.factor(train$BsmtExposure)

test$BsmtExposure[is.na(test$BsmtExposure)]<-"nope"
test$BsmtExposure<-is.factor(test$BsmtExposure)

ggplot(train, aes(reorder(BsmtExposure, SalePrice), SalePrice))+
  geom_boxplot()+
  geom_point(stat = "summary", fun.y = "mean", size = 4, col = "red")+
  ggtitle("SalePrice by BsmtExposure")+
  xlab("BsmtExposure")+
  theme(plot.title = element_text(hjust = .5))
```

##BsmtFinType1 and BsmtFinType2
I fixed the coding issues, and then made both variables ordinal.
```{r}
finish<-c("no"=0, "Unf"=1, "LwQ"=2, "Rec"=3, "BLQ"=4, "ALQ"=5, "GLQ"=6)

train$BsmtFinType1[is.na(train$BsmtFinType1)]<-"no"
#train$BsmtFinType1<-as.factor(train$BsmtFinType1)

test$BsmtFinType1[is.na(test$BsmtFinType1)]<-"no"
#test$BsmtFinType1<-as.factor(test$BsmtFinType1)


train$BsmtFinType1<-as.integer(revalue(train$BsmtFinType1, finish))
test$BsmtFinType1<-as.integer(revalue(test$BsmtFinType1, finish))

```

##BsmtSF
There were a few missing values for square feet in the test set. I made sure that when a home did not have a basement, the square feet was entered as 0.
```{r}
#BsmtFinSF1
test$BsmtFinSF1[is.na(test$BsmtFinSF1)]<-0


#BsmtFinSF2
test$BsmtFinSF2[is.na(test$BsmtFinSF2)]<-0

#BsmtUnfSF
test$BsmtUnfSF[is.na(test$BsmtUnfSF)]<-0

#TotalBsmtSF
test$TotalBsmtSF[is.na(test$TotalBsmtSF)]<-0
```

##Heating
Heating is a factor variable
```{r}
train$Heating<-as.factor(train$Heating)
test$Heating<-as.factor(test$Heating)
```

##Heating QC
Heating QC is an ordinal variable.
```{r}
train$HeatingQC<-as.integer(revalue(train$HeatingQC,c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$HeatingQC<-as.integer(revalue(test$HeatingQC,c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


```

##Central Air
Central Air is a factor
```{r}
train$CentralAir<-as.factor(train$CentralAir)
test$CentralAir<-as.factor(test$CentralAir)
```

##Electrical
Electrical is a factor. There was a missing value in the training set.
```{r}
#Electrical is factor
train$Electrical[is.na(train$Electrical)]<-"Undefined"

train$Electrical<-as.factor(train$Electrical)
test$Electrical<-as.factor(test$Electrical)

```

##1st and 2nd Floor SF
Neither variable had any missing values and both were already coded as integers so I did not have to do anything here.  

##LowQualFinSF
No missing values. Already an integer.  

##GrLivArea
No missing values. Already an integer.  

##Bsmt Bathrooms
There were some missing values for these two variables. The missing observations seemed to correspond with homes with no basement. Thus, I replaced NA with 0.
```{r}
#BsmtFullBath
train$BsmtFullBath[is.na(train$BsmtFullBath)]<-0
test$BsmtFullBath[is.na(test$BsmtFullBath)]<-0

#BsmtHalfBath
train$BsmtHalfBath[is.na(train$BsmtHalfBath)]<-0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)]<-0
```

##BedroomAbvGr
I left this variable as an integer. I wanted to investigate the impact on SalePrice. Due to unbalanced number of observations, it was difficult to establish a clear pattern. There does seem to be a positive relationship between BedroomAbvGr and SalePrice for Bedrooms 2,3, and 4. In a later version, I will bin the bedrooms with a small number of observations. It seemed to me like this variable would be highly correlated with other predictors such as total rooms above ground. I will deal with multicollinearity later in the analysis. 
```{r}
ggplot(train, aes(as.factor(BedroomAbvGr), SalePrice))+
  geom_boxplot(col = "blue")+
  geom_text(stat = "count", aes(label = ..count..),y = 700000)+
  ggtitle("SalePrice by BedroomAbvGr")+
  xlab("BedroomAbvGr")+
  theme(plot.title = element_text(hjust = .5))


cor(train$BedroomAbvGr, train$TotRmsAbvGrd)
```

##Kitchen
Kitchen was already coded as an integer.

##KitchenQual
KitchenQual is an ordinal variable. There were missing values in the test set that I had to fix. I have always heard how important the kitchen is in selling a home. I wanted to test this by looking at the distribution of SalePrice across KitchenQual. As suspected, SalePrice goes up with KitchenQual.
```{r}
test$KitchenQual[is.na(test$KitchenQual)]<-"no"

train$KitchenQual<-as.integer(revalue(train$KitchenQual,c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$KitchenQual<-as.integer(revalue(test$KitchenQual, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


ggplot(train, aes(as.factor(KitchenQual), SalePrice))+
  geom_boxplot(col = "blue")+
  ggtitle("SalePrice by KitchenQual")+
  xlab("KitchenQual")+
  theme(plot.title = element_text(hjust = .5))

ggplot(train, aes(SalePrice, fill = as.factor(KitchenQual)))+
  geom_histogram()+
  ggtitle("SalePrice by KitchenQual")+
  theme(plot.title = element_text(hjust = .5))+
  guides(fill = guide_legend(title = "KitchenQual"))
```

##TotRmsAbvGrd
TotRmsAbvGrd was already an integer.

##Functional
Functional had missing values in the test set that I had to deal with. I made Functional a factor variable.
```{r}
test$Functional[is.na(test$Functional)]<-"Undefined"
train$Functional<-as.factor(train$Functional)
test$Functional<-as.factor(test$Functional)
```

##Fireplaces
An integer variable.

##FireplaceQu
I fixed the missing value issue then made FireplaceQu an ordinal variable.
```{r}
train$FireplaceQu[is.na(train$FireplaceQu)]<-"no"
#train$FireplaceQu<-as.factor(train$FireplaceQu)

test$FireplaceQu[is.na(test$FireplaceQu)]<-"no"
#test$FireplaceQu<-as.factor(test$FireplaceQu)

train$FireplaceQu<-as.integer(revalue(train$FireplaceQu, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$FireplaceQu<-as.integer(revalue(test$FireplaceQu, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


```

##GarageType
I fixed the missing values then made GarageType a factor variable. I further looked into Garage Type and found that the average SalePrice was most for Attached and Built in garages. This makes sense, as one would see these in nicer homes. Detached would yield less money. A possible explanation is people would not want to walk to their garage in the cold during the winter months. Thus, Attached and Built in are more expensive more money. 
```{r}
train$GarageType[is.na(train$GarageType)]<-"no"
train$GarageType<-as.factor(train$GarageType)

test$GarageType[is.na(test$GarageType)]<-"no"
test$GarageType<-as.factor(test$GarageType)

ggplot(train, aes(reorder(GarageType,SalePrice), SalePrice))+
  geom_point()+
  geom_point(stat = "summary", fun.y = "mean", size = 6, col = "red")+
  geom_text(stat = "count", aes(label = ..count..), y = 700000, col = "blue")+
  ggtitle("SalePrice by GarageType")+
  xlab("GarageType")+
  theme(plot.title = element_text(hjust = .5))

```

##GarageYrBlt
To fix the missing values for GarageYrBlt, I observed the relationship between GarageYrBlt and YearBlt. I figured that in general, people had their garage built at the same time as their home. Therefore, I thought analyzing YearBlt was a reasonable thing to do. With a $r=.85$, I felt that imputing YearBlt for GarageYrBlt was the best avenue to take in correcting missing values.  
I checked other variables for a stronger correlation, but none were as strong as YearBlt.

```{r}
ggplot(train, aes(GarageYrBlt, YearBuilt))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


ggplot(train, aes(GarageYrBlt, YearRemodAdd))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(train, aes(GarageYrBlt, YrSold))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


train$GarageYrBlt[is.na(train$GarageYrBlt)]<-train$YearBuilt[is.na(train$GarageYrBlt)]
test$GarageYrBlt[is.na(test$GarageYrBlt)]<-test$YearBuilt[is.na(test$GarageYrBlt)]
```

##GarageFinish
I made GarageFinish ordinal.
```{r}
train$GarageFinish[is.na(train$GarageFinish)]<-"no"
#train$GarageFinish<-as.factor(train$GarageFinish)

test$GarageFinish[is.na(test$GarageFinish)]<-"no"
#test$GarageFinish<-as.factor(test$GarageFinish)


train$GarageFinish<-as.integer(revalue(train$GarageFinish, c("no"=0, "Unf"=1, "RFn"=2, "Fin"=3)))
test$GarageFinish<-as.integer(revalue(test$GarageFinish, c("no"=0, "Unf"=1, "RFn"=2, "Fin"=3)))

```

##GarageCars
GarageCars is an integer variable.
```{r}
#Garage Cars
test$GarageCars[is.na(test$GarageCars)]<-0

```


##GarageArea
GarageArea is an integer variable.
```{r}
#Garage Area
test$GarageArea[is.na(test$GarageArea)]<-0
```

##GarageQual
An ordinal variable.
```{r}
train$GarageQual[is.na(train$GarageQual)]<-"no"
#train$GarageQual<-as.factor(train$GarageQual)

test$GarageQual[is.na(test$GarageQual)]<-"no"
#test$GarageQual<-as.factor(test$GarageQual)

train$GarageQual<-as.integer(revalue(train$GarageQual, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$GarageQual<-as.integer(revalue(test$GarageQual, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))

```

##GarageCond
An ordinal variable.
```{r}
train$GarageCond[is.na(train$GarageCond)]<-"no"
#train$GarageCond<-as.factor(train$GarageCond)

test$GarageCond[is.na(test$GarageCond)]<-"no"
#test$GarageCond<-as.factor(test$GarageCond)


train$GarageCond<-as.integer(revalue(train$GarageCond, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$GarageCond<-as.integer(revalue(test$GarageCond, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))


```

##Paved Driveway
A factor variable.
```{r}
train$PavedDrive<-as.factor(train$PavedDrive)
test$PavedDrive<-as.factor(test$PavedDrive)
```

##WoodDeckSF
An integer variable.

##Porch Variables
OpenPorchSF, EnclosedPorch, ScreenPorch, and 3SsnPorch are integer variables.

##PoolArea
An integer variable.

##PoolQC
After dealing with missing values, converted to an ordinal variable.
```{r}
train$PoolQC[is.na(train$PoolQC)]<-"no"
#train$PoolQC<-as.factor(train$PoolQC)

test$PoolQC[is.na(test$PoolQC)]<-"no"
#test$PoolQC<-as.factor(test$PoolQC)


train$PoolQC<-as.integer(revalue(train$PoolQC, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))
test$PoolQC<-as.integer(revalue(test$PoolQC, c("no"=0,"Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)))

```


##Fence
An factor variable.
```{r}
train$Fence[is.na(train$Fence)]<-"no"
train$Fence<-as.factor(train$Fence)

test$Fence[is.na(test$Fence)]<-"no"
test$Fence<-as.factor(test$Fence)

```

##MiscFeature
A factor variable.
```{r}
train$MiscFeature[is.na(train$MiscFeature)]<-"no"
train$MiscFeature<-as.factor(train$MiscFeature)

test$MiscFeature[is.na(test$MiscFeature)]<-"no"
test$MiscFeature<-as.factor(test$MiscFeature)
```

##MiscVal
An integer variable. Already coded as integer so there was nothing for me to do here.

##MoSold
A factor variable.
```{r}
train$MoSold<-as.factor(train$MoSold)
test$MoSold<-as.factor(test$MoSold)
```

##YrSold
An integer variable.

##SaleType
The test set had a missing value. After taking care of it, I converted to factor a  variable.
```{r}
test$SaleType[is.na(test$SaleType)]<-"Undefined"
train$SaleType<-as.factor(train$SaleType)
test$SaleType<-as.factor(test$SaleType)
```

##SaleCondition
A factor variable.
```{r}
train$SaleCondition<-as.factor(train$SaleCondition)
test$SaleCondition<-as.factor(test$SaleCondition)
```

#Exploratory analysis
To some degree, I have already done some exploratory analysis. In this section, I wanted to dig a bit further. While there are near endless theories to test, I only checked into the most interesting ones to me. These were the variables that I wanted to investigate and see if they told any more of the story. 

##Exploration of MSZoning
I thought a good place to start was checking which zones have the highest SalePrice. I found that Residential low density and Floating Villages generally sold for a higher price. It makes sense for Residential low density to be more expensive, but, why was Floating Village? I decided to keep investigating what made Floating villages so expensive.
```{r}
ggplot(train, aes(reorder(as.factor(MSZoning), SalePrice),SalePrice))+
  geom_boxplot()+
  geom_text(stat = "count", aes(label = ..count..), y = 700000, col = "red")+
  ggtitle("SalePrice by MSZoning")+
  xlab("MSZoning")+
  theme(plot.title = element_text(hjust = .5))
```

In trying to figure out why Floating Villages were getting more money, I thought that perhaps Floating Villages had a larger lot size. According to the table below, Floating Zones have the second smallest lot size. Thus, lot size does not explain why Floating Zones were most expensive.

```{r}

train%>%
  group_by(as.factor(MSZoning))%>%
  summarise(Avg_LotArea=mean(LotArea), Avg_SalePrice=mean(SalePrice))%>%
  arrange(desc(Avg_LotArea))

```

My second thought on why Floating Zones were so expensive, had to do with Neighborhoods. Maybe all of the Floating Zone homes were located in an expensive neighborhood. I started by checking which neighborhoods were most expensive. Based on the figure below, clearly a top three exists. The table below shows that all Floating Zone homes are in Somerst. Somerst is the sixth most expensive neighborhood. This partially explains why Floating Zones are so expensive. Looking at residential low density and the neighborhoods they correspond to, you see that there are a lot of low density homes in cheaper neighborhoods which could bring down the average price. 

```{r}
ggplot(train, aes(reorder(as.factor(Neighborhood),SalePrice), SalePrice))+
  geom_point(stat = "summary", fun.y = "mean", col = "black", size=5)+
  geom_point(stat = "summary", fun.y = "mean", col = "red", size = 3)+
  xlab("Neighborhood")+
  ggtitle("Mean SalePrice by Neighborhood")+
  geom_text(stat = "count", aes(label = ..count..), y = 92500, col = "blue")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, vjust = .5,hjust = .5))

train%>%
  group_by(as.factor(MSZoning), as.factor(Neighborhood))%>%
  summarise(n=n(), Avg_SalePrice = mean(SalePrice), Avg_LotArea = mean(LotArea))%>%
  arrange(desc(Avg_SalePrice))%>%
  print(n = 42)
```

Another possible explanation of why Floating Zones are more money, is that more expensive types of homes are built there. I started by looking at which types of homes are most expensive. According to the figure below, 1Story, 2 story, and 2.5Fin are the three most expensive types of homes. The figure below offers a different look at the distributions of each type of home. Having established which home types are expensive, I wanted to check what the prominent type of home built in floating zones was. The table below shows that floating zones are mostly 2story homes, with the rest being 1 story. Again, I think this partially explains why floating zones are expensive. 

```{r}
ggplot(train, aes(reorder(as.factor(HouseStyle), SalePrice), SalePrice))+
  geom_boxplot(col = "blue")+
  ggtitle("Boxplots of SalePrice by HouseStyle")+
  xlab("HouseStyle")+
  theme(plot.title = element_text(hjust = .5))

ggplot(train, aes(SalePrice, fill = HouseStyle))+
  geom_histogram()+
  ggtitle("Histograms of SalePrice by HouseStyle")+
  theme(plot.title = element_text(hjust = .5))

train%>%
  filter(as.factor(MSZoning)=="FV")%>%
  group_by(as.factor(MSZoning), as.factor(HouseStyle))%>%
  summarise(n = n())%>%
  arrange(desc(n))

```

My last hypothesis into explaining Floating Zones was about KitchenQuality. From what I know about selling homes, Kitchen is always an important selling point. Therefore, I thought it necessary to look into.  

As one would imagine, the figures below showed that the higher kitchen quality, the higher the selling price. Tha table below, confirms my suspicion that floating zones have higher quality of kitchen. For floating zones, 94%  of kitchens are either good or excellent. While for low density residential, that number is only 50%. While this does not explain the whole phenomena, it does start to add detail to the picture.

```{r}
ggplot(train, aes(reorder(KitchenQual, SalePrice), SalePrice))+
  geom_boxplot(col = "blue")+
  ggtitle("Boxplots of SalePrice by KitchenQual")+
  xlab("KitchenQualR")+
  theme(plot.title = element_text(hjust = .5))

ggplot(train, aes(SalePrice, color = as.factor(KitchenQual)))+
  geom_density()+
  facet_wrap(~KitchenQual, ncol = 1)+
  ggtitle("Densities of SalePrice by KitchenQual")+
  theme(plot.title = element_text(hjust = .5))



#FV is almost all good kitchens
train%>%
  group_by(as.factor(MSZoning), as.factor(KitchenQual))%>%
  summarise(n = n())
```




Not totally happy with my answers to the floating zone price question, I decided to read up on Floating Zones in Ames, Iowa. My understanding is that places labeled a Floating Village receive more flexibility in terms of what the building can be used for. If this is the case, it makes sense for people to pay more money for added flexibility. Adding this definition with everything else I found out about floating zone homes, I am satisfied with why floating zones are the most expensive. 

#Modeling
##Feature Engineering
This feature engineering section is not very long or complicated. There were a couple variables that seemed to be overkill such as the porch variables which I consolidated into one. Additionally, I thought a total square foot variable would be important to include.

```{r}
TotalSF_train<-train$GrLivArea+train$TotalBsmtSF
PorchSF_train<-train$OpenPorchSF+train$EnclosedPorch+train$X3SsnPorch+train$ScreenPorch

TotalSF_test<-test$GrLivArea+test$TotalBsmtSF
PorchSF_test<-test$OpenPorchSF+test$EnclosedPorch+test$X3SsnPorch+test$ScreenPorch

ggplot(train, aes(TotalSF_train, SalePrice))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


ggplot(train, aes(PorchSF_train, SalePrice))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

```

##Numeric Variables

###Multicolinearity
Multidisciplinary is when there are strong correlations between numeric predictors. This is problematic because it makes model coefficients unstable. Interpretability and model performance can be negatively impacted by multicollinearity. To deal with this, I used caret's findCorrelation function. The function returns an index of columns that it recommends you to get rid of. Before use, it is necessary to separate numeric and factor variables. 

```{r}
#find the numeric predictors minus ID, SalePrice, TotalSF, and PorchSF
#separate numeric and factors

numeric_predictors_train<-train[,sapply(train, is.numeric)]
numeric_predictors_test<-test[,sapply(test, is.numeric)]

factor_predictors_train<-train[,sapply(train, is.factor)]
factor_predictors_test<-test[,sapply(test, is.factor)]

#drop Id and SalePrice from the numeric predictors
#drop the porch variables since I created a new one which adds them all
numeric_predictors_train<-numeric_predictors_train[,-c(1, 50)]
numeric_predictors_train<-numeric_predictors_train[,!(names(numeric_predictors_train) %in% c("OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch"))]

numeric_predictors_test<-numeric_predictors_test[,-c(1,50)]
numeric_predictors_test<-numeric_predictors_test[,!(names(numeric_predictors_test) %in% c("OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch"))]

#find correlations between predictors
correlations<-cor(numeric_predictors_train)
highCorr<-findCorrelation(correlations, cutoff = .75)
names(numeric_predictors_train[,highCorr])
filtered_numeric_predictors_train<-numeric_predictors_train[,-highCorr]
filtered_numeric_predictors_test<-numeric_predictors_test[,-highCorr]

filtered_numeric_predictors_train<-cbind(filtered_numeric_predictors_train,TotalSF_train)
filtered_numeric_predictors_train<-cbind(filtered_numeric_predictors_train,PorchSF_train)
filtered_numeric_predictors_test<-cbind(filtered_numeric_predictors_test,TotalSF_test)
filtered_numeric_predictors_test<-cbind(filtered_numeric_predictors_test,PorchSF_test)
```
###Skewness
Skewness is a statistic which measures the skew of a numeric variable's distribution. The function I used is called skewness. Skewness comes from the e1071 package. Before I used it, I separated true numeric variables. After identifying the degree of skew for each variable, I applied $log (x+1)$ transformation. The plus one is to deal with values of 0, since you cannot take the $log$ of 0. The histograms for the top four skewed variables are shown below to visualize what skew looks like.
```{r}
true_numeric_predictors_train<-filtered_numeric_predictors_train[,names(filtered_numeric_predictors_train) %in%
                                                       c("LotFrontage", "LotArea", "MasVnrArea", "BsmtUnfSF",
                                                         "X1stFlrSf", "X2ndFlrSF", "LowQualFinSF", "BsmtFullBath",
                                                         "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr",
                                                         "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageArea",
                                                         "WoodDeckSF", "TotalSF_train", "PorchSF_train")]

true_numeric_predictors_test<-filtered_numeric_predictors_test[,names(filtered_numeric_predictors_test) %in%
                                                                   c("LotFrontage", "LotArea", "MasVnrArea", "BsmtUnfSF",
                                                                     "X1stFlrSf", "X2ndFlrSF", "LowQualFinSF", "BsmtFullBath",
                                                                     "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr",
                                                                     "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageArea",
                                                                     "WoodDeckSF", "TotalSF_test", "PorchSF_test")]



other_numeric_predictors_train<-filtered_numeric_predictors_train[,!(names(filtered_numeric_predictors_train) %in%
                                                           c("LotFrontage", "LotArea", "MasVnrArea", "BsmtUnfSF",
                                                             "X1stFlrSf", "X2ndFlrSF", "LowQualFinSF", "BsmtFullBath",
                                                             "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr",
                                                             "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageArea",
                                                             "WoodDeckSF", "TotalSF_train", "PorchSF_train"))]


other_numeric_predictors_test<-filtered_numeric_predictors_test[,!(names(filtered_numeric_predictors_test) %in%
                                                                       c("LotFrontage", "LotArea", "MasVnrArea", "BsmtUnfSF",
                                                                         "X1stFlrSf", "X2ndFlrSF", "LowQualFinSF", "BsmtFullBath",
                                                                         "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr",
                                                                         "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageArea",
                                                                         "WoodDeckSF", "TotalSF_test", "PorchSF_test"))]


#Center and Scale / skew
#true_numeric_predictors_train<-na.omit(true_numeric_predictors_train)
#true_numeric_predictors_test<-na.omit(true_numeric_predictors_test)
a<-ggplot(train, aes(LotArea))+
  geom_histogram()

b<-ggplot(train, aes(LowQualFinSF))+
  geom_histogram()

c<-ggplot(train, aes(KitchenAbvGr))+
  geom_histogram()

d<-ggplot(train, aes(BsmtHalfBath))+
  geom_histogram()

grid.arrange(a,b,c,d, ncol = 2)


var_skew<-apply(true_numeric_predictors_train, 2, skewness)
head(sort(var_skew, decreasing = TRUE))
```

```{r}
true_numeric_predictors_train<-as.data.frame(
  apply(true_numeric_predictors_train, 2, FUN = function(x){log(x+1)}))
true_numeric_predictors_test<-as.data.frame(
  apply(true_numeric_predictors_test, 2, FUN = function(x){log(x+1)}))
```

###Center and Scale
Center and scaling numeric variables is done by subtracting the column mean and dividing by the column standard deviation. The purpose is to make the mean = 0 and standard deviation = 1. Center and scaling grants numerical stability and might help with multicollinearity. Also, the Lasso requires the predictors to be centered and scaled.
```{r}
true_numeric_predictors_train<-as.data.frame(apply(true_numeric_predictors_train, 2, scale))
true_numeric_predictors_test<-as.data.frame(apply(true_numeric_predictors_test, 2, scale))



all_numeric_predictors_train<-cbind(true_numeric_predictors_train, other_numeric_predictors_train)
all_numeric_predictors_test<-cbind(true_numeric_predictors_test, other_numeric_predictors_test)


```



##Factor Variables
###Near Zero Variance
While numeric variables are filtered for multicollinearity, factor variables are filtered for near zero variance. Near zero variance occurs when a factor variable does not have a balanced amount of data across levels. Or in other words, the variable has the same value for all observations or almost all observations. The caret package offers a function called nearZeroVar which handles this problem. Similar to findCorrelation, it returns the column index of variables you should remove. To use this function, you need to code factor variables as dummy variables. I did this using caret's dummyVars.

```{r}
dummy_variables_train<-dummyVars(~., data = factor_predictors_train)
dummy_variables_test<-dummyVars(~., data = factor_predictors_test)

factor_predictors_train_dummy<-as.data.frame(predict(dummy_variables_train, factor_predictors_train))
factor_predictors_test_dummy<-as.data.frame(predict(dummy_variables_test, factor_predictors_test))

#filter for near zero variance
zero_var<-nearZeroVar(factor_predictors_train_dummy)
names(factor_predictors_train_dummy[,zero_var])

factor_predictors_train_dummy<-factor_predictors_train_dummy[,-zero_var]
factor_predictors_test_dummy<-factor_predictors_test_dummy[,names(factor_predictors_test_dummy) %in% (names(factor_predictors_train_dummy))]
factor_predictors_train_dummy<-factor_predictors_train_dummy[,names(factor_predictors_train_dummy) %in% (names(factor_predictors_test_dummy))]

```
##Combine Numeric and Factor Variables
I combined numeric and factor variables and then added in SalePrice, TotalSF, and PorchSF.
```{r}
dim(factor_predictors_train_dummy)
dim(all_numeric_predictors_train)


all_predictors_train<-cbind(all_numeric_predictors_train, factor_predictors_train_dummy)
all_predictors_test<-cbind(all_numeric_predictors_test, factor_predictors_test_dummy)

dim(all_predictors_train)

all_predictors_train$SalePrice<-train$SalePrice
#all_predictors_train<-na.omit(all_predictors_train)
colnames(all_predictors_train)[colnames(all_predictors_train)=="TotalSF_train"] <- "TotalSF"
colnames(all_predictors_train)[colnames(all_predictors_train)=="PorchSF_train"] <- "PorchSF"

all_predictors_test$SalePrice<-test$SalePrice
#all_predictors_test<-na.omit(all_predictors_test)
colnames(all_predictors_test)[colnames(all_predictors_test)=="TotalSF_test"] <- "TotalSF"
colnames(all_predictors_test)[colnames(all_predictors_test)=="PorchSF_test"] <- "PorchSF"


```

##Linear Regression
###SalePrice (Response)
In regression, it is important to have a normally distributed response variable. The figure shows that SalePrice is skewed. Taking the logarithm sometimes fixes this issue. I applied the $log$ transformation and found that the skewed distribution was fixed. According to the figure below, the distribution of $log(SalePrice)$ is approximately normal. Thus, I will use $log (SalePrice)$ as my response variable. 
```{r}
ggplot(all_predictors_train, aes(SalePrice))+
  geom_histogram()+
  ggtitle("Distribution of SalePrice")+
  theme(plot.title = element_text(hjust = .5))

ggplot(all_predictors_train, aes(log(SalePrice)))+
  geom_histogram()+
  ggtitle("Distribution of log(SalePrice)")+
  theme(plot.title = element_text(hjust = .5))
```

###Outlier Detection and Removal
It is important to keep track of outliers when modeling. I wanted to check out Cook's distance an residuals vs fitted values plot for outlier detection. Upon doing so, I found observation 1299 and 524 to be heavy outliers. Since two values is small relative to the amount of data I have, I threw out these two observations.  
```{r}
linmod<-lm(log(SalePrice)~., data = all_predictors_train)
par(mfrow = c(2,2))
plot(linmod)
```



I re-ran the outlier detection scheme and found the graphs to be more assuring.
```{r}
par(mfrow=c(2,2))
plot(lm(log(SalePrice)~., data = all_predictors_train[-c(524,1299),]))
```

###Lasso
To create my Lasso model, I used the train function in the caret package. To make my results reproducible, I set the seed to 11. The model was trained based on 10-fold cross validation.
```{r}
set.seed(11)
ctrl<-trainControl(method = "cv", number = 10)
lasso.mod<-train(log(SalePrice)~., data = all_predictors_train[-c(524,1299),], method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = 1, lambda = seq(0, .05, length = 30)))
```

My best model used a $\lambda=.001724138$. My train RMSE was .1183659.
```{r}
lasso.mod$results
min(lasso.mod$results$RMSE)
lasso.mod$bestTune
```


Here I get it ready for submission and check the dimensions to make sure I didn't miss anything. 
```{r}
predictions<-as.data.frame(test$Id)
predictions$SalePrice<-exp(predict(lasso.mod, all_predictors_test))


colnames(predictions)<-c("Id", "SalePrice")
head(predictions)
```



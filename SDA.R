library(readxl)
All_variables_RIMWN_All_Imputed <- read_excel("C:/Users/kwang27/Desktop/kwang27/RIM_whole nation/ProcessedData/All variables_RIMWN_All_Imputed.xlsx")
View(All_variables_RIMWN_All_Imputed)

library("sda")

library("dplyr")

colnames(All_variables_RIMWN_All_Imputed)

removemissing <- All_variables_RIMWN_All_Imputed[,c(44, 1, 3:4,8,9,12:21, 23:34, 36:38, 40,42,43,45, 49,50,53,54,59:61,63:65,46,71)]
removemissing <- na.omit(removemissing)

missing_per_column <- colSums(is.na(removemissing))
print(missing_per_column)

XTrain <- as.matrix(removemissing[,2:45])
YTrain <- as.factor(removemissing$PrioriGroup)

regdata <- removemissing[,c(47,2,5, 10,11,13,14,15,16,19,20,23,24,26,29,30,32, 33,35,40,42,43,46)]

# Define intercept-only model
intercept_only <- lm(RIMScore_unweighted ~ 1, data=regdata)

# Define model with all predictors
all <- lm(RIMScore_unweighted ~ ., data=regdata)

# Perform both-direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

# View results of both-direction stepwise regression
both$anova

# View final model coefficients
both$coefficients


apsReg <- aps(regdata,"RIMScore_unweighted",as.list(colnames(regdata[,2:23])))
commonality_results <- commonality(apsReg)
print(commonality_results)

View(XTrain)
colnames(XTrain)
dim(XTrain)
length(YTrain)

ranking.LDA = sda.ranking(XTrain, YTrain, diagonal=FALSE)
ranking.DDA = sda.ranking(XTrain, YTrain, diagonal=TRUE)


plot(ranking.LDA[,"HC"], type="l")
which.max( ranking.LDA[1:44,"HC"] ) 

plot(ranking.DDA[,"HC"], type="l")
which.max( ranking.DDA[1:44,"HC"] ) 
plot(ranking.DDA, top=13)

ranking.LDA[1:5,]

XSelect <- XTrain[,ranking.DDA[1:13,c("idx")]]

sda.fit = sda(XSelect, YTrain, diagonal=TRUE)

postClass = predict(sda.fit, XSelect)

library(caret)

confusionMatrix(
  YTrain,
  postClass$class)

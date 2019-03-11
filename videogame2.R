library(plyr); library(dplyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(caret)
library(viridis)
library(reshape2)
library(scales)
library(kernlab)


video <- read.csv("r:/data/videogamesales/Video_Games_Sales_as_at_22_Dec_2016.csv")
video2 <- video

str(video)
summary(video)

## divide platforms into 2 types
portable <- c('3DS','DS','GBA','PSP','PSV')
type <- function(x){
        if (x %in% portable == TRUE) {return('PORT')}
        else{return('HOME')}
}
video$type <- sapply(video$Platform, type)

###########################

## make top 51 publishers/developers

table1 <- data.frame(table(video$Publisher))
table1 <- table1[order(-table1$Freq),]
noChange <- table1$Var1[1:25]
video$top51publisher <- (ifelse(video$Publisher %in% noChange, paste0("", video$Publisher), "Not in top 51")) 

video[video$Developer == "", ] <- NA
table2 <- data.frame(table(video$Developer))
table2 <- table2[order(-table2$Freq),]
noChange2 <- table2$Var1[1:25]
video$top51developer <- (ifelse(video$Developer %in% noChange2, paste0("", video$Developer), "Not in top 51"))

###########################

# video$id <- rownames(video)
videoSub <- video %>% dplyr::select(-Name, -Platform, -Year_of_Release, -Genre, -Publisher)
# videoSub2 <- video %>% dplyr::select(Global_Sales, Critic_Score, Critic_Count, User_Score, User_Count) %>% dplyr::mutate(logGS=log(Global_Sales), logUC = log(User_Count))
videoSub2 <- video %>% dplyr::select(type, Global_Sales, Critic_Score, Critic_Count, User_Score, User_Count) %>% dplyr::mutate(logSales=log(Global_Sales), logUserCount = log(User_Count))
videoSub2 <- videoSub2 %>% dplyr::select(-Global_Sales, -User_Count)
videoSub2$User_Score <- as.double(videoSub2$User_Score)


# plotMatrix2 <- ggpairs(data = videoSub2, mapping=aes(fill=type), upper = list(combo = "dot"), title = 'Correlations and distributions of continuous predictors') # , color= "sex"
# plotMatrix2

plotMatrix1 <- ggpairs(data = videoSub2, upper = list(combo = "dot"), title = 'Correlations and distributions of continuous predictors') # , color= "sex"
plotMatrix1

# videoSub2 <- na.omit(videoSub2)
# 
# 
# videoforDummy <- video %>% dplyr::select(id, Genre, Rating)
# dummy <- dummyVars("~ Rating + Genre", data = videoforDummy, fullRank=T)
# dummydf <- data.frame(predict(dummy, newdata=videoforDummy))

# videoSub3 <- video %>% dplyr::select(Global_Sales, Critic_Score, Critic_Count, User_Score, User_Count, Rating, Genre) %>% dplyr::mutate(logGS=log(Global_Sales), logUC = log(User_Count))
videoSub3 <- video %>% dplyr::select(Global_Sales, Critic_Score, Critic_Count, User_Score, User_Count, Rating, Genre, Year_of_Release, Publisher, NA_Sales, EU_Sales, JP_Sales, Other_Sales, type) %>% dplyr::mutate(NA_Sales = NA_Sales * 1000000, logUC = log(User_Count))

videoSub3[videoSub3$Rating == "", ] <- NA
videoSub3 <- na.omit(videoSub3)
videoSub3$User_Score <- as.double(videoSub3$User_Score)

videoSub3 <- videoSub3 %>% filter(Critic_Score > 60)

plot45 <- ggplot(data = videoSub3) + 
        ggtitle('Predictors vs Outcome') +
        geom_point(mapping = aes(x = Critic_Score, y = NA_Sales, color = type), size=5,alpha=.4) +
        geom_smooth(mapping = aes(x = Critic_Score, y = NA_Sales)) + 
        scale_y_continuous(name="Global Sales", limits=c(0, 10000000), labels=scales::dollar) + labs(x = 'Metacritic Critic Score') +
        scale_color_discrete(name="Platform Type",
                             breaks=c("HOME", "PORT"),
                             labels=c("Home", "Wireless or Portable"))


plot46 <- ggplot(data = videoSub3) + 
        geom_point(mapping = aes(x = Critic_Count, y = NA_Sales, color = Rating), size=5,alpha=.4) +
        geom_smooth(mapping = aes(x = Critic_Count, y = NA_Sales)) +
        scale_y_continuous(name="Global Sales", limits=c(0, 15000000), labels=scales::dollar) + labs(x = 'Metacritic Count')

videoSub3$User_Score <- as.integer(videoSub3$User_Score)
videoSub3mod <- videoSub3 %>% filter(User_Score > 50)

plot47 <- ggplot(data = videoSub3mod) + 
        geom_point(mapping = aes(x = User_Score, y = NA_Sales, color=type), size=5,alpha=.4) +
        geom_smooth(mapping = aes(x = User_Score, y = NA_Sales), method='loess') + 
        scale_x_continuous(name="User Score") + # limits=c(50, 100)
        scale_y_continuous(name="Global Sales", limits=c(0, 5000000), labels=scales::dollar) + labs(x = 'Metacritic User Score') + 
        scale_color_discrete(name="Platform Type",
                             breaks=c("HOME", "PORT"),
                             labels=c("Home", "Wireless or Portable"))

videoSub4 <- videoSub3 %>% dplyr::mutate(logGS = log(NA_Sales))

plot48 <- ggplot(data = videoSub4) + 
        geom_point(mapping = aes(x = logUC, y = logGS, color=type), size=5,alpha=.4) +
        geom_smooth(mapping = aes(x = logUC, y = logGS)) + labs(x = 'Log User Count', y = 'Log Global Sales') + 
        scale_color_discrete(name="Platform Type",
                             breaks=c("HOME", "PORT"),
                             labels=c("Home", "Wireless or Portable"))#+
#scale_y_continuous(name="Global Sales", limits=c(0, 25000000), labels=scales::dollar)
# plot45 <- ggplot(data = videoSub3) + 
#         geom_point(mapping = aes(x = Critic_Score, y = Global_Sales, color = Rating), size=5,alpha=.4) +
#         geom_smooth(mapping = aes(x = Critic_Score, y = Global_Sales)) + 
#         scale_y_continuous(name="Global Sales", limits=c(0, 25000000), labels=scales::dollar) + labs(x = 'Metacritic Critic Score')
# 
# 
# plot46 <- ggplot(data = videoSub3) + 
#         geom_point(mapping = aes(x = Critic_Count, y = Global_Sales, color = Rating), size=5,alpha=.4) +
#         geom_smooth(mapping = aes(x = Critic_Count, y = Global_Sales)) +
#         scale_y_continuous(name="Global Sales", limits=c(0, 25000000), labels=scales::dollar) + labs(x = 'Metacritic Count')
# 
# videoSub3mod <- videoSub3 %>% filter(User_Score > 50)
# 
# plot47 <- ggplot(data = videoSub3mod) + 
#         geom_point(mapping = aes(x = User_Score, y = Global_Sales, color=type), size=5,alpha=.4) +
#         geom_smooth(mapping = aes(x = User_Score, y = Global_Sales), method='loess') + 
#         scale_x_continuous(name="User Score") + # limits=c(50, 100)
#         scale_y_continuous(name="Global Sales", limits=c(0, 22500000), labels=scales::dollar) + labs(x = 'Metacritic User Score') + 
#         scale_color_discrete(name="Type",
#                             breaks=c("HOME", "PORT"),
#                             labels=c("Home", "Wireless or Portable"))
# 
# videoSub4 <- videoSub3 %>% dplyr::mutate(logGS = log(Global_Sales))
# 
# plot48 <- ggplot(data = videoSub4) + 
#         geom_point(mapping = aes(x = logUC, y = logGS, color=type), size=5,alpha=.4) +
#         geom_smooth(mapping = aes(x = logUC, y = logGS)) + labs(x = 'Log User Count', y = 'Log Global Sales') + 
#         scale_color_discrete(name="Type",
#                              breaks=c("HOME", "PORT"),
#                              labels=c("Home", "Wireless or Portable"))#+
#         #scale_y_continuous(name="Global Sales", limits=c(0, 25000000), labels=scales::dollar)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

multiplot(plot45,plot46,plot47,plot48, cols=2)
# ## dont know if keeping
# plot50 <- ggplot(data = videoSub3) + 
#         geom_bar(mapping = aes(x = Rating, fill = Genre))
# ## 

## Time series: how are video game sales globally overall doing?

videoSalesbyYear <- video2 %>% group_by(Year_of_Release, Rating) %>% summarise(Global_Sales=sum(Global_Sales), NA_Sales=sum(NA_Sales), JP_Sales=sum(JP_Sales), EU_Sales=sum(EU_Sales), Other_Sales=sum(Other_Sales))
test <- melt(videoSalesbyYear, id.vars=c('Year_of_Release'), measure.vars=c('NA_Sales', 'Global_Sales', 'JP_Sales', 'EU_Sales', 'Other_Sales'))
test[test$Year_of_Release == "", ] <- NA
test[test$value == "", ] <- NA
test[test$variable == "", ] <- NA


# test <- test %>% filter(Year_of_Release != 'N/A' | Year_of_Release != 2020 | Year_of_Release != 2017)
test <- na.omit(test)
test$Year_of_Release <- as.Date(test$Year_of_Release, format="%Y")
test$Year_of_Release <- gsub("-[0-9]{2}-[0-9]{2}$", "", test$Year_of_Release)
test$Year_of_Release <- as.numeric(test$Year_of_Release)
test <- test %>% group_by(Year_of_Release, variable) %>% summarise(sum=sum(value))

# videoSalesbyYear <- na.omit(videoSalesbyYear)
# videoSalesbyYear$date <- as.Date(videoSalesbyYear$Year_of_Release, format="%Y")
# videoSalesbyYear$date <- gsub("-10-08", "", videoSalesbyYear$date)
# videoSalesbyYear$date <- as.numeric(videoSalesbyYear$date)
# videoSalesbyYear <- videoSalesbyYear %>% group_by(date) %>% summarise(Global_Sales=sum(sum))
test <- test %>% filter(Year_of_Release != 'NA' & Year_of_Release != 2020 & Year_of_Release != 2017)
plot49 <- ggplot(data = test, aes(x = Year_of_Release, y = sum)) + geom_line(aes(color=variable), lwd=2) + labs(x='Date', y='Sales in Millions') + scale_y_continuous(labels=scales::dollar)
plot49


# plot51 <- ggplot(data = videoSalesbyYear) +
#         geom_smooth(mapping = aes(x = Year_of_Release, y = sum))
# plot51
        # geom_area(aes(fill=Genre))
        
        # geom_smooth(mapping = aes(x = logUC, y = Global_Sales)) +
        # scale_y_continuous(name="Global Sales", limits=c(0, 25000000), labels=scales::dollar)





## Which publishers have the most global sales?

videoPublisher <- video2 %>% group_by(Publisher) %>% summarise(Global = sum(Global_Sales), NorthAmerica = sum(NA_Sales), EU=sum(EU_Sales), Japan=sum(JP_Sales), Other = sum(Other_Sales)) %>% arrange(desc(Global)) %>% head(10)
videoPublisher <- gather(videoPublisher, "Region", "Total", 2:6) 
head(videoPublisher)

plot69 <- ( ggplot(videoPublisher, aes(Region, Publisher, fill = Total)) +
                   geom_tile(color = "white") +
                   ggtitle("Top 10 Publisher's Sales") +
                   scale_fill_viridis() +
                   geom_text(aes(label=Total), color='white') +
                   theme(legend.position = "top") )
plot69


# video[video$Rating == "", ] <- NA
video <- video[complete.cases(video),]
# videoforDummy <- video %>% dplyr::select(Genre, Rating)
# dummy <- dummyVars("~ .", data = videoforDummy, fullRank=T)
# dummydf <- data.frame(predict(dummy, newdata=videoforDummy))
# video <- cbind(video, dummydf)

# video <- video %>% dplyr::select(-Genre, -Rating, -Name, -Publisher, -Developer)
# video <- video %>% dplyr::select(-Name, -Publisher, -Developer)
# video <- video %>% dplyr::select(-Name, -Publisher, -Developer)

# nearzero <- nearZeroVar(video)

# dummy <- dummyVars("~ Genre + Platform + Year_of_Release + Rating + Publisher", data = train, fullRank=T)
# dummydf <- data.frame(predict(dummy, newdata=train))
# train3 <- cbind(dummydf, train)
# train3 <- 

########################################## MODEL BUILDING #########################################


# video <- video %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Publisher, -Name, -Developer)
video <- video %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Name, -Developer)
video$User_Score <- as.double(video$User_Score)
set.seed(888)
# index <- createDataPartition(video$NA_Sales, p=.75, list=FALSE)
# train <- video[index,]
# test <- video[-index,]

video <- na.omit(video)

video$Date <- as.Date(video$Year_of_Release, format="%Y")
video$Date <- gsub("-[0-9]{2}-[0-9]{2}$", "", video$Date)
video$Date <- as.numeric(video$Date)
trainDate <- video %>% filter(Date < 2010) %>% select(-Year_of_Release)
testDate <- video %>% filter(Date > 2010) %>% select(-Year_of_Release)

trainDate2 <- trainDate
trainDate2 <- trainDate2 %>% mutate(logNA_Sales=log(NA_Sales))
trainDate2$logNA_Sales[trainDate2$logNA_Sales == '-Inf'] <- 1
trainDate2 <- trainDate2 %>% select(-NA_Sales)

testDate2 <- testDate
testDate2 <- testDate2 %>% mutate(logNA_Sales=log(NA_Sales))
testDate2$logNA_Sales[testDate2$logNA_Sales == '-Inf'] <- 1
testDate2 <- testDate2 %>% select(-NA_Sales)


# train <- train %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Publisher, -Name, -Developer)
# train[train$NA_Sales == "", ] <- NA
train <- na.omit(train)
# train3 <- na.omit(train3)
# test[test$value == "", ] <- NA
# test[test$variable == "", ] <- NA

cls = makeCluster(4)
registerDoParallel(cls)

trainDate <- trainDate %>% select(-type)
trainDate$NA_Sales <- as.integer(trainDate$NA_Sales) 
trainDate$User_Score <- as.integer(trainDate$User_Score)

# controlObject <- trainControl(method = "repeatedcv", repeats = 5, number = 10)
linearReg <- train(NA_Sales ~ ., data=trainDate, method = "lm", trControl = trainControl(method="cv"), allowParallel = T)
# linearReg2 <- train(log(NA_Sales) ~ log(Critic_Score) + Platform + Genre + Publisher + log(Critic_Count) + log(User_Score) + log(User_Count) + Rating + Date, data=trainDate3, method = "lm", trControl = trainControl(method="cv"))
# linearReg3 <- train(NA_Sales ~ I(Critic_Score)^2 + Platform + Genre + Publisher + Critic_Count + User_Score + User_Count + Rating + Date, data=trainDate, method = "lm", trControl = trainControl(method="cv"))
# linearReg4 <- train(NA_Sales ~ ., data=trainDate, method = "lm", trControl = trainControl(method="cv"))

rfMod <- train(NA_Sales ~ ., data=trainDate, method = "rf", trControl = trainControl(method="cv"), num.trees = 1000)
CARTmod3 <- train(NA_Sales ~ ., data=trainDate, method = "rpart", trControl = trainControl(method="cv"))
# gbmGrid <- expand.grid(interaction.depth = seq(1,7, by=2), n.trees = seq(100, 1000, by=50), shrinkage = c(0.01, .1), n.minobsinnode = 10)
# gbmTune <- train(NA_Sales ~ ., data=train4, method = "gbm", trControl = trainControl(method="cv"), n.trees = 1000, verbose=FALSE, distribution="gaussian")
gbm.caret2 <- train(NA_Sales ~ .
                   , data=trainDate
                   , distribution="gaussian"
                   , method="gbm"
                   , trControl= trainControl(method="cv")
                   , verbose=FALSE
                   #, tuneGrid=caretGrid
                   , bag.fraction=0.75
)
gbm4 <- train(logNA_Sales ~  log(Critic_Score) + Platform + Genre + Publisher + log(Critic_Count) + log(User_Score) + log(User_Count) + Rating + Date
                    , data=trainDate2
                    , distribution="gaussian"
                    , method="gbm"
                    , trControl= trainControl(method="cv")
                    , verbose=FALSE
                    #, tuneGrid=caretGrid
                    , bag.fraction=0.75
)
# traindummy <- train
# dummy <- dummyVars("~ Genre + Platform + Year_of_Release + Rating + Publisher", data = traindummy, fullRank=T)
# dummydf <- data.frame(predict(dummy, newdata=traindummy))
# train3 <- cbind(dummydf, traindummy)
# train3 <- train3[,-nearZeroVar(train3)]
# svmMod <- train(NA_Sales ~ ., data=train3, method = "svmRadial", tuneLength = 8, preProc = c("center", "scale"))

svmMod2 <- ksvm(NA_Sales ~ ., data=trainDate, kernel="rbfdot", kpar="automatic", C=1, epsilon = 0.1, cross=10)
# trainDateY <- trainDate$NA_Sales
# trainDate2 <- trainDate[,-4]
# ridgeMod <- enet(x=as.matrix(trainDate2), y=trainDateY, lambda = 0.001)
# rlmfit <- rlm(NA_Sales ~ ., data=trainDate)

######################## EVALUATION AND PREDICTED ############################

rmseSVM <- sqrt(sum((svmpred-testDate$NA_Sales)^2 / nrow(testDate)))
plot(testDate$NA_Sales, svmpred, xlim=c(0,2.5))
abline(0,1,col='blue', lty=2)

ybar <- mean(testDate$NA_Sales)
tss <- sum((testDate$NA_Sales - ybar)^2)
rss <- sum((testDate$NA_Sales - svmpred)^2)
rsquared <- (tss - rss) / tss

resamp <- resamples(list("Linear Regression" = linearReg, "Tree" = CARTmod3, "rf" = rfMod, "gbm" = gbm.caret2))
# resamp <- resamples(list("Single Tree" = CARTmod3, "Linear Regression" = linearReg, "gbm" = gbm.caret, "lin2" = linearReg2))
summary(resamp)
RMSEmodels <- bwplot(resamp, metric = "RMSE", main='RMSE Between Models')
Rsquaredmodels <- bwplot(resamp, metric = "Rsquared", main='R Squared Between Models')
print(RMSEmodels, split = c(1, 1, 2,2), more = TRUE)
print(Rsquaredmodels, split = c(1, 2, 2,2), more = TRUE)

lmpred <- predict(linearReg, newdata=testDate)
CARTpred <- predict(CARTmod3, newdata=testDate)
gbmpred <- predict(gbm.caret2, newdata=testDate)
rfpred <- predict(rfMod, newdata=testDate)
gbmpred2 <- predict(gbm4, newdata=testDate)
# svmpred <- predict(svmMod2, newdata=testDate)

z1 <- postResample(lmpred, testDate$NA_Sales)
# z2 <- postResample(svmpred, testDate$NA_Sales)
z3 <- postResample(CARTpred, testDate$NA_Sales)
z4 <- postResample(rfpred, testDate$NA_Sales)
z5 <- postResample(gbmpred, testDate$NA_Sales)

dftest <- data.frame()
dftest <- rbind(dftest, z1, z3, z4, z5)
dftest$type <- c("lm", "singletree", "rf", "gbm")
names(dftest) <- c("rmse", "rsquared", "mae", "type")

plotRMSE <- ggplot(data = dftest, mapping = aes(x = type, y = rmse, fill = type)) + geom_bar(stat="identity") + ggtitle('RMSE Over Different Models') + xlab('model') #+ geom_point(size=5,alpha=.7) + coord_flip() + xlab('model') 
plotRMSE

plotrq <- ggplot(data = dftest, mapping = aes(x = type, y = rsquared, fill = type)) + geom_bar(stat="identity") + ggtitle('RSquared Over Different Models') + xlab('model') #geom_point(size=5,alpha=.7) + xlab('model') + ggtitle('RSquared Over Different Models') + coord_flip()
plotrq

plotrq <- ggplot(data = dftest) + geom_bar(mapping = aes(x = type , y = rsquared, group=1))
plotrq

# dftest99 <- data.frame()
# dftest99 <- rbind(dftest99, z1, z2, z3)

resid1 <- testDate$NA_Sales - lmpred
resid2 <- testDate$NA_Sales - CARTpred
resid3 <- testDate$NA_Sales - gbmpred
# resid4 <- testDate$NA_Sales - svmpred
resid5 <- testDate$NA_Sales - rfpred


plot(svmpred, resid4)
# abline(0,1,col='blue', lty=2)

plot(testDate$NA_Sales, gbmpred, xlim=c(0,8))
abline(0,1,col='blue', lty=2)

################################################### DIAGNOSTIC PLOTS

df1 <- data.frame(Sales=testDate$NA_Sales)
df1 <- cbind(df1, rfpred)

dfresiduals <- data.frame(residuals=resid1, observed=testDate$NA_Sales)

plotGBMpo <- ggplot(x=testDate$NA_Sales, y=gbmpred) + geom_point() + geom_abline(intercept=0)

plot65 <- ggplot(df1) + geom_point(mapping = aes(x = Sales, y = rfpred, colour = rfpred), size=5,alpha=.25) + geom_abline(intercept=0, slope=1) + xlab('observed') + ylab('predicted') + scale_color_continuous(name="Predicted") + ggtitle('Predicted vs Observed Random forest')
plot65

df2 <- data.frame(Sales=testDate$NA_Sales)
df2 <- cbind(df2, lmpred)

# plotGBMpo <- ggplot(x=testDate$NA_Sales, y=gbmpred) + geom_point() + geom_abline(intercept=0)

plotlmpo <- ggplot(df1) + geom_point(mapping = aes(x = Sales, y = lmpred, colour = lmpred), size=5,alpha=.25) + geom_abline(intercept=0, slope=1) + xlab('observed') + ylab('predicted') + scale_color_continuous(name="Predicted") + ggtitle('Predicted vs Observed Linear Model')
plotlmpo

plot66 <- ggplot(dfresiduals) + geom_point(mapping = aes(x = observed, y = residuals, colour = residuals), size=5,alpha=.25) + xlab('observed') + ylab('residuals') + geom_hline(yintercept=0) + ggtitle('Residuals vs Observed of Linear Model')
plot66

multiplot(plotlmpo, plot66)


df2 <- data.frame(R2)
r2gbm <- R2(gbmpred, testDate$NA_Sales)
r2lm <- R2(lmpred, testDate$NA_Sales)
r2CART <- R2(CARTpred, testDate$NA_Sales)
df2 <- rbind(df2, r2gbm)
df2 <- rbind(df2, r2lm)
df2 <- rbind(df2, r2CART)
df2$r2gbm <- r2gbm
df2$r2lm <- r2lm
df2$r2CART <- r2CART
rmsegbm <- RMSE(gbmpred, testDate$NA_Sales)
rmselm <- RMSE(lmpred, testDate$NA_Sales)
rmseCART <- RMSE(CARTpred, testDate$NA_Sales)
df2 <- cbind(df2, rmselm)
df2 <- cbind(df2, rmsegbm)
df2 <- cbind(df2, rmseCART)

df5 <- gather(df4, "type", "value", 1:6) 
df5 <- df5[!duplicated(df5),]

plot(testDate$NA_Sales, resid1, ylab='Residual')

# axis1 <- extendrange(c(testDate$NA_Sales, lmpred))
# plot(testDate$NA_Sales, resid1, ylab='Residual') # xlim=axis1, ylim=axis1)
# abline(h=0,col="blue", lty=2)
# # 
# plot(testDate$NA_Sales, lmpred, ylim=axis1, xlim=axis1)
# abline(0,1,col='blue', lty=2)
# 
# x <- R2(lmpred, testDate$NA_Sales)
# 
# df <- data.frame(R2='R2', RMSE= 'RMSE')

# gbmMod <- gbm(NA_Sales ~ ., data=train, distribution="gaussian")
# CARTmod <- train(NA_Sales ~ Genre + Critic_Score + Critic_Count + User_Count + Platform, data=train, method = "rpart", trControl = trainControl(method="cv"))

# CARTmod2 <- rpart(NA_Sales ~ Genre + Critic_Score + Critic_Count + User_Count + Platform, data=train, cp=0.02566789)
# prp(CARTmod2) # tree graph
# CARTmod4 <- rpart(NA_Sales ~ ., data=train, cp=0.02724707)

# trainSub <- train %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Name)
# linearReg2 <- train(NA_Sales ~ ., data=trainSub, method = "lm", trControl = controlObject)

# treeModel3 <- train(Global_Sales ~ ., data=train, method = "rpart", tuneLength = 10, trControl = controlObject)
# preds <- predict(treeModel, newdata=test)







train2 <- train %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Name)



trainY <- train$NA_Sales
train <- train %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales)
train <- train %>% dplyr::select(-Name)
pp_video <- preProcess(train[, -6], 
           method = c("center", "scale", "YeoJohnson"))
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1), .size = c(1:10), .bag = FALSE)
train <- train %>% dplyr::select(-Year_of_Release, -Publisher, -Developer, -Rating, -Platform)
train <- train %>% dplyr::select(-NA_Sales)
trainX <- train
# nnetTune2 <- train(trainX, trainY, method = "avNNet", tuneGrid = nnetGrid, linout=TRUE, trControl = trainControl(method="cv"), preProc = c("center", "scale"), trace = FALSE, MaxNWts = 5 * (ncol(trainX) +1 ) + 5 + 1 + 100, maxit = 500)
nnetTune3 <- train(trainX, trainY, method = "avNNet", trControl = trainControl(method="cv"), preProc = c("center", "scale"), trace = FALSE)

# dummy <- dummyVars("~ Genre + Year_of_Release", data = trainX, fullRank=T)
# dummydf <- data.frame(predict(dummy, newdata=trainX))
# trainX <- cbind(dummydf, trainX)
trainX <- trainX %>% select(-Genre)
trainX$User_Score <- as.integer(trainX$User_Score)

train2 <- train %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Name)
train2 <- train2 %>% dplyr::select(-newFactor, -top51publisher, -top51developer, -type)
nearzero <- nearZeroVar(train2)

dummy <- dummyVars("~ Genre", data = train2, fullRank=T)
dummydf <- data.frame(predict(dummy, newdata=train2))
train3 <- cbind(dummydf, train2)

train3 <- train3 %>% select(-Year_of_Release, -Publisher, -Genre, -Developer)
train2 <- train2 %>% select(-Publisher, -Year_of_Release, -Platform)
train2 <- train2 %>% select(-Platform)
train4 <- as.matrix(train2)
train2$Critic_Score <- as.double(train2$Critic_Score)
train2$Critic_Count <- as.double(train2$Critic_Count)
train2$User_Score <- as.double(train2$User_Score)
train2$User_Count <- as.double(train2$User_Count)
train3 <- train3 %>% select(-Genre, -Developer, -Rating)
lassoMod <- train(NA_Sales ~ ., data=train3, method = "lasso", trControl = trainControl(method="cv"), preProc = c("center", "scale"))

## lasso preds
dummy2 <- dummyVars("~ Genre", data = test, fullRank=T)
dummydf2 <- data.frame(predict(dummy2, newdata=test))
test <- cbind(dummydf2, test)
test <- test %>% select(-Year_of_Release, -Publisher, -Genre, -Developer, -top51publisher, -top51developer, -type)
test <- test %>% select(-newFactor, -Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales, -Name)
test <- test %>% select(-Rating) #  ** 
test <- test %>% select(-Platform)
test100 <- apply(test, 2, as.double)
test100 <- as.data.frame(test100)
lassopred <- predict(lassoMod, newdata=test100)
evalasso <- postResample(lassopred, test100$NA_Sales)

resamp <- resamples(list("Single Tree" = CARTmod3, "Linear Regression" = linearReg)) # "gbm" = gbm.caret))
summary(resamp)
RMSEmodels <- bwplot(resamp, metric = "RMSE", main='RMSE Between Models')
Rsquaredmodels <- bwplot(resamp, metric = "Rsquared", main='R Squared Between Models')
print(RMSEmodels, split = c(1, 1, 2,2), more = TRUE)
print(Rsquaredmodels, split = c(1, 2, 2,2), more = TRUE)

lassopred <- predict(lassoMod, newdata=test100)

# require(glmnet)
# ##returns variables from lasso variable selection, use alpha=0 for ridge
# ezlasso=function(df,yvar,folds=10,trace=F,alpha=1){
#         x<-model.matrix(as.formula(paste(yvar,"~.")),data=df)
#         x=x[,-1] ##remove intercept
#         
#         glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha)
#         
#         co<-coef(glmnet1,s = "lambda.1se")
#         inds<-which(co!=0)
#         variables<-row.names(co)[inds]
#         variables<-variables[!(variables %in% '(Intercept)')];
#         return( c(yvar,variables));
# }

# model <- rfe(data[,2:4], data[,1], sizes=c(1:4), rfeControl=ctrl)
# plot <-ggplot(model,type=c("g", "o"), metric="RMSE")+ scale_x_continuous(breaks = 2:4, labels = names(data)[2:4])

# nnetTune <- avNNet(trainX, trainY, size = 5, decay = .01, repeats = 5, linout = T, trace = F, maxit = 500, MaxNWts = 5 * (ncol(trainX) +1 ) + 5 + 1)
# nnetfit <- nnet(trainX, trainY, size=5, decay=.01, linout=T, trace=F, maxit=500, MaxNWts = 5 * (ncol(trainX) + 1) + 5 + 1)

resamp2 <- resamples(list("tree" = CARTmod, "linear reg" = linearReg, "test" = nnetTune3))
plot.rpart.obj <- function(rpart.obj, font.size = 0.8) {
        ## plot decision tree
        plot(rpart.obj,
             uniform   = T,    # if 'TRUE', uniform vertical spacing of the nodes is used
             branch    = 1,    # controls the shape of the branches from parent to child node
             compress  = F,    # if 'FALSE', the leaf nodes will be at the horizontal plot
             nspace    = 0.1,
             margin    = 0.1, # an extra fraction of white space to leave around the borders
             minbranch = 0.3)  # set the minimum length for a branch
        
        ## Add text
        text(x      = rpart.obj,   #
             splits = T,           # If tree are labeled with the criterion for the split
             all    = T,           # If 'TRUE', all nodes are labeled, otherwise just terminal nodes
             use.n  = T,           # Use numbers to annotate
             cex    = font.size)   # Font size
}


# par(mfrow=c(1,2)) # two plots on one page
# rsq.rpart(fit)

# 
# # train2 <- train %>% dplyr::select(-NA_Sales, -EU_Sales, -JP_Sales, -Other_Sales)
# # linearReg2 <- lm(Global_Sales ~ ., data=train2)
# # 
# # linearReg3 <- lm(Global_Sales ~ Genre + Critic_Score + Critic_Count + User_Count + Platform, data=train2)
# train3 <- train %>% dplyr::select(-Global_Sales, -EU_Sales, -JP_Sales, -Other_Sales)
# linearReg4 <- lm(NA_Sales ~ Genre + Critic_Score + Critic_Count + User_Count + Platform, data=train3)




# predictors <- names(train2)
# f <- as.formula(paste("NA_Sales ~", paste(predictors[!predictors %in% "NA_Sales"], collapse = "+")))
# print(f)
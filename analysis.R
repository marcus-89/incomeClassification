library(tidyverse)
library(RColorBrewer)
library(tree)
library(randomForest)
library(ROCR)
library(plotROC)
library(e1071)

####loading, processing and cleaning data####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df<-read.csv("adult.data",header=F,strip.white=T)
colnames(df) <- c("age","workclass","weight", "education", 
                  "edunum", "marital", "occupation", "relationship", 
                  "race", "sex", "capgain", "caploss", 
                  "hrsperwk", "native", "income")
str(df)

#check for potential missing data of various shape
sum(is.na(df)) #0
colSums(df == 0) #only capgain/caploss have values of zero, which does not seem unreasonable
sapply(df,levels) #several factor variables have "?" levels, with no further information given elsewhere, let's remove these

levels(df$workclass)[1] <- NA
levels(df$occupation)[1] <- NA
levels(df$native)[1] <- NA

sum(is.na(df)) #~4200 occurences in ~2k observations
df <- na.omit(df) #we remove these and focus on the remaining 30k observations

#certain variables are either already explained through other variables or not wanted (weight) and can be removed
df <- subset(df, select = -c(edunum, weight, marital))

#an overview of the conditional distribution of Y (income class) given each factor
factors <- c("workclass","education", "occupation", "relationship", "race", "sex", "native")
sapply(df[factors],function(x)table(x,df$income))

#many of these contain a lot of factor levels and may be aggregated into wider groups

##combining countries into continent/regions
central.america <- c("Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                     "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                     "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South")
north.america <- c("United-States", "Canada")

df$native <- as.character(df$native)
df$native[df$native %in% central.america] <- "Central America and Caribbean"
df$native[df$native %in% asia] <- "Asia"
df$native[df$native %in% south.america] <- "South America"
df$native[df$native %in% europe] <- "Europe"
df$native[df$native %in% other] <- "Other"
df$native[df$native %in% north.america] <- "North America"

table(df$native, df$income)

##combining a few work class categories equivalently
df$workclass <- as.character(df$workclass)

df$workclass[df$workclass == "State-gov" |
               df$workclass == "Local-gov"] <- "Other-gov"
df$workclass[df$workclass == "Self-emp-inc" |
               df$workclass == "Self-emp-not-inc"] <- "Self-employed"
df$workclass[df$workclass == "Without-pay" |
               df$workclass == "Never-worked"] <- "Unemployed"

table(df$workclass, df$income)

##combining "wife" and "husband" into "married" in the relationship factor
df$relationship <- as.character(df$relationship)
df$relationship[df$relationship == "Husband" |
                  df$relationship == "Wife"] <- "Married"

##combining all <highschool education categories into simply "no diploma"
df$education <- as.character(df$education)
df$education[df$education == "Preschool" | 
               df$education == "1st-4th" |
               df$education == "5th-6th" |
               df$education == "7th-8th" |
               df$education == "9th" |
               df$education == "10th" |
               df$education == "11th" |
               df$education == "12th"] <- "No high school diploma"

df$education[df$education == "Assoc-acdm" |
               df$education == "Assoc-voc"] <- "Associate degree"

table(df$education, df$income)

##finally, occupation has a lot of categories, however many of them 
##can't necessarily be combined without losing too much information
df$occupation <- as.character(df$occupation)

df$occupation[df$occupation == "Other-service" |
                df$occupation == "Priv-house-serv"] <- "Service"

table(df$occupation, df$income)

#revert into factors from characters after the changes
df$native <- as.factor(df$native)
df$workclass <- as.factor(df$workclass)
df$occupation <- as.factor(df$occupation)
df$education <- as.factor(df$education)
df$relationship <- as.factor(df$relationship)

str(df)

####descriptive plots####

#custom ggplot theme
custom_theme <- function() {
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  theme_bw(base_size=9) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#create a df summarizing count of all factors
df %>%
  select(c(factors,income)) %>%
  gather(key = "factor", value = "value", factors) %>%
  count(factor, income, value) %>%
  group_by(value) %>%
  mutate(percent = n/sum(n), 
         label = paste0(sprintf("%.0f", 100*percent), "%")) -> count.df

#plot a bar chart of workclass by income

count.df %>% filter(factor == "workclass") %>%
ggplot(aes(x = value, y = n, fill = income)) +
  geom_bar(stat = "identity", width = .7, position = position_stack()) +
  geom_text(aes(label = label), size = 2, position = position_stack(vjust=0.7)) + 
  custom_theme() +
  labs(x = "Workclass", y = "Count", fill = "Income") -> factor_barplot
factor_barplot

#and for education
factor_barplot %+% 
  subset(count.df, factor == "education") + 
  labs(x = "Education")

#...occupation....
factor_barplot %+% 
  subset(count.df, factor == "occupation") + 
  labs(x = "Occupation") +
  theme(axis.text.x=element_text(angle=30, vjust=0.5))

#...relationship
factor_barplot %+% 
  subset(count.df, factor == "relationship") + 
  labs(x = "Relationship")

#...race...
factor_barplot %+% 
  subset(count.df, factor == "race") + 
  labs(x = "Race")

#...sex...
factor_barplot %+% 
  subset(count.df, factor == "sex") + 
  labs(x = "Sex")

#...and finally native
factor_barplot %+% 
  subset(count.df, factor == "native") + 
  labs(x = "Native")

#next, a brief summary of the distribution of the continuous variables
#first out, age
ggplot(df, aes(x=age, fill=income)) + 
  geom_histogram(bins = 30, alpha=0.4, position="identity") + 
  custom_theme() +
  labs(x = "Age", y = "Count", fill = "Income")

#repeat for the remaining variables
ggplot(df, aes(x=hrsperwk, fill=income)) + 
  geom_histogram(alpha=0.4, position="identity") + 
  custom_theme() +
  labs(x = "Hours per week", y = "Count", fill = "Income")

ggplot(df, aes(x=capgain, fill=income)) + 
  geom_histogram(alpha=0.4, position="identity") + 
  custom_theme() +
  labs(x = "Capital gains", y = "Count", fill = "Income")

ggplot(df, aes(x=caploss, fill=income)) + 
  geom_histogram(alpha=0.4, position="identity") + 
  custom_theme() +
  labs(x = "Capital losses", y = "Count", fill = "Income")

#and finally, the distribution of the income classes overall
df %>%
  dplyr::select(income) %>%
  group_by(income) %>%
  summarise(count = n()) %>%
  mutate(percent = 100*count/sum(count), label = paste0(sprintf("%.2f", percent), "%")) %>%
  arrange(income, desc(income)) %>%
  ggplot(aes(x = factor(income), y = count, fill = income)) +
  geom_bar(stat = "identity", width = .7, position = position_stack()) +
  geom_text(aes(label = label), size = 4, position = position_stack(vjust=0.7)) +
  theme(axis.text.x=element_text(angle=30, vjust=0.5)) + 
  custom_theme() + 
  labs(x = "Income level", y = "Count", fill = "Income")

#which may not seem very interesting at a first glance, but it allows us to set a threshold for the lowest
#reasonable misclassification rate. obviously a misclassification rate of anything above 1-0.7511 is entirely
#useless since we could obtain better by just classifying everyone as <=50k. let's save that for later

basemcr = mean(as.numeric(df$income)-1)

####modelling####

#generate training/test partitions
train = sample(1:nrow(df), nrow(df)*0.8)
y_test<-df[-train,12]

##Classification tree
m1.dt = tree(income~.,df,subset=train)

summary(m1.dt)
plot(m1.dt,type="uniform",col="dark blue",lwd=2);text(m1.dt,pretty=0,col = "dark red",cex=1)
#(unpruned tree)

#predicting the test set using the unpruned tree
yhat.dt=predict(m1.dt,newdata=df[-train,],type="class")

#calculate misclassification rate
pred.dt<-table(yhat.dt,y_test)
mcr.dt <- 1-(pred.dt[1,1]+pred.dt[2,2])/sum(pred.dt) 

#similarly, sensitivity and specificity
spec.dt<-pred.dt[1,1]/sum(pred.dt[,1]) 
sens.dt<-pred.dt[2,2]/sum(pred.dt[,2])

mcr.dt;sens.dt;spec.dt

#prune the tree, 5 terminal nodes seem fine based on the plot above
m2.dt <- prune.misclass(m1.dt,best=5)

summary(m2.dt)
plot(m2.dt,type="uniform",col="dark blue",lwd=2);text(m2.dt,pretty=0,col = "dark red",cex=1)

#predict on testset
yhat2.dt=predict(m2.dt,newdata=df[-train,],type="class")

#calculate misclassification rate again
pred.dt_pruned<-table(yhat2.dt,y_test)
mcr.dt_pruned <- 1-(pred.dt_pruned[1,1]+pred.dt_pruned[2,2])/sum(pred.dt_pruned)

#sens and spec
spec.dt_pruned<-pred.dt_pruned[1,1]/sum(pred.dt_pruned[,1]) #5 % för 0 (<= 50K)
sens.dt_pruned<-pred.dt_pruned[2,2]/sum(pred.dt_pruned[,2]) #51 % för 1 (> 50K)

#no loss in prediction, but much more comprehensible model, that'll do
mcr.dt_pruned;sens.dt_pruned;spec.dt_pruned


##Random forest
m1.rf=randomForest(income~.,df,subset=train,importance=TRUE,mtry=3) #ntree=500, m set to floor(sqrt(11)) = 3
m1.rf

plot(m1.rf)
#500 may have been overkill, not really necessary

m1.rf

importance(m1.rf)
varImpPlot(m1.rf)
#race and native does not contribute much at all, but first, let's predict the test set and save the performance

#predict testset
yhat.rf=predict(m1.rf,newdata=df[-train,])


#check misclassification
pred.rf<-table(yhat.rf,y_test)
mcr.rf <- 1-(pred.rf[1,1]+pred.rf[2,2])/sum(pred.rf) #1-0.867 = 13.3 misclassification rate


#sens and spec
spec.rf<-pred.rf[1,1]/sum(pred.rf[,1]) #7 % för 0 (<= 50K)
sens.rf<-pred.rf[2,2]/sum(pred.rf[,2]) #36 % för 1 (> 50K)

#another randomForest, excluding race and native. and as the error rates of the previous model suggested, 
#we can lower ntree to 200 without any performance loss
m2.rf=randomForest(income~.-race-native,df,subset=train,importance=TRUE,mtry=3, ntree=200)

plot(m2.rf)

#predict testset
yhat.rf2=predict(m2.rf,newdata=df[-train,])


#check misclassification
pred.rf2<-table(yhat.rf2,y_test)
mcr.rf2 <- 1-(pred.rf2[1,1]+pred.rf2[2,2])/sum(pred.rf2) #1-0.867 = 13.3 misclassification rate


#sens and spec
spec.rf2<-pred.rf2[1,1]/sum(pred.rf2[,1]) #7 % för 0 (<= 50K)
sens.rf2<-pred.rf2[2,2]/sum(pred.rf2[,2]) #36 % för 1 (> 50K)

mcr.rf;sens.rf;spec.rf
mcr.rf2;sens.rf2;spec.rf2
#relatively the same, very slight loss in performance when removing variables, but to not lose absolutely zero information
#is not expected

##logistic regression
#fit training set to the model
m1.log <- glm(income ~.,family=binomial(link='logit'),data=df,subset=train)

#generate roc curve
p <- predict(m1.log,newdata=df[-train,],type="response")
pr <- prediction(p, y_test)
prf <- performance(pr, measure = "sens", x.measure = "spec")
plot(prf);abline(a=1,b=-1)


#since we receive probabilities with logistic reg, we should determine a threshold to classify as 1 or 0.
y.roc <- unlist(prf@y.values)
x.roc <- unlist(prf@x.values)
alpha.roc <- unlist(prf@alpha.values)


#n obs Y = 1 and 0 in training set
nx<-summary(df$income[train])[1]
ny<-summary(df$income[train])[2]

#loop number of misclassified obs on each point on the ROC curve to find a minimum
mscftot <- vector("numeric",length(y.roc))
for(i in 1:length(y.roc)){
  mscftot[[i]] <- (1-y.roc[i])*ny + (1-x.roc[i])*nx  
}

#result
plot(mscftot,type="l")
#threshold for minima
threshold<-alpha.roc[which.min(mscftot)]

#new ROC-curve with optimum added
plot(prf);abline(a=1,b=-1)
points(x.roc[which.min(mscftot)],y.roc[which.min(mscftot)],pch=19,col="red")
abline(h=y.roc[which.min(mscftot)],lty=2)
abline(v=x.roc[which.min(mscftot)],lty=2)


#code prediction according to the decided threshold
yhat.log <- predict(m1.log,newdata=df[-train,],type="response")
yhat.log <- ifelse(yhat.log>threshold,1,0)
yhat.log <- as.factor(yhat.log)

#misclassification rate
pred.log <- table(yhat.log,y_test)
mcr.log <- 1-(pred.log[1,1]+pred.log[2,2])/sum(pred.log)

#sens and spec
spec.log<-pred.rf[1,1]/sum(pred.rf[,1]) 
sens.log<-pred.rf[2,2]/sum(pred.rf[,2]) 


#above plots recoded into ggplots for consistency

rocplot.log <- data.frame(y_test,p)

#basic ROC-curve for log reg 
ggplot(rocplot.log, aes(d=y_test, m=p)) + geom_roc(n.cuts=10) +
  style_roc(ylab="Sensitivity", xlab="1-Specificity") + custom_theme()

#misclassification-curve based on points on roc curve
misclass.log <- data.frame(mscftot)
ggplot(misclass.log, aes(y=mscftot, x=seq(1, length(mscftot)))) + geom_line() +
  xlab("Index") + ylab("Number of misclassifications") + custom_theme()

#the ROC curve with chosen threshold marked
ggplot(rocplot.log, aes(d=y_test, m=p)) + geom_roc(cutoffs.at = threshold, cutoff.labels=round(threshold,3)) +
  style_roc(ylab="Sensitivity", xlab="1-Specificity") + geom_vline(xintercept=1-x.roc[which.min(mscftot)],lty=2) +
  geom_hline(yintercept=y.roc[which.min(mscftot)],lty=2) + geom_abline(intercept=0,slope=1) + custom_theme()

##SVM
#rbf kernel, 10 fold cross validation on the training set, tuning over a few gamma and cost parameter values
#(computationally heavy, run it without the cross val and tuning if time is prioritized)
m1.svm <- tune.svm(income~.,data=df[train,], kernel = "radial", gamma = 10^(-2:0), cost = 10^(-1:1))
#best performing model
summary(m1.svm)
#fit according to speicification above
m2.svm <- svm(income~.,data=df,subset=train,probability=T, 
              gamma = m1.svm$best.parameters[1,1], cost = m1.svm$best.parameters[1,2])

#predict
svm.yhat <- predict(m2.svm,newdata=df[-train,],class=T)

#calculate performance key values
pred.svm <- table(svm.yhat,y_test)

mcr.svm <- 1-(pred.svm[1,1]+pred.svm[2,2])/sum(pred.svm) 

spec.svm<-pred.svm[1,1]/sum(pred.svm[,1])
sens.svm<-pred.svm[2,2]/sum(pred.svm[,2])


####Results####
pred.summaries <- data.frame(c("Classification tree", "Random forest", "Logistic regression", "SVM"))
colnames(pred.summaries)<-"model"
pred.summaries$sens <- c(sens.dt_pruned,sens.rf2,sens.log,sens.svm)
pred.summaries$spec <- c(spec.dt_pruned,spec.rf2,spec.log,spec.svm)
pred.summaries$mcr <- c(mcr.dt_pruned, mcr.rf2, mcr.log, mcr.svm)
pred.summaries$mcr.label = paste0(sprintf("%.2f", pred.summaries$mcr*100), "%")
pred.summaries$sens.label <- paste0(sprintf("%.2f", pred.summaries$sens*100), "%")
pred.summaries$spec.label <- paste0(sprintf("%.2f", pred.summaries$spec*100), "%")


#misclassification rate plot
plot.mcr<-ggplot(pred.summaries, aes(x=model,y=mcr,fill=model)) + 
  geom_bar(stat="identity") + 
  ylim(0,0.3) + 
  geom_hline(yintercept=basemcr,lty=2) + 
  labs(x = "Model", y = "Misclassification rate", fill = "Model") + 
  geom_text(aes(label = mcr.label), size = 4, position = position_stack(vjust=0.7)) + 
  custom_theme()
plot.mcr

#plot of sensitivity
plot.sens<-ggplot(pred.summaries, aes(x=model,y=sens,fill=model)) + 
  geom_bar(stat="identity") +
  ylim(0,1) + 
  labs(x = "Model", y = "Sensitivity", fill = "Model") + 
  geom_text(aes(label = sens.label), size = 4, position = position_stack(vjust=0.7)) + 
  custom_theme()

plot.sens

#specificity
plot.spec<-ggplot(pred.summaries, aes(x=model,y=spec,fill=model)) + 
  geom_bar(stat="identity") +
  labs(x = "Model", y = "Specificity", fill = "Model") + 
  geom_text(aes(label = spec.label), size = 4, position = position_stack(vjust=0.7)) + 
  custom_theme()

plot.spec

#combined ROC curve

#roc log
p <- predict(m1.log,newdata=df[-train,],type="response")
pr <- prediction(p, y_test)
prf <- performance(pr, measure = "sens", x.measure = "spec")
plot(prf);abline(a=1,b=-1)

#roc dt
p2 <- predict(m2.dt,newdata=df[-train,],type="vector")[,2]
pr2 <- prediction(p2, y_test)
prf2 <- performance(pr2, measure = "sens", x.measure = "spec")
plot(prf2);abline(a=1,b=-1)

#roc rf
p3 <- predict(m2.rf,newdata=df[-train,],type="prob")[,2]
pr3 <- prediction(p3, y_test)
prf3 <- performance(pr3, measure = "sens", x.measure = "spec")
plot(prf3);abline(a=1,b=-1)

#roc svm
p4 <- predict(m2.svm,newdata=df[-train,],probability=T)
p4 <- attr(p4, "probabilities")[,2]
pr4 <- prediction(p4, y_test)
prf4 <- performance(pr4, measure = "sens", x.measure = "spec")
plot(prf4);abline(a=1,b=-1)


combined.roc <- data.frame(y_test,p2,p3,p4,p)
colnames(combined.roc) <- c("y","Decision tree","Random forest", "SVM", "Logistic regression")
combined.roc<-gather(combined.roc, "Decision tree", "Random forest", "SVM", "Logistic regression", key="model", value="prob")


ggplot(combined.roc, aes(d=y, m=prob, color=model)) + geom_roc(n.cuts=0) +
  style_roc(ylab="Sensitivity", xlab="1-Specificity") + geom_abline(intercept=0,slope=1) +
  scale_color_discrete(name="Model") + custom_theme() + theme(legend.position="right")



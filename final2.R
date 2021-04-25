library(dplyr)
library(stringr)
library(forcats)
library(caret)
library(randomForest)
library(gridExtra)
library(e1071)
library(ranger)
library(car)

#dataset top 10 brands only
filter_top <- autobazar%>%count(brand, sort=T)%>%slice(1:10)
top10_brands <- filter(autobazar, brand %in% filter_top$brand & year_data >1999)

top10_brands<- top10_brands[,c(1,4,5, 7:12)]

visdat::vis_miss(top10_brands)

#to replace missing "Power" data by average kWs of each Brand

for (b in c(unique(top10_brands$brand))){
  mean_kw <- as.numeric(top10_brands%>%filter(brand == b)%>%group_by(brand)%>%summarise(m=mean(kw_data, na.rm=T)))  
  mis <- which(top10_brands$brand == b & is.na(top10_brands$kw_data)==TRUE)
  top10_brands[mis, 7] <- mean_kw[2]
}

#basic treatments for variables

top10_brands$brand <- as.factor(top10_brands$brand)
top10_brands$diesel_data <- as.factor(top10_brands$diesel_data)
top10_brands$transmission_data <- as.factor(top10_brands$transmission_data)
top10_brands$type_data<-as.factor(top10_brands$type_data)
top10_brands$quattro<-as.factor(top10_brands$quattro)
top10_brands$year_data<-as.numeric(top10_brands$year_data)


top10_brands<- na.omit(top10_brands)

#outliers detection displayed by brands

top10_brands%>%group_by(brand)%>%ggplot(.,aes(price_data))+geom_boxplot()+facet_wrap(~brand)+labs(x="Price")
top10_brands%>%group_by(brand)%>%ggplot(.,aes(km_data))+geom_boxplot()+facet_wrap(~brand)+labs(x="Mileage (km)")
top10_brands%>%group_by(brand)%>%ggplot(.,aes(kw_data))+geom_boxplot()+facet_wrap(~brand)+labs(x="Power (kW)")



#deleting serious outliers

outlier <- which((top10_brands$brand == "Kia" & top10_brands$price_data >150000) | (top10_brands$brand == "Kia" & top10_brands$kw_data >550))
top10_brands<-top10_brands[-outlier, ]

#relationships between variables
top10_brands%>%group_by(brand)%>%ggplot(.,aes(log(price_data), km_data))+geom_point()+facet_wrap(~brand)+labs(x="Log(Price)", y="Mileage (km)")
top10_brands%>%group_by(brand)%>%ggplot(.,aes(log(price_data), kw_data))+geom_point()+facet_wrap(~brand)+labs(x="Log(Price)", y="Power (kW)")

#FACTORS treatments
#Grouping less common factors into common group

top10_brands$diesel_data <-fct_collapse(top10_brands$diesel_data, Benzín = "Benzín+Plyn", Elektro = "Hybrid")
unique(top10_brands$diesel_data)

top10_brands%>%group_by(type_data)%>%count(type_data)%>%ggplot(.,aes(x=reorder(type_data,n), y=n, fill=type_data))+geom_bar(stat="identity")+coord_flip()+
  labs(y = "Count", x= "Car's body type")
unique(top10_brands$type_data)

top10_brands$type_data <-fct_collapse(top10_brands$type_data, Iné = c("Dodávka", "Van", "Iné", "Roadster", "Bus",
                                                                      "Cabrio", "Pick up" ))
###zero variance predictors
nzv <- nearZeroVar(top10_brands, saveMetrics= TRUE)
print(nzv)

#multicollinearity check
car::vif(lm(price_data ~ ., data=top10_brands))


#DATA SPLITTING into TRAINING and TEST datasets

set.seed(25892)
trainIndex <- createDataPartition(top10_brands$price, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- top10_brands[ trainIndex,]
test  <- top10_brands[-trainIndex,]

########MODELS########

#Specification of resampling method for training sets

fitControl2 <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  verboseIter = TRUE)


###LOG - LINEAR REGRESSION 
set.seed(20512)
model.lm <- train(log(price_data) ~ .,
                  data = train,
                  method = "lm",
                  trControl = fitControl2)

p_lm <- predict(model.lm, newdata = test)
p1 <- postResample(pred = exp(p_lm), obs = test$price_data)

print(p1)
summary(model.lm)


###RANDOM FOREST Regression

#TUNNING PARAMETERS for Random Forests
gr <- expand.grid(mtry= c(6:15),
                  splitrule = c("variance", "extratrees"),
                  min.node.size=c(1,5,10))

#Random Forest

set.seed(20512)
model.tree2_250 <- train(price_data ~ .,
                         data = train,
                         method = "ranger",
                         trControl = fitControl2,
                         tuneGrid = gr,
                         num.trees = 250,
                         importance = "impurity",
                         verbose = T)

p_tree <- predict(model.tree2_250, newdata = test)
p4 <- postResample(pred = p_tree, obs = test$price_data)
print(p4)

model.tree2_250


#Plots

plot(model.tree2_250)


densityplot(model.tree2_250, pch = "|", resamples = "all")

plot(varImp(model.tree2_250))

#Predicted vs Observed table for RF

perc_tree<- cbind(p_tree, test$price_data)

diff <- abs(perc_tree[,2]-perc_tree[,1])
perc2<- cbind(p_tree, test$price_data, diff, as.character(test$brand))
perc2 <- as.data.frame(perc2)
perc2$V4 <- as.numeric(perc2$V4)


#Predicted vs Observed table for Reg

perc<- cbind(exp(p_lm), test$price_data)

diff <- abs(perc[,2]-perc[,1])
perc1<- cbind(exp(p_lm), test$price_data, diff, as.character(test$brand))
perc1 <- as.data.frame(perc1)
perc1$V4 <- as.numeric(perc1$V4)

#SMAPE

smape_reg <- mean(abs(perc[,2]-perc[,1])/((abs(perc[,1])+abs(perc[,2]))/2))

smape_rf <- mean(abs(perc_tree[,2]-perc_tree[,1])/((abs(perc_tree[,1])+abs(perc_tree[,2]))/2))


#MAPE

mape_reg <- mean(abs((perc[,2]-perc[,1])/perc[,2]), na.rm = T)

mape_rf <- mean(abs((as.numeric(perc2[,2])-as.numeric(perc2[,1]))/as.numeric(perc2[,2])), na.rm = T)

#Comparison Table

table <- data.frame(c(SMAPE = smape_reg, MAPE = mape_reg, p1[1], p1[3]), c(SMAPE = smape_rf, MAPE = mape_rf,  p4[1], p4[3]))
colnames(table) <- c("Regression", "Random Forest")
print(table)

###Comparison graphs

#Prediction's errors by brands
graph_lm <- perc1%>%group_by(V5)%>%summarise(mean1 = mean(V4)) %>%ggplot(.,aes(x=V5, y=mean1))+geom_bar(stat="identity")
graph_tree<-perc2%>%group_by(V5)%>%summarise(mean1 = mean(V4)) %>%ggplot(.,aes(x=V5, y=mean1))+geom_bar(stat="identity")

graph_tree
graph_lm


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

plot1 <- bwplot(p1)
plot2<- bwplot(p4)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

saveRDS(model.tree2_250, "model.tree2_250.rds")
saveRDS(model.lm, "model.lm.rds")

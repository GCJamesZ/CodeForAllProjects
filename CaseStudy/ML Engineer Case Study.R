# install.packages("readxl")
rm(list=ls())
setwd("Z:/CurrQuarter/ML Engineer")
library(readxl)
library(tidyverse)
library(ggplot2)
data = read_excel("data.xlsx",sheet="Transactions data")

# prod_hiearchy = read_excel("Product_Hierarchy.xlsx")
# str(prod_hiearchy)


demo_data = read_excel("Demographic Data.xlsx", sheet = "Data")
demo_data=demo_data %>% mutate(Zipcode = substr(Zipcode, 1, 5))
demo_data[, 1:ncol(demo_data)] <- sapply(demo_data[, 1:ncol(demo_data)], as.numeric)
demo_data$Zipcode = as.numeric(demo_data$Zipcode)
demo_data=demo_data %>% group_by(Zipcode) %>% summarise(across(everything(), list(mean)))


# store info clean
store_info = read_excel("data.xlsx",sheet="Store info")
store_info$`Store Num` =as.factor(store_info$`Store Num`)
store_info$Zipcode = as.numeric(store_info$Zipcode)
store_info = store_info %>% select(c(`Store Num`,Zipcode))
# str(store_info)

unique_zip = unique(store_info$Zipcode)
demo_data %>% filter(Zipcode %in% unique_zip) %>% print

# Combined store info and demo info
store_demo_info = left_join(store_info, demo_data, 
                            by = "Zipcode")
# str(store_demo_info)
# print(store_demo_info)

unique_store  = unique(data$`Store Num`)
unique_item = unique(data$SLD_MENU_ITM_ID)
length(unique_store)
length(unique_item)
# 
# for(i in 1:length(unique_item)){
#   tem_data = data %>% filter(`SLD_MENU_ITM_ID` == unique_item[i])
#   g1 = ggplot(data = tem_data, aes(WK_END_DT, TOT_UNITS)) +
#     geom_line(color = "steelblue", size = 1) +
#     geom_point(color = "steelblue") +
#     facet_wrap(~ `Store Num`)+
#     labs(title=unique_item[i])
#   print(g1)
# }

guest_count = read_excel("data.xlsx",sheet="Guest count data")
# correct guest data type
guest_count$`Store Num` = as.factor(guest_count$`Store Num`)
guest_count$WK_END_DT = as.Date(guest_count$WK_END_DT)
guest_count = guest_count %>% select(c(`Store Num`,totalguestcount,WK_END_DT))
# str(guest_count)



# correct transaction data type
data$WK_END_DT = as.Date(data$WK_END_DT)
data$`Store Num` = as.factor(data$`Store Num`)
data$SLD_MENU_ITM_ID = as.factor(data$SLD_MENU_ITM_ID)

#join transaction data and guest data
data = left_join(data, guest_count, 
                 by = c("WK_END_DT","Store Num"))
# str(data)


# clean transaction data
c.data = data %>% select(-c(WK_END_THU_ID_NU,	
                                WK_END_THU_WK_NU,	
                                WK_END_THU_END_YR_NU,
                            SMI_DESC,
                            COMBO_FLG))
# str(c.data)


#Add previous 4 weeks data
curr.data = c.data %>% select(c(WK_END_DT,
                                `Store Num`,
                                SLD_MENU_ITM_ID,
                                TOT_UNITS))


data.pw1 = c.data %>% mutate(WK_END_DT = as.Date(WK_END_DT) +7)
data.pw1=data.pw1 %>% rename_with( ~ paste0("pw1.", .x))
# str(data.pw1)

wj.data = left_join(curr.data, data.pw1, 
                    by = c("WK_END_DT"="pw1.WK_END_DT",
                           "Store Num"="pw1.Store Num",
                           "SLD_MENU_ITM_ID" = "pw1.SLD_MENU_ITM_ID"))
# str(wj.data)
# tail(wj.data)


data.pw2 = c.data %>% mutate(WK_END_DT = as.Date(WK_END_DT) +2*7)
data.pw2=data.pw2 %>% rename_with( ~ paste0("pw2.", .x))

wj.data = left_join(wj.data, data.pw2, 
                    by = c("WK_END_DT"="pw2.WK_END_DT",
                           "Store Num"="pw2.Store Num",
                           "SLD_MENU_ITM_ID" = "pw2.SLD_MENU_ITM_ID"))
# str(wj.data)
# wj.data$pw2.totalguestcount

data.pw3 = c.data %>% mutate(WK_END_DT = as.Date(WK_END_DT) +3*7)
data.pw3=data.pw3 %>% rename_with( ~ paste0("pw3.", .x))
wj.data = left_join(wj.data, data.pw3, 
                    by = c("WK_END_DT"="pw3.WK_END_DT",
                           "Store Num"="pw3.Store Num",
                           "SLD_MENU_ITM_ID" = "pw3.SLD_MENU_ITM_ID"))
# str(wj.data)

data.pw4 = c.data %>% mutate(WK_END_DT = as.Date(WK_END_DT) +4*7)
data.pw4=data.pw4 %>% rename_with( ~ paste0("pw4.", .x))
wj.data = left_join(wj.data, data.pw4, 
                    by = c("WK_END_DT"="pw4.WK_END_DT",
                           "Store Num"="pw4.Store Num",
                           "SLD_MENU_ITM_ID" = "pw4.SLD_MENU_ITM_ID"))
# str(wj.data)
# wj.data = na.omit(wj.data)

# Complete Dataset
full.data = left_join(wj.data, store_demo_info,
                      by = "Store Num")
full.data = full.data %>% select(-c(WK_END_DT,Zipcode))
# str(full.data)




# saveRDS(full.data, file = "full_data.rds")
# library(tidyverse)
# rm(list=ls())
# setwd("Z:/CurrQuarter/ML Engineer")
# full.data = readRDS("full_data.rds")
colnames(full.data)[1] = "Store.Num"
data.without.na = na.omit(full.data)
# nrow(data.without.na)


#linear model assumption checking
# par(mfrow=c(1,2))
# hist(data.without.na$TOT_UNITS, freq = F)
# hist(log(data.without.na$TOT_UNITS), freq = F)
lm.data = data.without.na
lm.data$TOT_UNITS = log(lm.data$TOT_UNITS)

X <- model.matrix(TOT_UNITS ~ ., data = lm.data)[,-1]
Y <- lm.data$TOT_UNITS

set.seed(2022)
train <- sample(1:nrow(X), nrow(X)*7/10)
test <- (-train)

train.data = lm.data[train,]
test.data = lm.data[test,] 

library(lmvar)
full.model = lm(TOT_UNITS ~ ., data = train.data,y = TRUE,x = TRUE)

# cv.lm(full.model, k = 5)
#0.1102372 full model without interaction
summary(full.model)
# Adjusted R-squared:  0.961
pval = summary(full.model)$coefficients[,4] 
lmfm.var = pval[pval < 0.05] %>% sort %>% names
lmfm.var = lmfm.var[!(grepl("SLD_MENU_ITM_ID", lmfm.var) | 
             grepl("Store.Num", lmfm.var)|
             grepl("(Intercept)", lmfm.var))] %>% sort

# plot(fitted(full.model), resid(full.model))
# qqnorm(resid(full.model))
yhat.test = predict(full.model, newdata = test.data)
# cbind(yhat.test, test.data$TOT_UNITS)
MSE.full.model <- mean((yhat.test - test.data$TOT_UNITS)^2) #0.1024728

# Lasso Regression
library(glmnet)
grid <- 10^seq(10, -4, length = 100)
library(glmnet)
lasso.model <- glmnet(X[train,], Y[train], alpha = 1, lambda = grid)
cv.out <- cv.glmnet(X[train,], Y[train], alpha = 1)
# plot(cv.out)
# format(grid, scientific = F, digits = 8)
lam <- cv.out$lambda.min
lam
# 0.11 cv
coe=format(sort(abs(coef(lasso.model)[, 94])), scientific = F, digits = 8)
lasso.var = coe[as.numeric(coe) > 0.00] %>% names %>% sort
lasso.var = lasso.var[!(grepl("SLD_MENU_ITM_ID", lasso.var) | 
                        grepl("Store.Num", lasso.var)|
                        grepl("(Intercept)", lasso.var))] %>% sort
predictions <- predict(lasso.model, s = grid[94], newx = X[test,])
res = predictions - Y[test]
# plot(predictions,res)
test.error.lasso <- mean((predictions - Y[test])^2);test.error.lasso #0.1027213
weight.lasso = 1/test.error.lasso;weight.lasso



# Improved model based on p-value of full model and lasso
imp.var = c(lasso.var, lmfm.var) %>% unique
xs = paste(imp.var,collapse="+")
form1 <- as.formula(sprintf("TOT_UNITS ~ `SLD_MENU_ITM_ID` +`Store.Num`+ (%s)",xs))
lm.model.1 = lm(form1, data = train.data,y = TRUE,x = TRUE)
# cv.lm(lm.model.1, k = 5) 
# 0.132701 for model with 2 way interaction
# 0.110872 for only main
#Adjusted R-squared:  0.9707
#Adjusted R-squared: 0.9608 for only main
# summary(lm.model.1)
# plot( fitted(lm.model.1), resid(lm.model.1))
yhat.test.lm1 = predict(lm.model.1, newdata = test.data)
MSE.lm1 <- mean((yhat.test.lm1 - test.data$TOT_UNITS)^2) 
weight.lm = 1/MSE.lm1
#0.1096733 for model with 2 way interaction
#0.1027396 for only main
pval = summary(lm.model.1)$coefficients[,4] 
lmfm.var = pval[pval < 0.05] %>% sort %>% names
lmfm.var = lmfm.var[!(grepl("SLD_MENU_ITM_ID", lmfm.var) | 
                        grepl("Store.Num", lmfm.var)|
                        grepl("(Intercept)", lmfm.var))] %>% sort



xs = paste(lmfm.var,collapse="+")
form1 <- as.formula(sprintf("TOT_UNITS ~ `SLD_MENU_ITM_ID` +`Store.Num`+ (%s)^2",xs))
lm.model.2 = lm(form1, data = train.data, y = TRUE,x = TRUE)
# cv.lm(lm.model.2, k = 5) 
# 0.1023584 
# summary(lm.model.2) # 0.9662
# par(mfrow=c(2,2))
# plot(lm.model.2)
# plot(resid(lm.model.2))
# plot( fitted(lm.model.2), resid(lm.model.2))
yhat.test.lm2 = predict(lm.model.2, newdata = test.data)
MSE.lm1 <- mean((yhat.test.lm2 - test.data$TOT_UNITS)^2) 
weight.lm = 1/MSE.lm1
# 0.09217973

# Random Forest Regression
# library(randomForest)
# library(ggplot2)
# 
# set.seed(2022)
# rf.fit <- randomForest(TOT_UNITS ~., data=train.data, ntree=1000,
#                        keep.forest=T, importance=F, type= "regression")
# 
# test.data = test.data %>% select(-c(TOT_UNITS))
# yhat.rf = predict(rf.fit, test.data, type="response",
#         norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
# test.error.rf = mean((yhat.rf - Y[test])^2);test.error.rf
# # # 0.07598639 500 tree
# # # 0.07489674  1000 tree
# # # 0.07499083 1500 tree
# # # 0.07493116 2500 tree
# # # 0.07502001 4000 tree
# weight.rf = 1/test.error.rf



# Ensemble method
# yhat.test.lm1 = predict(lm.model.2, newdata = test.data)
# rf.pred = predict(rf.fit, test.data, type="response",
#                   norm.votes=TRUE, predict.all=FALSE,
#                   proximity=FALSE, nodes=FALSE)
# ensemble.pred = (weight.lm * yhat.test.lm1 +
#                    weight.rf * rf.pred) / (weight.lm + weight.rf)
# test.error.en = mean((ensemble.pred - Y[test])^2);test.error.en #0.0741965


# d =data
# data = d
# 
# s = data[1413,]
# 
# data$WK_END_DT = as.Date(data$WK_END_DT)
# data$`Store Num` = as.factor(data$`Store Num`)
# data$SLD_MENU_ITM_ID = as.factor(data$SLD_MENU_ITM_ID)

# 
# data = data %>% filter(`Store Num` == s$`Store Num` &
#                   s$SLD_MENU_ITM_ID == SLD_MENU_ITM_ID &
#                   (WK_END_DT == (s$WK_END_DT) |
#                     WK_END_DT == (s$WK_END_DT+7) |
#                      WK_END_DT == (s$WK_END_DT + 2*7) |
#                      WK_END_DT == (s$WK_END_DT + 3*7)))
# # 
# write_xlsx(sam_test,"sam.xlsx")
# Prediction for User
# 





uifn.td = readline(prompt ="\nPlease enter the file name that \nstores last month's transaction \ndata for a specfic store and item: ") 

uifn.gd = readline(prompt ="\nPlease enter the file name that \nstores last month's guest data \nfor a specfic store and item: ") 


guest_count = read_excel(uifn.gd,sheet="Guest count data")

# correct guest data type
guest_count$`Store Num` = as.factor(guest_count$`Store Num`)
guest_count$WK_END_DT = as.Date(guest_count$WK_END_DT)
guest_count = guest_count %>% select(c(`Store Num`,totalguestcount,WK_END_DT))

data = read_excel(uifn.td)
#join transaction data and guest data

data$WK_END_DT = as.Date(data$WK_END_DT)
data$TOT_UNITS = log(data$TOT_UNITS)
data$`Store Num` = as.factor(data$`Store Num`)
data$SLD_MENU_ITM_ID = factor(data$SLD_MENU_ITM_ID, levels = unique_item)

data = left_join(data, guest_count, 
                 by = c("WK_END_DT","Store Num"))

# clean transaction data
c.data = data %>% select(-c(WK_END_THU_ID_NU,	
                            WK_END_THU_WK_NU,	
                            WK_END_THU_END_YR_NU,
                            SMI_DESC,
                            COMBO_FLG))
c.data$TOT_UNITS
exp(c.data$TOT_UNITS)
for(week in 1:4){
  c.data = c.data[order(c.data$WK_END_DT),]
  c.data$WK_END_DT = as.Date(c.data$WK_END_DT)
  
  # construct new week row
  new.data = bind_rows(c.data, c.data %>% summarise(across(where(is.numeric), mean, na.rm = TRUE)))
  new.data[nrow(new.data),]$`Store Num` = new.data[1,]$`Store Num`
  new.data[nrow(new.data),]$SLD_MENU_ITM_ID = new.data[1,]$SLD_MENU_ITM_ID
  new.data[nrow(new.data), ]$WK_END_DT = as.Date(new.data[nrow(new.data)-1, ]$WK_END_DT +7)
  new.data$TOT_UNITS
  
  # create pred data
  pred.week = new.data[nrow(new.data), ] %>% select(c(WK_END_DT,`Store Num`, SLD_MENU_ITM_ID, TOT_UNITS))
  pred.week
  
  gen_pre_week_data = function(pred.week, index){
    data.pw1 = c.data %>% mutate(WK_END_DT = as.Date(WK_END_DT) + index * 7)
    suf = paste0("pw",index,".")
    data.pw1=data.pw1 %>% rename_with( ~ paste0(suf, .x))
    str(data.pw1)
    
    newWCN = paste0(suf,"WK_END_DT")
    newSN = paste0(suf,"Store Num")
    newIID = paste0(suf,"SLD_MENU_ITM_ID")
    return(left_join(pred.week, data.pw1, by = c("WK_END_DT"=newWCN,
                                                 "Store Num"=newSN,
                                                 "SLD_MENU_ITM_ID" = newIID)))
  }
  
  for(i in 1:4){
    pred.week = gen_pre_week_data(pred.week, i)
  }
  # pred.week = as.matrix(pred.week)
  pred.week = left_join(pred.week, store_demo_info,
                        by = "Store Num")
  pred.week = pred.week %>% select(-c(WK_END_DT,Zipcode))
  pred.week = pred.week %>% rename(Store.Num=`Store Num`)
  # pred.week = model.matrix(TOT_UNITS ~ ., data = pred.week)[,-1]
  pred.week = pred.week %>% select(-c(TOT_UNITS))
  lm.pred =  predict(lm.model.2, pred.week, type="response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)
  # rf.pred = predict(rf.fit, pred.week, type="response",
  #                   norm.votes=TRUE, predict.all=FALSE,
  #                   proximity=FALSE, nodes=FALSE)
  new.data[nrow(new.data), ]$TOT_UNITS =    lm.pred

  c.data = new.data[-c(1), ]
}
c.data
c.data$TOT_UNITS = exp(c.data$TOT_UNITS)
res = c.data %>% select(WK_END_DT, `Store Num`, SLD_MENU_ITM_ID, TOT_UNITS);res

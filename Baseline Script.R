## Github Seminar
rm(list=ls())

library(bsts)
library(hms)
library(stringr)
library(geometry)
library(base)
library(writexl)
library(matrixStats)

# 
#     Script
#
#

memory.limit(size = 8*10^9)                                 #maximal memory 
wd <- paste(getwd(),"/main/",sep="")
path_data <- paste(wd,"visits/",sep="")                     #path for the data
path_functions <- paste(wd,"",sep="")                  #path for the functions
path_lift <- paste(wd,"spotlifts/BSTS_4min/",sep="")        #path for the lift
path_MSE <- paste(wd,"MSE/",sep="")                         #path for the MSE

source(paste(path_functions,"Functions.R", sep = ""))       #Load in the functions

#
#   The optimal BSTS spot lifts for all the four data sets 
#

nl_web_case1_4min <- read.csv(paste(path_data,file="nl_web_case1_4min.csv", sep = ""), header = T)
lift_nl_web_case1_4min <- BayesianBaseline(nl_web_case1_4min)
be_web_case1_4min <- read.csv(paste(path_data,file="be_web_case1_4min.csv", sep = ""), header = T)
lift_be_web_case1_4min <- BayesianBaseline(be_web_case1_4min)
be_app_case1_4min <- read.csv(paste(path_data,file="be_app_case1_4min.csv", sep = ""), header = T)
lift_be_app_case1_4min <- BayesianBaseline(be_app_case1_4min)
nl_app_case1_4min <- read.csv(paste(path_data,file="nl_app_case1_4min.csv", sep = ""), header = T)
lift_nl_app_case1_4min <- BayesianBaseline(nl_app_case1_4min)

#
# Calculate the MSE's for the optimal datasets and time frame
# These lead to the MSE's for table 4 in the report
#


MSE_baseline_opt <- integer(4)
MSE_baseline_opt[1] <- BayesianMSE(nl_web_case1_4min)
MSE_baseline_opt[2] <- BayesianMSE(be_web_case1_4min)
MSE_baseline_opt[3] <- BayesianMSE(be_app_case1_4min)
MSE_baseline_opt[4] <- BayesianMSE(nl_app_case1_4min)
write.csv2(MSE_baseline_opt,paste(path_MSE,"MSE_optimal_baselines.csv",sep=""))

#
# AR Yule-walker method for the optimal data sets
# These lead to the MSE's for table 4 in the report
#


MSE_AR <- integer(4)
MSE_AR[1] <- AR_MError(be_app_case1_4min$visits_index)  
MSE_AR[2] <- AR_MError(nl_app_case1_4min$visits_index)
MSE_AR[3] <- AR_MError(be_web_case1_4min$visits_index)  
MSE_AR[4] <- AR_MError(nl_web_case1_4min$visits_index)
write.csv2(MSE_AR,paste(path_MSE,"MSE_AR.csv",sep=""))




#
#     COMPARING BAYESLINES AGAINST EACH OTHER 
#     These lead to the MSE's in table 3 of the report.
#

day <- 60*24
week <- day*7
data_nl_web <- read.csv(paste(path_data,file="nl_web_case1_4min.csv", sep = ""), header = T)
data_nl_web_bayes <- data_nl_web
data_nl_web_bayes$visits_index[data_nl_web_bayes$ads==1] <- NaN               #Set visits for ads to NA
y_nl_web <- as.numeric(data_nl_web_bayes$visits_index)             #quicker
obs <- length(y_nl_web)
numberOfDays <- obs/day
Regressors_nl_web <- XVarsData(data_nl_web_bayes)                  #regressors over all data
Regressors_nl_web <- Regressors_nl_web[rep(rownames(Regressors_nl_web),numberOfDays),] #Repeat for the number of days
rownames(Regressors_nl_web) <- 1:NROW(Regressors_nl_web)
Regressors_model_nl_web <- as.matrix(Regressors_nl_web[,3])        #Only use the mean (in row 3)
Dummies_model_nl_web <- MakeDummies(obs)                           #daily dummies

ss_nl_web1 <- AddAutoAr(list(),y_nl_web,lags=1)      # AR1 in state space
ss_nl_web2 <- AddAutoAr(list(),y_nl_web,lags=2)      # AR2 in state space
ss_nl_web3 <- AddAutoAr(list(),y_nl_web,lags=3)      # AR3 in state space
ss_nl_web4 <- AddSeasonal(ss_nl_web1,y_nl_web,nseasons=7,season.duration=day) #AR1 in state space and 
ss_nl_web5 <- ss_nl_web1       #AR1 in state space
ss_nl_web6 <- ss_nl_web4       #AR2 in state space
ss_nl_web7 <- ss_nl_web1       #AR1 in state space
ss_nl_web8 <- AddSeasonal(ss_nl_web1,y_nl_web,nseasons=7,season.duration=day)     #AR1 in state space and seasonal dummy
ss_nl_web9 <- ss_nl_web1       #AR1 in state space
ss_nl_web10 <- AddSeasonal(ss_nl_web2,y_nl_web,nseasons=7,season.duration=day)    #AR2 in state space and seasonal dummy
ss_nl_web11 <- AddSeasonal(ss_nl_web3,y_nl_web,nseasons=7,season.duration=day)    #AR3 in state space and seasonal dummy

Regressors_model_nl_web1 <- cbind(Regressors_model_nl_web,Dummies_model_nl_web)   #Mean and daily dummy as regressors
Regressors_model_nl_web2 <- Regressors_model_nl_web1                              #Mean and daily dummy as regressors
Regressors_model_nl_web3 <- Regressors_model_nl_web1                              #Mean and daily dummy as regressors
Regressors_model_nl_web4 <- as.matrix(Regressors_nl_web[,3])                      #Mean
Regressors_model_nl_web5 <- as.matrix(Regressors_nl_web[,2:5])                    #All Regressors 
Regressors_model_nl_web5 <- cbind(Regressors_model_nl_web5,Dummies_model_nl_web)  #daily dummy as regressors
Regressors_model_nl_web6 <- as.matrix(Regressors_nl_web[,2:5])                    #All Regressors
Regressors_model_nl_web7 <- XVarsDataMonthly(data_nl_web_bayes)                   #Regressors based on 1 month
Regressors_model_nl_web7 <- cbind(Regressors_model_nl_web7,Dummies_model_nl_web)  #daily dummy as regressors
Regressors_model_nl_web8 <- as.matrix(XVarsDataMonthly(data_nl_web))              #Regressors based on 1 month without daily dummy
Regressors_model_nl_web9 <- XVarsDataWeek(data_nl_web_bayes)                      #Regressors based on daily data, no dummy needed
Regressors_model_nl_web10 <- Regressors_model_nl_web4                             #Mean
Regressors_model_nl_web11 <- Regressors_model_nl_web4                             #Mean



model_nl_web1 <- bsts(y_nl_web~Regressors_model_nl_web1,state.specification=ss_nl_web1,niter=2000,ping=100, 
                     model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                     seed=1234)
Errors1 <- model_nl_web1$one.step.prediction.errors   #Find errors for MSE
rm(model_nl_web1) #remove big file

model_nl_web2 <- bsts(y_nl_web~Regressors_model_nl_web2,state.specification=ss_nl_web2,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                      seed=1234)
Errors2 <- model_nl_web2$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web2) #remove big file

model_nl_web3 <- bsts(y_nl_web~Regressors_model_nl_web3,state.specification=ss_nl_web3,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                      seed=1234)
Errors3 <- model_nl_web3$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web3) #remove big file


model_nl_web4 <- bsts(y_nl_web~Regressors_model_nl_web4,state.specification=ss_nl_web4,niter=2000,ping=100, 
                     model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                     seed=1234)
Errors4 <- model_nl_web4$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web4) #remove big file


model_nl_web5 <- bsts(y_nl_web~Regressors_model_nl_web5,state.specification=ss_nl_web5,niter=2000,ping=100, 
     model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE), seed=1234)
Errors5 <- model_nl_web5$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web5) #remove big file


model_nl_web6 <- bsts(y_nl_web~Regressors_model_nl_web6,state.specification=ss_nl_web6,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                      seed=1234)
Errors6 <- model_nl_web6$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web6) #remove big file


model_nl_web7 <- bsts(y_nl_web~Regressors_model_nl_web7,state.specification=ss_nl_web7,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                      seed=1234)
Errors7 <- model_nl_web7$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web7) #remove big file


model_nl_web8 <- bsts(y_nl_web~Regressors_model_nl_web8,state.specification=ss_nl_web8,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE)
                      ,seed=1234) 
Errors8 <- model_nl_web8$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web8) #remove big file


model_nl_web9 <- bsts(y_nl_web~Regressors_model_nl_web9,state.specification=ss_nl_web9,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE),
                      seed=1234)
Errors9 <- model_nl_web9$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web9) #remove big file

  
model_nl_web10 <- bsts(y_nl_web~Regressors_model_nl_web10,state.specification=ss_nl_web10,niter=2000,ping=100, 
                      model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE)
                      ,seed=1234)
Errors10 <- model_nl_web10$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web10) #remove big file


model_nl_web11 <- bsts(y_nl_web~Regressors_model_nl_web11,state.specification=ss_nl_web11,niter=2000,ping=100, 
                       model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE)
                       ,seed=1234)
Errors11 <- model_nl_web11$one.step.prediction.errors #Find errors for MSE
rm(model_nl_web11) #remove big file



MSE_bayes <- integer(11)
MSE_bayes[1] <- MSE(Errors1,data_nl_web)
MSE_bayes[2] <- MSE(Errors2,data_nl_web)
MSE_bayes[3] <- MSE(Errors3,data_nl_web)
MSE_bayes[4] <- MSE(Errors4,data_nl_web)
MSE_bayes[5] <- MSE(Errors5,data_nl_web)
MSE_bayes[6] <- MSE(Errors6,data_nl_web)
MSE_bayes[7] <- MSE(Errors7,data_nl_web)
MSE_bayes[8] <- MSE(Errors8,data_nl_web)
MSE_bayes[9] <- MSE(Errors9,data_nl_web)
MSE_bayes[10] <- MSE(Errors10,data_nl_web)
MSE_bayes[11] <- MSE(Errors11,data_nl_web)
write.csv2(MSE_bayes,paste(path_MSE,"MSE_bayes_specifications.csv",sep="")) #Write the MSE's for the baseline to csv file


#
#   Finding the best BSTS for 4 minute data 
#   Turns out the best BSTS are for case 1
#



# NL Web
write.csv2(lift_nl_web_case1_4min,file=paste(path_lift,"lift_nl_web_case1_4min.csv",sep=""))
nl_web_case2_4min <- read.csv(paste(path_data,file="nl_web_case2_4min.csv", sep = ""), header = T)
lift_nl_web_case2_4min <- BayesianBaseline(nl_web_case2_4min)
write.csv2(lift_nl_web_case2_4min,file=paste(path_lift,"lift_nl_web_case2_4min.csv",sep=""))
nl_web_case3_4min <- read.csv(paste(path_data,file="nl_web_case3_4min.csv", sep = ""), header = T)
lift_nl_web_case3_4min <- BayesianBaseline(nl_web_case3_4min)
write.csv2(lift_nl_web_case3_4min,file=paste(path_lift,"lift_nl_web_case3_4min.csv",sep=""))
nl_web_case4_4min <- read.csv(paste(path_data,file="nl_web_case4_4min.csv", sep = ""), header = T)
lift_nl_web_case4_4min <- BayesianBaseline(nl_web_case4_4min)
write.csv2(lift_nl_web_case4_4min,file=paste(path_lift,"lift_nl_web_case4_4min.csv",sep=""))

# BE web
write.csv2(lift_be_web_case1_4min,file=paste(path_lift,"lift_be_web_case1_4min.csv",sep=""))
be_web_case2_4min <- read.csv(paste(path_data,file="be_web_case2_4min.csv", sep = ""), header = T)
lift_be_web_case2_4min <- BayesianBaseline(be_web_case2_4min)
write.csv2(lift_be_web_case2_4min,file=paste(path_lift,"lift_be_web_case2_4min.csv",sep=""))
be_web_case3_4min <- read.csv(paste(path_data,file="be_web_case3_4min.csv", sep = ""), header = T)
lift_be_web_case3_4min <- BayesianBaseline(be_web_case3_4min)
write.csv2(lift_be_web_case3_4min,file=paste(path_lift,"lift_be_web_case3_4min.csv",sep=""))
be_web_case4_4min <- read.csv(paste(path_data,file="be_web_case4_4min.csv", sep = ""), header = T)
lift_be_web_case4_4min <- BayesianBaseline(be_web_case4_4min)
write.csv2(lift_be_web_case4_4min,file=paste(path_lift,"lift_be_web_case4_4min.csv",sep=""))


# BE app
write.csv2(lift_be_app_case1_4min,file=paste(path_lift,"lift_be_app_case1_4min.csv",sep=""))
be_app_case2_4min <- read.csv(paste(path_data,file="be_app_case2_4min.csv", sep = ""), header = T)
lift_be_app_case2_4min <- BayesianBaseline(be_app_case2_4min)
write.csv2(lift_be_app_case2_4min,file=paste(path_lift,"lift_be_app_case2_4min.csv",sep=""))
be_app_case3_4min <- read.csv(paste(path_data,file="be_app_case3_4min.csv", sep = ""), header = T)
lift_be_app_case3_4min <- BayesianBaseline(be_app_case3_4min)
write.csv2(lift_be_app_case3_4min,file=paste(path_lift,"lift_be_app_case3_4min.csv",sep=""))
be_app_case4_4min <- read.csv(paste(path_data,file="be_app_case4_4min.csv", sep = ""), header = T)
lift_be_app_case4_4min <- BayesianBaseline(be_app_case4_4min)
write.csv2(lift_be_app_case4_4min,file=paste(path_lift,"lift_be_app_case4_4min.csv",sep=""))


#NL app
write.csv2(lift_nl_app_case1_4min,file=paste(path_lift,"lift_nl_app_case1_4min.csv",sep=""))
nl_app_case2_4min <- read.csv(paste(path_data,file="nl_app_case2_4min.csv", sep = ""), header = T)
lift_nl_app_case2_4min <- BayesianBaseline(nl_app_case2_4min)
write.csv2(lift_nl_app_case2_4min,file=paste(path_lift,"lift_nl_app_case2_4min.csv",sep=""))
nl_app_case3_4min <- read.csv(paste(path_data,file="nl_app_case3_4min.csv", sep = ""), header = T)
lift_nl_app_case3_4min <- BayesianBaseline(nl_app_case3_4min)
write.csv2(lift_nl_app_case3_4min,file=paste(path_lift,"lift_nl_app_case3_4min.csv",sep=""))
nl_app_case4_4min <- read.csv(paste(path_data,file="nl_app_case4_4min.csv", sep = ""), header = T)
lift_nl_app_case4_4min <- BayesianBaseline(nl_app_case4_4min)
write.csv2(lift_nl_app_case4_4min,file=paste(path_lift,"lift_nl_app_case4_4min.csv",sep=""))
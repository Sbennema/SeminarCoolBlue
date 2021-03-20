#
#       Functions
#
#

#This function finds the mean, median and the 1st and 3rd quartile of the data for every minute, 
#where the data have dates in the form "YYYY-MM-DD HH:MM:SS" "D-M-YYYY H:M"
XVarsData <- function(data){
  
  minutes <- 60     #Minutes in hour
  hours <- 24       #Hours in day
  day <- minutes*hours 
  daysInData <- dim(data)[1]/day   #This calculates the number of days in the data
  
  #Prepare the vectors 
  time <- integer(day)
  medians <- integer(day)
  averages <- integer(day)
  quart1 <- integer(day)
  quart3 <- integer(day)
  inds <- integer(day)
  
  #Put the time data into the desired format
  data$date_time <- GetTime(data$date_time)
  
  #Need to obtain the x variables for every minute of the day
  for (h in 1:hours-1){ #-1, to range from 0 to 23
    for (m in 1:minutes-1){ #Logic analogue to hours
      hms_code <- 60*m+60*60*h
      time_cur <- as_hms(hms_code)
      time_cur <- toString(time_cur)  #Stringify the time in the format "HH:MM:00"
      time_cur_s <- substr(time_cur,1,5)
      data_cur <- data$visits_index[data$date_time==time_cur_s]
      data_cur <- sort(data_cur) #Take the visits_index that have a corresponding time representation and sort (deletes NAs)
      data_quants <- quantile(data_cur)
      
      #Add the statistics to the vectors
      ind <- 1+(h*60)+m
      time[ind] <- time_cur
      medians[ind] <- median(data_cur)
      averages[ind] <- mean(data_cur)
      quart1[ind] <- data_quants[2]
      quart3[ind] <- data_quants[4]
    }
  }
  X <- cbind(time,medians,averages,quart1,quart3)  #make dataframe of the variables combined
  X_df <- as.data.frame(X)
  
  X_df$medians <- as.numeric(X_df$medians)       #make the following variables numeric
  X_df$averages <- as.numeric(X_df$averages)     #opposed to characters
  X_df$quart1 <- as.numeric(X_df$quart1)
  X_df$quart3 <- as.numeric(X_df$quart3)
  
  return (X_df)
}


# This function finds the start and end of the advertisement blocks
Ad_locations <- function(data){
  
  data_dim = dim(data)[1]
  numbs <- 1:data_dim   
  data["numbs"] <- numbs
  
  ad_ind <- data$numbs[data$ads==1]
  ad_start <- c()
  ad_end <- c()
  
  i <- 1
  ad_finished <- 1
  while(i<=length(ad_ind)){
    i = i+1
    if(ad_finished==1){
      ad_start <- rbind(ad_start,ad_ind[i])
      if (i>length(ad_ind)){
        ad_end <- rbind(ad_end,ad_ind[length(ad_ind)])
        break
      }
      if (ad_ind[i]-ad_ind[i-1]>1){
        ad_end <- rbind(ad_end,ad_ind[i-1])
        ad_finished <- 1
      } else{
        ad_finished <- 0
      }
    } else {
      if (i>length(ad_ind)){
        ad_end <- rbind(ad_end,ad_ind[i-1])
        break
      }
      if (ad_ind[i]-ad_ind[i-1]>1){
        ad_end <- rbind(ad_end,ad_ind[i-1])
        ad_finished <- 1
      } else {
        ad_finished <- 0
      }
    }
  }
  combined <- as.data.frame(cbind(ad_start,ad_end))
  colnames(combined)<-c("start","end")
  return (combined)
}

#This function converts dates with time into HH:MM
GetTime <- function(time){
  time <- sub(".* ", "", time)     #Deletes the year/month/day
  time_s <- str_split(time[1],":") #Splits HH:MM:SS into separate parts
  if(length(time_s[[1]])>2){       #if we have 3 parts, then we need to take the HH:MM part
    times <- length(time)
    time_new <- integer(times)
    for (i in 1:times){
      time_new[i] = substr(time[i],1,5)
    }
  } else{                         #We only have 2 parts, so we already have the HH:MM format which we want
    time_new <- time
  }
  return (time_new)
}

# This function makes the daily dummies
MakeDummies <- function(obs){
  day <- 24*60
  days <- 7
  obs_days <- obs/day  #number of days
  Dummies <- matrix(0,nrow=obs,ncol=days) 
  
  for (t in 1:obs_days){
    for (d in 1:days){     #between 1 and 7
      day_cur <- t%%days   #between 0 and 6
      if (day_cur==0){
        day_cur <- 7      #Assigns to day 7
      }
      if (day_cur==d)
      Dummies[((t-1)*day+1):(t*day),d] <- 1   #24*60 cells become 1
    }
  }
  return (Dummies[,2:7]) #Assume that the first day is not used
}

#This functions calculates the BSTS predictions using the optimal structure
BSTS_pred <- function(Bsts_model,ad_locations,Regressors,burn_in){
  gamma <- Bsts_model$coefficients       #Regressor coefficients
  alpha <- Bsts_model$full.state         #State space values
  phi <- Bsts_model$AR1.coefficients     #AR coefficients of state space
  rm(Bsts_model)          #model can be large in size
  f_horizon <- ad_locations$end - ad_locations$start
  Regressors[,1] <- 1
  
  MCMCiter <- dim(alpha)[1]
  obs <- dim(alpha)[3]
  numberLifts <- dim(ad_locations)[1]
  
  Dummies <- MakeDummies(obs)
  predicts <- sum(f_horizon)+numberLifts      #predetermine the number of predictions
  preds <- matrix(0,nrow=predicts,ncol=MCMCiter-burn_in)      #Makes the prediction matrix smaller to save memory
  
  #Assign space for the looped variables
  predictions <- integer(predicts) 
  phi_iter <- 0
  gammas_iter <- integer(5)
  AR_comp <- 0
  X_comp <- 0
  
  for (i in 1:(MCMCiter-burn_in)){
    predictions <- integer(predicts)
    count <- 1                        #This counter keeps track of the number of predictions made
    phi_iter <- phi[(i+burn_in),]
    gammas_iter <- gamma[(i+burn_in),]
    alpha_iter <- alpha[(i+burn_in),,]
    preds_iter <- preds[,i]     #preallocate memory
    
    for (j in 1:numberLifts){
      start_f <- ad_locations$start[j]     #Determine which is the starting date
      for (k in 0:f_horizon[j]){
        t <- start_f + k        #finds the specific date
        if (t>obs){
          break
        } else{
          AR_comp <- phi_iter^(k+1)*alpha_iter[start_f-1]          #phi^k * y_{t-1}
          X_comp <- sum(gammas_iter*as.numeric(Regressors[t,]))    #Dot product of the regressors Xi'B
          preds_iter[count] <- AR_comp+X_comp    #prediction
          count <- count+1     #update the number of predictions made
        }
      }
    }
    preds[,i] <- preds_iter
  }
  return (preds)
}

#This function return the mean predictions of the bayesian predictions
mean_pred <- function(predictions){
  lifts <- colMeans(t(predictions))
  return (lifts)
}

#This function translates the lifts to the correct dates
predsToVector <- function(ad_locations,lifts){
  N <- length(ad_locations$start)
  f_horizon <- ad_locations$end- ad_locations$start  #determine the forecast horizon for each ad
  data <- rep(NA,ad_locations$end[N]) 
  count <- 1            #keeps track of the prediction
  for (i in 1:N){
    start <- ad_locations$start[i]
    for (k in 0:f_horizon[i]){
      data[start+k] <- lifts[count]  #Puts the prediction to the actual time
      count <- count+1
    }
  }
  return (data)
}

#This function calculates the spot lift
calc_lift <- function(data,lifts){
  data$visits_index[data$ads==0] <- NA
  N_data <- length(data$visits_index)  
  N_lift <- length(lifts)
  lift <- rep(NA,N_data)             #The lifts vector may be larger so make them equally sized
  lift[1:N_lift] <- lifts
  lift <- data$visits_index-lifts    #This is the actual spot lift for each point
  time <- data$date_time
  data_new <- as.data.frame(cbind(time,lift))
  return(data_new)
}


#This function determines the mean on 13 days before and 13 days after the observation
XVarsDataMonthly <- function(data){
  
  minutes <- 60     #Minutes in hour
  hours <- 24       #Hours in day
  day <- minutes*hours
  N <- dim(data)[1]
  daysInData <- N/day   #This calculates the number of days in the data
  averages <- integer(N) 
  
  for (i in 1:daysInData){
    for (m in 1:day){
      ind <- (i-1)*1440+m
      Dat_cur <- rep(NaN,27)   #The data vector which include the relevant observations
      for (t in -13:13){
        t_ind <- ind + t*day
        if (t_ind>0 && t_ind<=N){ 
          Dat_cur[t+14] <- data$visits_index[t_ind]  
        }
      }
      averages[ind] <- mean(Dat_cur,na.rm=TRUE)  #most likely several NA's occur
    }
  }
  return (as.matrix(averages))
}

#This function finds the weekly day-of-the-week mean regressors
XVarsDataWeek <- function(data){
  minutes <- 60     #Minutes in hour
  hours <- 24       #Hours in day
  day <- minutes*hours
  week <- 7 
  N <- week       #number of observations per week
  daysInData <- dim(data)[1]/day   #This calculates the number of days in the data
  obs <- daysInData*day            
  averages <- integer(N)
  
  #Find the regressors for every minute in the week
  for (i in 1:week){
    for (m in 1:day){
      ind <- (i-1)*day+m             #The index of the minute
      Dat_cur <- rep(NaN,daysInData) #Data vector for every minute to find regressors
      for (t in 1:daysInData-1){
        t_ind <- ind + t*day         
        if (t_ind>0 && t_ind<=obs){    #check if index for observation to be included in range
          Dat_cur[t+1] <- data$visits_index[t_ind]    #include the observation for the mean calculation
        }
      }
      averages[ind] <- mean(Dat_cur,na.rm=TRUE)
    }
  }
  repetition <- floor(daysInData/week) #Floor because most likely, the number of days not multiple of 7
  repeatedDays <- daysInData%%week
  avs <- integer(daysInData*day)       #Create the X matrix such that the number of rows corresponds 
  avs[1:(repetition*day*week)] <- rep(averages,repetition)  #repeat for 7*repetition days
  avs[(repetition*day*week+1):(daysInData*day)] <- averages[1:(repeatedDays*day)]   #for the last days that fall outside the last week
  return (avs)
}


#This function finds the Bayesian MSE 
MSE <- function(Errors,data){
  dims <- dim(Errors)
  Errors <- Errors[501:dims[1],][,data$ads==0]   #burn in of the first 500 values
  dims2 <- dim(Errors) #the number of observations
  MSError <- mean(diag(Errors %*% t(Errors)))/dims2[2] #diagonal contains the MSEs for each observation averaged
  return (MSError)
}

#This functions finds the Mean error for the AR method based on the Yule Walker equations
AR_MError = function(y){
  AR_pack =ar(y, na.action = na.pass, demean = TRUE, aic=TRUE)
  coef = AR_pack$ar
  lags = AR_pack$order
  
  #Preallocate memory for the vectors
  y_hat = integer(length(y))
  error = integer(length(y))
  
  for (t in (lags+1):length(y)){
    y_hat[t] =  sum(y[(t-lags):(t-1)]*rev(coef))  #Reverse the coefficients because phi_p * y_{t-p}
  }
  
  error = abs(y-y_hat)
  return (mean(error, na.rm = TRUE))
}


#Sets the visits where there is an add to NA
DataBayes <- function(data){
  data_bay <- data
  data_bay$visits_index[data$ads==1] <- NA
  return(data_bay)
}



# This function finds the spot lifts for a given dataset
BayesianBaseline <- function(data){
  data_bayes <- DataBayes(data) #Set the visits when there is an ad to missing
  obs <- dim(data)[1]
  ad_locations <- Ad_locations(data)    #Find where the ads are for prediction
  Regressors <- XVarsDataWeek(data_bayes)
  Regressors_model <- Regressors   #Optimal regressors
  y <- as.numeric(data_bayes$visits_index) #Quicker notation
  ss <- AddAutoAr(list(),y)   #AR1 in state space
  model <- bsts(y ~ Regressors_model,state.specification=ss,niter=2000,ping=100, 
                model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=FALSE,
                                            save.full.state=TRUE),seed=1234)
  Regressors_preds <- cbind(rep(1,obs),Regressors_model)   #intercept added manually
  preds <- BSTS_pred(model,ad_locations,Regressors_preds,500)  #finds predictions with burn in of 500 iterations
  baseline <- mean_pred(preds)                   #reduces predictions to vector
  preds_full <- predsToVector(ad_locations,baseline)  #puts the predictions at the right place
  lift <- calc_lift(data,preds_full)                  #calculates the lift for the predictions
  return (lift)
}

BayesianMSE <- function(data){
  data_bayes <- DataBayes(data) #Set the visits when there is an ad to missing
  Regressors <- XVarsDataWeek(data_bayes)
  Regressors_model <- Regressors   #Optimal regressors
  y <- as.numeric(data_bayes$visits_index) #Quicker notation
  ss <- AddAutoAr(list(),y)   #AR1 in state space
  model <- bsts(y ~ Regressors_model,state.specification=ss,niter=2000,ping=100, 
                model.options = BstsOptions(save.state.contributions=FALSE,save.prediction.errors=TRUE,
                                            save.full.state=FALSE),seed=1234)
  Errors <- model$one.step.prediction.errors
  MSe <- MSE(Errors,data)   #find the MSE for the model
  return (MSe)
}


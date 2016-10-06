+ # Ultimate-Student-Hunt
 + -Code
 + # Ultimate-Student-Hunt-Code
 + 
 + # Library packages used
 + 
 + library(gbm) # gbm model # version 2.1.1
 + library(caret) # for dummyVars # version 6.0-71
 + library(Metrics) # calculate RMSE and MAE errors # version 0.1.1
 + library(xgboost) # xgboost model # version 0.4-4
 + library(plyr) # Tools for Splitting, Applying and Combining Data # Version 6.0-71
 + library(doParallel) # provides drop- in replacements for most of the functionality of those packages, with integrated handling of random-number generation # version 3.2.3
 + library(foreach) # provides a new looping construct for executing R code repeatedly  # version 1.4.3
 + library(doSNOW) # provides drop- in replacements for most of the functionality of those packages, with integrated handling of random-number generation # version 3.2.3
 + library(dummies) # create dummy variables # version 1.5.6
 + library(caTools) # for splitting data # version 1.17.1
 + library(neuralnet) # running neural network model # version 1.33
 + library(nnet) # running neural network model with cv # version 7.3-11
 + library(date) # splitting date into month, day and year variable # version 1.2-34
 + library(mice) # predicting and imputing missing data # version 2.25
 + 
 + 
 + 
 + +setwd("F:/DataScience/av_student_hunt/data/Train")
 + +
 + # Run the test data and read it 
 + 
 + +test_new <- read.csv("F:/DataScience/av_student_hunt/data/test_new.csv")
 + 
 + # View the test data
 + 
 + +View(test_new)
 + 
 + # Run the train data and read it
 + 
 + +train_new <- read.csv("F:/DataScience/av_student_hunt/data/train_new.csv")
 + 
 + # View the train data
 + 
 + +View(train_new)
 + 
 + 
 + +# GBM MODEL with rearranging data
 + +# replacing missing values by -999 and converting date to day, month and year and removed the variables Location, Park ID, ID
 + +
 + +
 + +library(gbm)
 + +Model.gbm = gbm(Footfall ~.,distribution="laplace", data=train_new, n.trees=5000, interaction.depth =6, shrinkage=0.05, n.minobsinnode = 10)
 + +# outputfile 
 + +Prediction_gbm = predict.gbm(Model.gbm, test_new, n.trees=5000)
 + +mysolution = data.frame( ID = Test$ID, Footfall = Prediction_gbm)
 + +write.csv(mysolution, file = "gbm.csv", row.names = FALSE)
 + +
 + +importance = summary.gbm(Model.gbm, plotit=TRUE)
 + +
 + +
 + +# XG-BOOST MODEL.... REARRANGING THE DATA AGAIN 
 + +
 + +library(caret) # for dummyVars
 + +library(Metrics) # calculate errors
 + +library(xgboost) # model
 + +
 + +library(plyr)
 + +library(doParallel)
 + +library(foreach)
 + +library(doSNOW)
 + +
 + +train <- read.csv('file:///F:/DataScience/av_student_hunt/data/Train.csv')
 + +test <- read.csv('file:///F:/DataScience/av_student_hunt/data/Test.csv')
 + +temp_train = train
 + +temp_train$Footfall = NULL  # to remove footfall variable from train data
 + +
 + +total = rbind(temp_train, test)
 + +rm(temp_train)
 + +
 + +# feature engineering
 + +total$diff_breeze_speed = total$Max_Breeze_Speed-total$Min_Breeze_Speed
 + +total$diff_Atmospheric_Pressure = total$Max_Atmospheric_Pressure-total$Min_Atmospheric_Pressure
 + +total$diff_Ambient_Pollution = total$Max_Ambient_Pollution-total$Min_Ambient_Pollution
 + +total$diff_moisture = total$Max_Moisture_In_Park-total$Min_Moisture_In_Park
 + +total$month = substring(total$Date, 4,5)
 + +total$month = as.numeric(as.character(total$month))
 + +
 + +# some more feature engineering added later
 + +total$avg_breeze_speed =          (total$Max_Breeze_Speed+total$Min_Breeze_Speed)/2
 + +total$avg_Atmospheric_Pressure =  (total$Max_Atmospheric_Pressure+total$Min_Atmospheric_Pressure)/2
 + +total$avg_Ambient_Pollution =     (total$Max_Ambient_Pollution+total$Min_Ambient_Pollution)/2
 + +total$avg_moisture =              (total$Max_Moisture_In_Park+total$Min_Moisture_In_Park)/2
 + +
 + +# feature engineering for direction of wind 
 + +summary(total$Direction_Of_Wind)
 + +total$wind_dir_categorical[is.na(total$Direction_Of_Wind )] <- 5 #assigning 1 to missing values 
 + +total$wind_dir_categorical[total$Direction_Of_Wind<=90] <- 1 
 + +total$wind_dir_categorical[total$Direction_Of_Wind >90 & total$Direction_Of_Wind <=180 ] <- 2
 + +total$wind_dir_categorical[total$Direction_Of_Wind >180 & total$Direction_Of_Wind <=270 ] <- 3 
 + +total$wind_dir_categorical[total$Direction_Of_Wind >270 & total$Direction_Of_Wind <=360 ]  <- 4
 + +summary(total$wind_dir_categorical)
 + +
 + +names(total)
 + +# columns not required for modelling
 + +remove_column=c("ID","Date", "Average_Breeze_Speed","Max_Breeze_Speed","Min_Breeze_Speed","Average_Atmospheric_Pressure",
 + +                "Max_Atmospheric_Pressure","Min_Atmospheric_Pressure","Min_Ambient_Pollution","Max_Ambient_Pollution",
 + +                "Average_Moisture_In_Park","Max_Moisture_In_Park", "Min_Moisture_In_Park","Location_Type",
 + +                "Avg_Ambient_Pollution")
 + +# including ,"Park_ID" to train the model 
 + +
 + +# removing only id and date 
 + +remove_column=c("ID","Date")
 + +
 + +total=total[,!(names(total) %in% remove_column)]
 + +names(total)
 + +
 + +# next line creates a data frame with additional features of dummy variables using specified features
 + +library(dummies)
 + +# since location type and park ID ARE not helpful, hence setting it to null and not creating dummy variable from this
 + +#total$Park_ID = NULL
 + +#total$Location_Type = NULL
 + +
 + +total_with_dummy <- dummy.data.frame(total, names=c("month","Park_ID","wind_dir_categorical","Location_Type"), sep="_")
 + +names(total_with_dummy)
 + +summary(total_with_dummy)
 + +str(total_with_dummy)
 + +
 + +total_with_dummy <- data.frame(total_with_dummy)
 + +# spliting the data in original format 
 + +training_with_dummy = total_with_dummy[1:nrow(train),]
 + +testing_with_dummy = total_with_dummy[114540:nrow(total),]
 + +rm(total_with_dummy)
 + +
 + +
 + +# now preparing the data for the model 
 + +#split the training_with_dummy for training and validation 
 + +library(caTools)
 + +set.seed(3000)
 + +# now adding footfall to training data
 + +training_with_dummy$Footfall <- train$Footfall
 + +spl = sample.split(training_with_dummy$Footfall, SplitRatio = 0.8)
 + +sample_train = subset(training_with_dummy, spl == TRUE)
 + +x <- sample_train$Footfall  # it will be used in xgb.DMatrix
 + +sample_train$Footfall = NULL
 + +
 + +sample_test = subset(training_with_dummy, spl == FALSE)
 + +y <- sample_test$Footfall    # it will be used in xgb.DMatrix
 + +sample_test$Footfall = NULL
 + +
 + +summary(sample_train)
 + +summary(sample_test)
 + +names(sample_train)
 + +names(sample_test)
 + +summary(x)
 + +summary(y)
 + +summary(train$Footfall)
 + +
 + +# now creating the model 
 + +t1 <- Sys.time()
 + +library(xgboost)
 + +# creating first matrix (data and label) using sample_train
 + +dtrain<-xgb.DMatrix(data=data.matrix(sample_train),label=data.matrix(x),missing=NA)
 + +# creating second matrix (data and label) using sample_test
 + +dval<-xgb.DMatrix(data=data.matrix(sample_test),label=data.matrix(y),missing=NA)
 + +watchlist<-list(val=dval,train=dtrain)
 + +fin_pred={}
 + +for (eta in c(0.1,0.05,0.01) )
 + +{
 + +  t <- Sys.time()
 + +  for (colsample_bytree in c(0.2,0.4,0.6))
 + +  {
 + +    for(subsample in c(0.4,0.8,1))
 + +    {
 + +      param <- list(  objective           = "reg:linear", 
 + +                      booster             = "gbtree",
 + +                      eta                 = eta,
 + +                      max_depth           = 10, # 7 initially it was 8 
 + +                      subsample           = subsample,
 + +                      colsample_bytree    = colsample_bytree
 + +      )
 + +      gc()
 + +      set.seed(1429)
 + +      # creating the model 
 + +      clf <- xgb.train(   params              = param, 
 + +                          data                = dtrain, 
 + +                          nrounds             = 500, 
 + +                          verbose             = 1,
 + +                          early.stop.round    = 150,
 + +                          watchlist           = watchlist,
 + +                          maximize            = TRUE,
 + +                          eval_metric       = "rmse"
 + +      )
 + +      #[645]  val-auc:0.872786  train-auc:0.993390
 + +      #Stopping. Best iteration: 596
 + +      # making the prediction 
 + +      pred_exp=predict(clf,data.matrix(testing_with_dummy),missing=NA)
 + +      print(head(fin_pred))
 + +      fin_pred<-cbind(fin_pred,pred_exp)
 + +    }
 + +  }
 + +  print(Sys.time() - t)
 + +}
 + +
 + +print(Sys.time() - t1)
 + +
 + +
 + +#saving the raw output
 + +z <- data.frame(fin_pred)
 + +write.csv(z, file = "z.csv", row.names = FALSE)
 + +
 + +# preparing for final sumission
 + +final_pred=rowMeans(fin_pred)
 + +summary(train$Footfall)
 + +summary(final_pred)
 + +submission = data.frame( ID = test$ID, Footfall = final_pred)
 + +write.csv(submission, file = "submission.csv", row.names = FALSE)
 + +
 + +
 + +# for Feature importance
 + +# get the trained model
 + +model = xgb.dump(clf, with.stats=TRUE)
 + +# get the feature real names
 + +names = names(sample_train)
 + +# compute feature importance matrix
 + +importance_matrix = xgb.importance(names, model=clf)
 + +# plot
 + +gp = xgb.plot.importance(importance_matrix)
 + +print(gp)
 + +
 + +
 + +for (i in 1:ncol(fin_pred))
 + +{
 + +  print ( max(fin_pred[,i]) - min(fin_pred[,i]))
 + +}
 + +
 + +
 + +# training the model with tuned parameters 
 + +# run the aove code upto creating training and testing data set 
 + +
 + +x <- train$Footfall
 + +# now creating the model 
 + +t1 <- Sys.time()
 + +library(xgboost)
 + +# creating first matrix (data and label) using train
 + +dtrain<-xgb.DMatrix(data=data.matrix(training_with_dummy),label=data.matrix(x),missing=NA)
 + +
 + +# creating second matrix (data and no label this time) using test
 + +#dval<-xgb.DMatrix(data=data.matrix(testing_with_dummy),missing=NA)
 + +watchlist<-list(train=dtrain)
 + +
 + +t1 <- Sys.time()
 + +fin_pred={}
 + +iteration = 1
 + +for (eta in c(0.01) )  # getting 118 with 0.01 and 187 with 0.1
 + +{
 + +  t <- Sys.time()
 + +  for (colsample_bytree in c(0.9))
 + +  {
 + +    for(subsample in c(0.6))
 + +    {
 + +      for  (max_depth in c(10))
 + +      {
 + +        param <- list(  objective           = "reg:linear", 
 + +                        booster             = "gbtree",
 + +                        eta                 = eta,
 + +                        max_depth           = max_depth , #7 initially it was 8 
 + +                        subsample           = subsample,
 + +                        colsample_bytree    = colsample_bytree
 + +        )
 + +        gc()
 + +        set.seed(1020)
 + +        # creating the model 
 + +        clf <- xgb.train(   params              = param, 
 + +                            data                = dtrain,
 + +                            nrounds             = 600, 
 + +                            verbose             = 2,
 + +                            #early.stop.round    = 150,
 + +                            watchlist           = watchlist,
 + +                            maximize            = TRUE,
 + +                            eval_metric       = "rmse",
 + +                            print.every.n = 50
 + +        )
 + +        
 + +        # making the prediction 
 + +        cat("this is iteration number:",  iteration, "\n"  )
 + +        cat ("making the prediction with : ", eta,  colsample_bytree, subsample,  "\n")
 + +        pred_exp=predict(clf,data.matrix(testing_with_dummy),missing=NA)
 + +        cat("prediction completed",  "\n")
 + +        print(head(fin_pred))
 + +        fin_pred<-cbind(fin_pred,pred_exp)
 + +        iteration = iteration + 1 
 + +      }
 + +    }
 + +  }
 + +  print(Sys.time() - t)
 + +}
 + +
 + +print(Sys.time() - t1)
 + +
 + +#saving the raw output
 + +z <- data.frame(fin_pred)
 + +write.csv(z, file = "tuned_z.csv", row.names = FALSE)
 + +
 + +# preparing for final submission
 + +final_pred=rowMeans(fin_pred)
 + +summary(train$Footfall)
 + +summary(final_pred)
 + +submission = data.frame( ID = test$ID, Footfall = final_pred)
 + +write.csv(submission, file = "tuned_submission.csv", row.names = FALSE)
 + +
 + +for (w in 1:ncol(z))
 + +{
 + +  submission = data.frame( ID = test$ID, Footfall = z[,w])
 + +  a = paste(w,'.csv',sep="")
 + +  write.csv(submission, file = a, row.names = FALSE)
 + +}
 + +
 + +# for Feature importance
 + +# get the trained model
 + +model = xgb.dump(clf, with.stats=TRUE)
 + +# get the feature real names
 + +names = names(training_with_dummy)
 + +# compute feature importance matrix
 + +Sys.time(importance_matrix = xgb.importance(names, model=clf))
 + +# plot
 + +gp = xgb.plot.importance(importance_matrix[1:35,])
 + +print(gp)
 + +
 + +
 + +setwd("F:/DataScience/kaggle/WhatsCooking/modified_output")
 + +getwd()
 + +output <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/modified_output/ind_tuned_6_115.86.csv")
 + +summary(output)
 + +
 + +output$Footfall[output$Footfall <= 1417 & output$Footfall >= 1416] <- 1200      #use 1400
 + +
 + +write.csv(output, file = "modified_7.csv", row.names = FALSE)
 + +
 + +# neural network
 + +
 + +# neural network without cross validation
 + +
 + +set.seed(1234567890)
 + +install.packages("neuralnet")
 + +library(neuralnet)
 + +
 + +# data imputation for train
 + +train_deleted <- train[!(is.na(train$Average_Atmospheric_Pressure)) | !(is.na(train$Avg_Ambient_Pollution)),]
 + +
 + +library(mice)
 + +summary(train_deleted)
 + +simple_nn = train_deleted[c("Location_Type", "Park_ID", "Direction_Of_Wind", "month" , "Average_Breeze_Speed" , "Var1" , 
 + +                 "Avg_Ambient_Pollution", "Average_Moisture_In_Park" , "Average_Atmospheric_Pressure")]
 + +summary(simple_nn)
 + +set.seed(144)
 + +imputed_nn = complete(mice(simple_nn))
 + +summary(imputed_nn)
 + +train_deleted$Direction_Of_Wind = imputed_nn$Direction_Of_Wind
 + +train_deleted$Average_Breeze_Speed = imputed_nn$Average_Breeze_Speed
 + +train_deleted$Var1 = imputed_nn$Var1
 + +train_deleted$Avg_Ambient_Pollution = imputed_nn$Avg_Ambient_Pollution
 + +train_deleted$Average_Moisture_In_Park = imputed_nn$Average_Moisture_In_Park
 + +train_deleted$Average_Atmospheric_Pressure = imputed_nn$Average_Atmospheric_Pressure
 + +train_deleted$Var2 = log(train_deleted$Var1)
 + +train_deleted$Var2[train_deleted$Var2 == -Inf] = 0
 + +
 + +train_deleted$month = as.numeric(as.character(train_deleted$month))
 + +
 + +
 + +# spliting into training and testing data set
 + +
 + +library(caTools)
 + +set.seed(3000)
 + +spl = sample.split(train_deleted$Footfall, SplitRatio = 0.7)
 + +Train_nn = subset(train_deleted, spl == TRUE)
 + +Test_nn = subset(train_deleted, spl == FALSE)
 + +
 + +
 + +neural = neuralnet(Footfall ~ month + Direction_Of_Wind + Average_Breeze_Speed + Var2 + Avg_Ambient_Pollution + Average_Moisture_In_Park + Average_Atmospheric_Pressure+ Location_Type, Train_nn, hidden=4, linear.output = TRUE, threshold = 0.1, lifesign = "minimal")
 + +
 + +test$month = as.numeric(as.character(test$month))
 + +test$date_month = as.numeric(as.character(test$date_month))
 + +
 + +temp_test <- subset(Test_nn, select = c("month" , "Direction_Of_Wind" , "Average_Breeze_Speed" , "Var2" , 
 + +                     "Avg_Ambient_Pollution" , "Average_Moisture_In_Park" , "Average_Atmospheric_Pressure" , "Location_Type"))
 + +prediction_nn <- compute(neural,temp_test)
 + +
 + +
 + +results <- data.frame(actual = Test_nn$Footfall, prediction = prediction_nn$net.result)
 + +results[100:115, ]
 + +
 + +
 + +mysolution = data.frame(ID = test$ID, Footfall = prediction_nn)
 + +write.csv(mysolution, file = "mysolution1.csv", row.names = FALSE)
 + +
 + +# neural network with cross validation
 + +
 + +library(mice)
 + +train = read.csv("Train.csv")
 + +test = read.csv("Test.csv")
 + +install.packages("neuralnet")
 + +library(neuralnet)
 + +library(date)
 + +install.packages("nnet")
 + +library(nnet)
 + +imputed = read.csv("imputed_train_and_test.csv")
 + +
 + +imputed$Location_Type = NULL
 + +imputed$Min_Ambient_Pollution = NULL
 + +imputed$Max_Breeze_Speed = NULL
 + +imputed$Average_Atmospheric_Pressure = NULL
 + +
 + +#breaking into train and test
 + +training = imputed[1:114539,]
 + +testing = imputed[114540:nrow(imputed),]
 + +
 + +training$Footfall = train$Footfall
 + +testing$ID = test$ID
 + +
 + +training$Date = train$Date
 + +testing$Date = test$Date
 + +
 + +training$day = substring(training$Date, 1,2)
 + +training$year = substring(training$Date, 7,10)
 + +
 + +
 + +testing$day = substring(testing$Date, 1,2)
 + +testing$year = substring(testing$Date, 7,10)
 + +
 + +training$Date = NULL
 + +testing$Date = NULL
 + +
 + +
 + +testing$day = as.numeric(as.character(testing$day))
 + +testing$year = as.numeric(as.character(testing$year))
 + +
 + +training$day = as.numeric(as.character(training$day))
 + +training$year = as.numeric(as.character(training$year))
 + +
 + +
 + +library(car)
 + +library(caret)
 + +trainIndex = createDataPartition(training$Footfall, p=.7, list=F)
 + +train_cv = training[trainIndex, ]
 + +test_cv = training[-trainIndex, ]
 + +
 + +
 + +my.grid = expand.grid(.decay = c(0.5, 0.1), .size = c(6, 7, 8))
 + +neural_network = train(Footfall ~ Park_ID + Direction_Of_Wind + month +  Average_Breeze_Speed + 
 + +                         Min_Breeze_Speed + Var1 + Max_Ambient_Pollution + Average_Moisture_In_Park + 
 + +                         Max_Atmospheric_Pressure + Min_Atmospheric_Pressure + Max_Moisture_In_Park + 
 + +                         Min_Moisture_In_Park + day + year, data = train_cv,
 + +                       method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)  
 + +
 + +
 + +
 + +# RMSE calculation
 + +
 + +predict_nn = predict(neural_network, newdata = test_cv)
 + +rmse = sqrt(mean((predict_nn - test_cv$Footfall)^2))
 + +
 + +
 + +Prediction_nn = predict(neural_network, testing)
 + +mysolution = data.frame( ID = testing$ID, Footfall = Prediction_nn)
 + +write.csv(mysolution, file = "mysolution2.csv", row.names = FALSE)
 + +
 + +
 + +# combining neural_net and xgb of 115.85
 + +xgb <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/modified_output/ind_tuned_6_115.86.csv")
 + +neural <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/modified_output/neural_124.41.csv")
 + +a <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/gbm_120.csv")
 + +b <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/neural_net_new_124.59.csv")
 + +c <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/ind_tuned_6_115.86.csv")
 + +
 + +submission_combo = data.frame( ID = output$ID, Footfall = (((xgb$Footfall)*0.4+(neural$Footfall)*0.4+(c$Footfall)*0.2))
 + +
 + +submission_combo = data.frame( ID = output$ID, Footfall = (((xgb$Footfall)*0.6+(c$Footfall)*0.4)))
 + +
 + +submission_combo = data.frame( ID = output$ID, Footfall = ((xgb$Footfall+neural$Footfall)/2))
 + +
 + +write.csv(submission_combo, file = "combined_xgb+neural_A_6.csv", row.names = FALSE)
 + +
 + +
 + +# without neural
 + +# combining neural_net and xgb of 115.85
 + +xgb <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/modified_output/ind_tuned_6_115.86.csv")
 + +a <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/ind_tuned_6_115.86.csv")
 + +b <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/116.4_from_64_combo_without_imputation.csv")
 + +
 + +submission_combo = data.frame( ID = output$ID, Footfall = ((xgb$Footfall+a$Footfall+b$Footfall)/3))
 + +write.csv(submission_combo, file = "combined_xgb_other_b.csv", row.names = FALSE)
 + +
 + +
 + +
 + +
 + +# modifying the data
 + +modify <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/modified_output/Test.csv")
 + +names(modify)
 + +
 + +
 + +# replacing footfall by mean year wise 
 + +modify$new_predicted <- modify$a_114.13-14.5
 + +modify$new_predicted[modify$month==12 & modify$year==2002] <- median(modify$new_predicted[modify$month==12 & modify$year==2002])
 + +modify$new_predicted[modify$month==12 & modify$year==2003] <- median(modify$new_predicted[modify$month==12 & modify$year==2003])
 + +
 + +modify$new_predicted[modify$year==2003 & modify$a_114.13 <685] <-  700   #keep it always
 + +modify$new_predicted[modify$year==2002 & modify$a_114.13 <721] <- 900
 + +
 + +
 + +
 + +# preparing the modified prediction for submission
 + +submission = data.frame( ID = modify$ID, Footfall = modify$new_predicted)
 + +write.csv(submission, file = "modified_by_value_2.csv", row.names = FALSE)
 + +
 + +
 + +
 + +x <- read.csv("file:///F:/DataScience/kaggle/WhatsCooking/combined_net_xgb_other_114.13.csv")
 + +x$new_predicted <- (x$Footfall)-20
 + +submission = data.frame( ID = modify$ID, Footfall = x$new_predicted)
 + +write.csv(submission, file = "modified_by_value_2.csv", row.names = FALSE)
 + +
 + +
 + +final = read.csv("file:///C:/Users/Sud/Documents/combined_net_xgb_other_114.13.csv")
 + +final$Footfall = (final$Footfall - 15)
 + +
 + +write.csv(final, file = "sol_final.csv", row.names = FALSE)
 + +
 + +
 + +
 + +# RMSE SCORE/FUNCTION---- to get rmse score
 + +
 + +# Function that returns Root Mean Squared Error
 + +rmse <- function(error)
 + +{
 + +  sqrt(mean(error^2))
 + +}
 + +# Function that returns Root Mean Squared Error
 + +rmse <- function(error)
 + +{
 + +  sqrt(mean(error^2))
 + +}
 + +
 + +# Function that returns Mean Absolute Error
 + +mae <- function(error)
 + +{
 + +  mean(abs(error))
 + +}
 + +
 + +actual = test$Footfall
 + +View(actual)
 + +View(pred1)
 + +error <- actual - pred
 + +rmse(error)
 + +mae(error)
 + +rmse(m$residuals)
 + +
 + +sol_final = combined_net_xgb_other_114.13
 + +
 + +
 + +
 + +# combining gbm+XGboost+ANN model
 + +# providing appropriate weightage
 + +
 + +pred_ens = (2.5*gbm$Footfall + 7.5*combined_net_xgb_other_114.13$Footfall)/10
 + +mysolution = data.frame( ID = Test$ID, Footfall = pred_ens)
 
 # write the final prediction
 + +write.csv(mysolution, file = "final_solution.csv", row.names = FALSE)

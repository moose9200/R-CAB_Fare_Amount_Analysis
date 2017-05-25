# R-CAB_Fare_Amount_Analysis

    rm(list = ls())
    setwd("C:/Users/Moose/Desktop/einstine project")

    train = read.csv("train.csv")

    library(geosphere)
	library(mnormt) #GENERATE CORELATION
	library(corrplot) #VISUALIZE CORELATION
	library(caret)
	library(randomForest)
	library(clusterSim)
	library(DMwR)
	library(h2o)
	library("reshape2")
	library("ggplot2")

	h2o.init()

#Visualization

	pc_vs_Fare_amount = ggplot(data=train,
       aes(x=fare_amount, y=passenger_count)) +
  	geom_jitter()
	pc_vs_Fare_amount


	tolls_amount_vs_Fare_amount = ggplot(data=train,
                           aes(x=fare_amount, y=tolls_amount)) +
  	geom_jitter()
	tolls_amount_vs_Fare_amount



	tip_amount_vs_Fare_amount = ggplot(data=train,
                          aes(x=fare_amount, y=tip_amount)) +
 	 geom_jitter()
	tip_amount_vs_Fare_amount



	mta_tax_vs_Fare_amount = ggplot(data=train,
                          aes(x=fare_amount, y=mta_tax)) +
  	geom_jitter()
	mta_tax_vs_Fare_amount


	surcharge_vs_Fare_amount = ggplot(data=train,
                      aes(x=fare_amount, y=surcharge)) +
  	geom_jitter()
	surcharge_vs_Fare_amount


# converting longitude and latitude from Degree to Radian

	x = train[,10:11]*0.0174533   
	y =train[,14:15]*0.0174533

 #calculating the distance using longitude and latitude
 
		distance = distVincentyEllipsoid(x, y)  
		distance = as.data.frame(distance)

		options(scipen = 999) #conevrt exponantioal values to numaric
	
	#subserring the columns on the basis of Domain Knowledge

		train = train[c(4,5,6,9,12,17,18)] 

		train = cbind(train,distance)   # using distance as predictor

		distance  = NULL
		x= NULL
		y=NULL


		train$passenger_count = as.factor(train$passenger_count) #converting to respective data type
		train$rate_code = as.factor(train$rate_code)
	
#missing values detection by column

	sapply(train, function(x) sum(is.na(x)))
	
#converting to h2o data type

	train = as.h2o(train)                                     

#imputing missing values by their mean value

	h2o.impute(train,  method= "mean")                  




	data =train

#spliting the dataset into test and train

	prostate.split =  h2o.splitFrame(data=data, ratios=0.70)    

	train = prostate.split[[1]]
	test =  prostate.split[[1]]

#applying random forest using h2o because of constrain computational resources

	drf = h2o.randomForest (y = "fare_amount",
                        training_frame    = train,
                        validation_frame  = test,
                        ntrees            = 300,
                        max_depth         = 10)

   #calculating MAE error
	
		Model_Error = h2o.mae(drf)   
	    Model_Error

				Model_Accuracy = 100-Model_Error
				Model_Accuracy


#Prediction on Test Dataset

	test2 = read.csv("test.csv")       #data Import

 #converting longitude and latitude from Degree to Radian
 
		x = test2[,10:11]*0.0174533       
		y =test2[,14:15]*0.0174533


		distance = distVincentyEllipsoid(x, y)
		distance = as.data.frame(distance)

		options(scipen = 999)

		test2 = test2[c(1,4,5,6,9,12,17)] #Feature selection based on domain knowledge
		test2 = cbind(test2,distance)

		distance=NULL
		x=NULL
		y=NULL



		test2$passenger_count = as.factor(test2$passenger_count)
		test2$rate_code = as.factor(test2$rate_code)
		str(test2)

		sapply(test2, function(x) sum(is.na(x)))   #missing values detection by column 

		test_data = as.h2o(test2)

		h2o.impute(test_data,  method= "mean")


		pred = h2o.predict(drf, test_data[,-1])  #prediction on test dataset using our Regression model

		summary(pred,exact_quantiles=TRUE)


		predicted = as.data.frame(pred)       

		final_output = cbind(test2$TID, predicted)

		colnames(final_output)[1]="TID"
		colnames(final_output)[2]="fare_amount"


		write.csv(final_output, file = "final_output.csv",row.names = FALSE)

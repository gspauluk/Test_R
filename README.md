# Test_R
A. The overall purpose of this project was to create/train a model that was as accurate as possible for predicting the number of 
  bicycle rentals every hour of every day for almost 2 years in Washington D.C.   
B. The .gitignore file is a list of files and folders to ignore in the project, the git_test.R file is the summary data for the project under which the code used to create the model: Bike Share Competition.R is located. The results of my model's predictions are in the MySubmission.csv file. 
C. For cleaning the data, I removed the casual and registered variables as it was easier to create a model based on the overall count without their sub-counts. For feature engineering, I created the hour and month variables from the datetime variable, transformed the count into a log(count) because the data was left-skewed, and turned the season, holiday, month, weather, and hour variables into factors as they were the categorical variables I used in my model. I also created dummy variables for season amd weather and encoded them using both one-hot encoding and target encoding since they were categorical variables with multiple factor values other than 0 and 1.   
D. 

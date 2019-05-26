library(ggplot2)

setwd("C:/Users/Zuhaib Ahmed/Desktop")

data <- read.csv("pima-indians-diabetes.csv", header = T)
names(data) <- c("preg", "plas", "pres", "skin", "test", "mass", "pedi", 
                 "age", "class")

#Given a number, gives ~80% of them a lable of 1 and 20% of them a 
#label of 0. The number of rows in a data frame will be passed here
#to split them into test-train groups.
test_train_split <- function(num_rows) {
  newCol <- rbinom(n = num_rows, size = 1, prob = 0.8)
  split <- list(which(newCol == 1), which(newCol == 0))
  return(split)
}

#Takes in a column vector from a data frame and fits a normal curve to
#it by finding the maximum likelihood estimates of the parameters of
#a normal distribution (i.e. mean and sd), given the data in the 
#column. If ignore_zeros is true, it'll ignore the zeros in the 
#column
fit_normal <- function(col, ignore_zeros) {
  if (ignore_zeros == F) {
    return(list(mean(col), sd(col)))
  } else {
    zeros_excluded <- col[which(col != 0)]
    return(list(mean(zeros_excluded), sd(zeros_excluded)))
  }
}


#######PART A##########
accuracy <- c()
for (i in 1:10) {
  #Gets a test-train split
  lst <- test_train_split(nrow(data))
  training_df <- data[lst[[1]],]
  testing_df <- data[lst[[2]],]
  
  #Finds the parameters of the normal distribution fitted to the values
  #of the rows of the training data frame that correspond to people
  #with diabetes (i.e. class == 1)
  param_preg_yes <- fit_normal(training_df[which(training_df$class == 1),"preg"], F)
  param_plas_yes <- fit_normal(training_df[which(training_df$class == 1),"plas"], F)
  param_pres_yes <- fit_normal(training_df[which(training_df$class == 1),"pres"], F)
  param_skin_yes <- fit_normal(training_df[which(training_df$class == 1),"skin"], F)
  param_test_yes <- fit_normal(training_df[which(training_df$class == 1),"test"], F)
  param_mass_yes <- fit_normal(training_df[which(training_df$class == 1),"mass"], F)
  param_pedi_yes <- fit_normal(training_df[which(training_df$class == 1),"pedi"], F)
  param_age_yes <- fit_normal(training_df[which(training_df$class == 1),"age"], F)
  
  #Finds the parameters of the normal distribution fitted to the values
  #of the rows of the training data frame that correspond to people
  #without diabetes (i.e. class == 0)
  param_preg_no <- fit_normal(training_df[which(training_df$class == 0),"preg"], F)
  param_plas_no <- fit_normal(training_df[which(training_df$class == 0),"plas"], F)
  param_pres_no <- fit_normal(training_df[which(training_df$class == 0),"pres"], F)
  param_skin_no <- fit_normal(training_df[which(training_df$class == 0),"skin"], F)
  param_test_no <- fit_normal(training_df[which(training_df$class == 0),"test"], F)
  param_mass_no <- fit_normal(training_df[which(training_df$class == 0),"mass"], F)
  param_pedi_no <- fit_normal(training_df[which(training_df$class == 0),"pedi"], F)
  param_age_no <- fit_normal(training_df[which(training_df$class == 0),"age"], F)
  
  num_correct <- 0
  for (j in 1:nrow(testing_df)) {
    #Goes through the test data, and determines the posterior probability
    #of the person having diabetes given the feature data
    prob_of_diabetes_yes <- length(which(training_df$class == 1)) / nrow(training_df)
    posterior_prob_of_diabetes_yes <- log(dnorm(testing_df[j,"preg"], param_preg_yes[[1]], param_preg_yes[[2]])) +
      log(dnorm(testing_df[j,"plas"], param_plas_yes[[1]], param_plas_yes[[2]])) +
      log(dnorm(testing_df[j,"pres"], param_pres_yes[[1]], param_pres_yes[[2]])) +
      log(dnorm(testing_df[j,"skin"], param_skin_yes[[1]], param_skin_yes[[2]])) +
      log(dnorm(testing_df[j,"test"], param_test_yes[[1]], param_test_yes[[2]])) +
      log(dnorm(testing_df[j,"mass"], param_mass_yes[[1]], param_mass_yes[[2]])) +
      log(dnorm(testing_df[j,"pedi"], param_pedi_yes[[1]], param_pedi_yes[[2]])) +
      log(dnorm(testing_df[j,"age"], param_age_yes[[1]], param_age_yes[[2]])) +
      log(prob_of_diabetes_yes)
    
    #Goes through the test data, and determines the posterior probability
    #of the person not having diabetes given the feature data
    prob_of_diabetes_no <- length(which(training_df$class == 1)) / nrow(training_df)
    posterior_prob_of_diabetes_no <- log(dnorm(testing_df[j,"preg"], param_preg_no[[1]], param_preg_no[[2]])) +
      log(dnorm(testing_df[j,"plas"], param_plas_no[[1]], param_plas_no[[2]])) +
      log(dnorm(testing_df[j,"pres"], param_pres_no[[1]], param_pres_no[[2]])) +
      log(dnorm(testing_df[j,"skin"], param_skin_no[[1]], param_skin_no[[2]])) +
      log(dnorm(testing_df[j,"test"], param_test_no[[1]], param_test_no[[2]])) +
      log(dnorm(testing_df[j,"mass"], param_mass_no[[1]], param_mass_no[[2]])) +
      log(dnorm(testing_df[j,"pedi"], param_pedi_no[[1]], param_pedi_no[[2]])) +
      log(dnorm(testing_df[j,"age"], param_age_no[[1]], param_age_no[[2]])) +
      log(prob_of_diabetes_no)
    
    classified <- 0
    if (posterior_prob_of_diabetes_yes >= posterior_prob_of_diabetes_no) {
      classified <- 1
    }
    if (classified == testing_df[j, "class"]) {
      num_correct <- num_correct + 1
    }
  }
  accuracy <- c(accuracy, num_correct / nrow(testing_df))
}
average_accuracy <- mean(accuracy)

#######PART B##########
#Almost the same as PART A. Teh only idfference is that in some of the
#columns, the zeros are being considered as missing values and excluded.
accuracy <- c()
for (i in 1:10) {
  #Gets a test-train split
  lst <- test_train_split(nrow(data))
  training_df <- data[lst[[1]],]
  testing_df <- data[lst[[2]],]
  
  #Finds the parameters of the normal distribution fitted to the values
  #of the rows of the training data frame that correspond to people
  #with diabetes (i.e. class == 1)
  param_preg_yes <- fit_normal(training_df[which(training_df$class == 1),"preg"], F)
  param_plas_yes <- fit_normal(training_df[which(training_df$class == 1),"plas"], F)
  param_pres_yes <- fit_normal(training_df[which(training_df$class == 1),"pres"], T)
  param_skin_yes <- fit_normal(training_df[which(training_df$class == 1),"skin"], T)
  param_test_yes <- fit_normal(training_df[which(training_df$class == 1),"test"], F)
  param_mass_yes <- fit_normal(training_df[which(training_df$class == 1),"mass"], T)
  param_pedi_yes <- fit_normal(training_df[which(training_df$class == 1),"pedi"], F)
  param_age_yes <- fit_normal(training_df[which(training_df$class == 1),"age"], T)
  
  #Finds the parameters of the normal distribution fitted to the values
  #of the rows of the training data frame that correspond to people
  #without diabetes (i.e. class == 0)
  param_preg_no <- fit_normal(training_df[which(training_df$class == 0),"preg"], F)
  param_plas_no <- fit_normal(training_df[which(training_df$class == 0),"plas"], F)
  param_pres_no <- fit_normal(training_df[which(training_df$class == 0),"pres"], T)
  param_skin_no <- fit_normal(training_df[which(training_df$class == 0),"skin"], T)
  param_test_no <- fit_normal(training_df[which(training_df$class == 0),"test"], F)
  param_mass_no <- fit_normal(training_df[which(training_df$class == 0),"mass"], T)
  param_pedi_no <- fit_normal(training_df[which(training_df$class == 0),"pedi"], F)
  param_age_no <- fit_normal(training_df[which(training_df$class == 0),"age"], T)
  
  num_correct <- 0
  for (j in 1:nrow(testing_df)) {
    #Goes through the test data, and determines the posterior probability
    #of the person having diabetes given the feature data
    prob_of_diabetes_yes <- length(which(training_df$class == 1)) / nrow(training_df)
    posterior_prob_of_diabetes_yes <- log(dnorm(testing_df[j,"preg"], param_preg_yes[[1]], param_preg_yes[[2]])) +
      log(dnorm(testing_df[j,"plas"], param_plas_yes[[1]], param_plas_yes[[2]])) +
      log(dnorm(testing_df[j,"pres"], param_pres_yes[[1]], param_pres_yes[[2]])) +
      log(dnorm(testing_df[j,"skin"], param_skin_yes[[1]], param_skin_yes[[2]])) +
      log(dnorm(testing_df[j,"test"], param_test_yes[[1]], param_test_yes[[2]])) +
      log(dnorm(testing_df[j,"mass"], param_mass_yes[[1]], param_mass_yes[[2]])) +
      log(dnorm(testing_df[j,"pedi"], param_pedi_yes[[1]], param_pedi_yes[[2]])) +
      log(dnorm(testing_df[j,"age"], param_age_yes[[1]], param_age_yes[[2]])) +
      log(prob_of_diabetes_yes)
    
    #Goes through the test data, and determines the posterior probability
    #of the person not having diabetes given the feature data
    prob_of_diabetes_no <- length(which(training_df$class == 1)) / nrow(training_df)
    posterior_prob_of_diabetes_no <- log(dnorm(testing_df[j,"preg"], param_preg_no[[1]], param_preg_no[[2]])) +
      log(dnorm(testing_df[j,"plas"], param_plas_no[[1]], param_plas_no[[2]])) +
      log(dnorm(testing_df[j,"pres"], param_pres_no[[1]], param_pres_no[[2]])) +
      log(dnorm(testing_df[j,"skin"], param_skin_no[[1]], param_skin_no[[2]])) +
      log(dnorm(testing_df[j,"test"], param_test_no[[1]], param_test_no[[2]])) +
      log(dnorm(testing_df[j,"mass"], param_mass_no[[1]], param_mass_no[[2]])) +
      log(dnorm(testing_df[j,"pedi"], param_pedi_no[[1]], param_pedi_no[[2]])) +
      log(dnorm(testing_df[j,"age"], param_age_no[[1]], param_age_no[[2]])) +
      log(prob_of_diabetes_no)
    
    classified <- 0
    if (posterior_prob_of_diabetes_yes >= posterior_prob_of_diabetes_no) {
      classified <- 1
    }
    if (classified == testing_df[j, "class"]) {
      num_correct <- num_correct + 1
    }
  }
  accuracy <- c(accuracy, num_correct / nrow(testing_df))
}
average_accuracy <- mean(accuracy)
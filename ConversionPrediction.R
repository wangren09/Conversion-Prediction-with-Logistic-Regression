##read files
data <- read.csv(file = "transactions.csv", head = TRUE, sep = ",")
data_sessions <- read.csv(file = "sessions.csv", head = TRUE, sep = ",")

##feature engineering: the final features are:
##impressions of all current ads, impressions of previous ads, number of previous interactions

#impressions of all current ads: ads number times ads relavence
install.packages("dplyr")
library(dplyr)
data = data %>% mutate(ads_impress = num_impressions*avg_relevance)

#impressions of previous ads, number of previous interactions, number of previous interactions, number of products bought before
install.packages("lubridate")
library(lubridate)
date_sess <- data$session_dt
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
date_sess = as.Date(date_sess)
NumDays = Diff(date_sess, date_sess) #transfer date to number of days in the year
data = data %>% mutate(NumDays) #add to the dataset

impress_prev = rep(0, length(data$session_id)) #impressions of previous ads
view_hist = rep(0, length(data$session_id)) #number of previous interactions (number of sessions before)
num_bought = rep(0, length(data$session_id)) #number of products bought before
for(i in 1:length(data$session_id)){
  sess_id = data$session_id[i] #session name
  days = data$NumDays[i]
  index = match(sess_id, data_sessions$session_id) #the row ind of the session
  user = data_sessions$user_id[index] #the user corresponds to the session
  index_user = which(data_sessions$user_id == user) #row numbers of the user
  sess_user = data_sessions$session_id[setdiff(index_user, index)]
  len = length(sess_user)
  index_sess = c()
  if(len > 0){
    for(j in 1:len){
      k = match(sess_user[j], data$session_id)
      if(!is.na(k)){
        index_sess <- c(index_sess, k)
      }
    }
  }
  if(is.na(index_sess) || length(index_sess) == 0){
    impress_prev[i] = 0
  } else {
    for(j in 1:length(index_sess)){
      dateold = data$NumDays[index_sess[j]]
      if(!is.na(dateold) && !is.na(days) && dateold < days){
        datediff = days - dateold
        impress_prev[i] = impress_prev[i] + data$ads_impress[index_sess[j]]/(datediff + 1)
        view_hist[i] = view_hist[i] + 1
        if(data$conversion[index_sess[j]] == TRUE){
          num_bought[i] = num_bought[i] + 1
        }
      }
    }
  }
}
data = data %>% mutate(impress_prev)
data = data %>% mutate(view_hist)
data = data %>% mutate(num_bought)

##separate data used in training, testing, and goodness fit
data_training <- subset(data, train == "TRUE")
data_testing <- subset(data, test == "TRUE")
data_goodness <- subset(data, score == "TRUE")


##Likelihood Ratio Test
mod_fit_one <- glm(conversion~num_search+ads_impress+impress_prev+view_hist, data_training, family = "binomial")
mod_fit_two <- glm(conversion~ads_impress+impress_prev+view_hist, data_training, family = "binomial")
anova(mod_fit_one, mod_fit_two, test ="Chisq")
mod_fit_three <- glm(conversion~impress_prev+view_hist, data_training, family = "binomial")
mod_fit_four <- glm(conversion~impress_prev+view_hist, data_training, family = "binomial")
mod_fit_five <- glm(conversion~ads_impress+impress_prev, data_training, family = "binomial")
anova(mod_fit_two, mod_fit_three, test ="Chisq")
anova(mod_fit_two, mod_fit_four, test ="Chisq")
anova(mod_fit_two, mod_fit_five, test ="Chisq")



##generate model: the variables we used are:
##number of search, impressions of all current ads, impressions of previous ads, number of previous interactions
attach(data_training)
model <- glm(conversion~ads_impress+impress_prev+view_hist, data_training, family = "binomial")
summary(model)


##goodness of fit: ROC test
res <- predict(model, data_goodness, type = "response")
install.packages("gplots")
install.packages("ROCR")
library(ROCR)
ROCRPred = prediction(res, data_goodness$conversion)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize=TRUE, print.cutoffs.at=seq(0.1,by=0.1))




##prediction on testing dataset
res <- predict(model, data_testing, type = "response")
data_testing = data_testing %>% mutate(prediction = res>0.3)
pred_accuracy = mean(data_testing$conversion == data_testing$prediction)
cat(" the prediction accuracy is ", pred_accuracy, "\n")
table(Actualvalue = data_testing$conversion, Predictvalue = res>0.3)
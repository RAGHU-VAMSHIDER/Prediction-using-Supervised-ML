# WRITTEN BY RAGHU VAMSHI
# DATA SCIENCE AND BUSINESS ANALYTICS INTERN AY THE SPARKS FOUNDATION
# PREDICTION USING SUPERVISED LEARNING 
# PREDICT SCORE OF STUDENT BASED ON THE NO OF STUDY HOURS

#Read the CSV file using read.csv
studentdata <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")

cor(studentdata$Hours,studentdata$Scores)
# As the correlation is 0.971 we can confirm that the variables are positively correlated

plot(studentdata,main = "Scatterplot",xlab = "Hours", ylab = "Scores")
# Plot the variables into a scatter plot
 
linemodstudentdata <- lm(studentdata$Scores~studentdata$Hours)
# We create a linear model using 'lm()' function
linemodstudentdata

plot(studentdata,main = "Scatterplot",xlab = "Hours", ylab = "Scores")
abline(linemodstudentdata)
# Plot the variables into a scatter plot and best fiting line to scatteplot

# We create a linear function for prediction
prediction <- function(x){
y <- (x*(linemodstudentdata[["coefficients"]][["studentdata$Hours"]])+(linemodstudentdata[["coefficients"]][["(Intercept)"]]))
return (y)
}
# y= x*m+i where m is slope and i is intercept

sample_hours <- studentdata$Hours[12:20]
# test sample hours

Actual_scores <- studentdata$Scores[12:20]
# actual scores for test samples

Predicted_scores <- prediction(sample_hours)
# actual scores for test samples

predictiontable <-cbind(sample_hours,Actual_scores,Predicted_scores)
colnames(predictiontable) <- c("Hours","actual Scores","predicted Scores")
# creating tabular form for test samples ,actual valus,predicted valus
prediction(9.25) 
# According to the linear model if a student studies 9.25 hrs, student is likely to score 92.9

# finding error in prediction
Mean_squared_error= mean((Actual_scores - Predicted_scores)^2)   
Root_Mean_Squared_Error = sqrt(Mean_squared_error)
Root_Mean_Squared_Error
# the rms-error in predicting value is 4.789 
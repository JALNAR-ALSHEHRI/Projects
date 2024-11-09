# Statistics
## Lab Assignment 2

###ChickWeight Data set

data("ChickWeight")
View(ChickWeight)
![image](https://github.com/user-attachments/assets/4d664997-2111-4452-a07e-70fdbaba09a7)

##Coefficient of determination for the simple linear reg model
#Weight = Dependent , Time = Independent
weight.lm = lm(weight ~ Time, data=ChickWeight) 
summary(weight.lm)$r.squared

summary(weight.lm)

weight.lm_summary <- summary(weight.lm)
p_value_time <- weight.lm_summary$coefficients["Time", "Pr(>|t|)"]
cat("P-value for Time: ", p_value_time, 
    ifelse(p_value_time <= 0.05,"(There is a significant relationship between the variables)"
           , "(There is no significant relationship between the variables)"), "\n")
#The p-value is 2.2e-16, wich is less than 0.05. 

#Women Data set
data("women")
View(women)
#Simple linear regression model.
weight = lm(weight ~ height, data=women) 
summary(weight)
predicted_weight <- predict(weight, women)
comparison_df <- data.frame( weight = women$weight,height = women$height
                             ,predicted_weight= predicted_weight)
print(comparison_df)


#Fish Data set
library(readr)
file_path <- "C:/Users/jalna/Downloads/Fish.csv"
Fish <- read.csv(file_path)
View(Fish)

colnames(Fish)
table(Fish$Species)

Weight = lm( Weight ~  
                Species + Length1 + Length2 + Length3 + Height + Width 
                , data=Fish)

# Make predictions for the given values for each species

fish_df1 <- data.frame (Length1 = c(28.5, 28.4)
                        ,Length2 = c(30.7, 31) 
                        ,Length3 = c(36.2, 36.2)
                        ,Height= c(14.2266,14.2628)
                        ,Width = c(4.9594,5.1042)) 
View(fish_df1) 
#Predicted weight over each species
predictions <- do.call(rbind
                      ,lapply(unique(Fish$Species)
                              ,function(species) {
                                     new_data <- fish_df1
                                     #Update Species column
                                     new_data$Species <- factor(species
                                                               ,levels = levels(Fish$Species))
  
                                     predicted_weights <- predict(Weight, newdata = new_data)
                                     #Return a data frame for the current species
                                     data.frame(Species = species, Predicted_Weight = predicted_weights)
                                         }
                            )   
                        )
View(predictions)
print(predictions)

#ِAssignment 2

#ChickWeight Data set
data("ChickWeight")
View(ChickWeight)

#Coefficient of determination for the simple linear reg model
#Weight = Dependent , Time = Independent
weight.lm = lm(weight ~ Time, data=ChickWeight) 
summary(weight.lm)$r.squared

summary(weight.lm)

weight.lm_summary <- summary(weight.lm)
p_value_time <- weight.lm_summary$coefficients["Time", "Pr(>|t|)"]
cat("P-value for Time: ", p_value_time, 
    ifelse(p_value_time <= 0.05,"(There is a significant relationship between the variables)"
           , "(There is no significant relationship between the variables)"), "\n")
 

# Women Data set
data("women")
View(women)

# Simple linear regression model.
weight = lm(weight ~ height, data=women) 
summary(weight)

# Predicted weight 
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

#  Multiple linear regression model.
weight.lm = lm( Weight ~  
                Species + Length1 + Length2 + Length3 + Height + Width 
                , data=Fish)

# Data frame for the given values for each species

fish_df1 <- data.frame (Length1 = c(28.5, 28.4)
                        ,Length2 = c(30.7, 31) 
                        ,Length3 = c(36.2, 36.2)
                        ,Height= c(14.2266,14.2628)
                        ,Width = c(4.9594,5.1042)) 
View(fish_df1) 

# Predicted weight for each species
Fish$Species <- as.factor(Fish$Species)
predictions <- c()

predictions <- do.call(rbind
                       ,lapply(unique(Fish$Species)
                               ,function(species) {
                                 new_data <- fish_df1
                                 # Update Species column
                                 new_data$Species <- factor(species
                                                            ,levels = levels(Fish$Species))
                                 
                                 predicted_weights <- predict(weight.lm, newdata = new_data)
                                 # Data frame for the current species and Predicted weight
                                 data.frame(Species = species, Predicted_Weight = predicted_weights)
                               }
                       )   
)
View(predictions)
print(predictions)



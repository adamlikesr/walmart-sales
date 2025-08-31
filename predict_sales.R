# Load packages
library(tidyverse)
library(readr)
library(here)


# Read CSV from user, omit any rows with NA's in Weekly_Sales or Temperature
data <- read_csv(here("data/Walmart.csv")) %>%
  as_tibble()


# Tidying data, remove rows including NA's in Weekly_Sales and Temperature
# Rename as "sales" and "temp"
# Remove outliers

walmartSales <- data %>% 
  
  transmute(sales = Weekly_Sales,
            temp = Temperature) %>% 
  
  filter(!is.na(sales), !is.na(temp),
         between(sales,
           quantile(sales, 0.25) - 1.5 * IQR(sales),
         quantile(sales, 0.75) + 1.5 * IQR(sales)
         )
  )
  

# Check if sales and temp have a linear relationship
ggplot(walmartSales,
       aes(x = temp, y = sales)
       ) +
  geom_point() + geom_smooth(se = FALSE)


# As you can see, the regression line is not linear, rather somewhat parabolic
# Lets fit a new regression line using a parabolic model
ggplot(walmartSales,
       aes(x = temp, y = sales)
       ) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE)


# Much better!
# Let's create a new model using this regression line

model <- lm(sales ~ temp + I(temp^2), data = walmartSales)

# Using this model, let's create a function that predicts the sales by an input temperature.
predict_sales <- function(input) {
  predictedSale <- predict(model, data.frame(temp = (input)
                                             )
                           )
  return(predictedSale)
}


# Now we can create a function that asks the user for an input, and then returns a predictied value


# Creates a new function
new_prediction <- function() {
  input_temp <- as.numeric(readline(prompt = "Enter a temperature between 0 and 100 Fahrenheight:"))

# Returns error if input_temp is not a numeric value
  if (is.na(input_temp)) {
    cat("Invalid input, please enter a numeric value. \n")
    return(NULL)
  }

# Returns error if input_temp is not between 0 and 100
  if(input_temp < 0 | input_temp > 100) {
    cat("Invalid input, please enter a temperature between 0 and 100. \n")
    return(NULL)
  }

# Feeds input_temp into predict_sales(), returns output statement  
  outputPrediction <- predict_sales(input_temp)
  
  cat("Predicted sales for a temperature ", input_temp, " is $", outputPrediction, "\n")
  }

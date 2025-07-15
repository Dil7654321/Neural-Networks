# Clear the environment
rm(list = ls())

# Load the necessary libraries
library("neuralnet")
library("readr")  # For reading CSV files

# Read the input file
mydata_pre <- read.csv("C:/Users/Dilshan/Desktop/Portfolio Projects/Neural Networks/weatherHistory.csv", header = TRUE)
mydata <- mydata_pre[1:100, ]

# Normalize the data
maxs <- apply(mydata[, c("Humidity", "Wind.Speed..km.h.", "Temperature..C.")], 2, max)
mins <- apply(mydata[, c("Humidity", "Wind.Speed..km.h.", "Temperature..C.")], 2, min)
mydata_scaled <- as.data.frame(scale(mydata[, c("Humidity", "Wind.Speed..km.h.", "Temperature..C.")], center = mins, scale = maxs - mins))

# Rename columns in the scaled data
colnames(mydata_scaled) <- c("Humidity", "Wind.Speed", "Temperature")

# Train the neural network on the normalized data
model <- neuralnet(
  Temperature ~ Humidity + Wind.Speed,
  data = mydata_scaled,
  hidden = c(50,50,50),
  threshold = 0.001,
  stepmax = 1e+08
)

# Make predictions and rescale back to the original scale
predicted_scaled <- as.data.frame(model$net.result)
predicted <- predicted_scaled * (max(mydata$Temperature..C.) - min(mydata$Temperature..C.)) + min(mydata$Temperature..C.)

# Create final output with expected and predicted temperatures
final_output <- cbind(mydata$Humidity, mydata$`Wind.Speed..km.h.`, mydata$Temperature..C., predicted)
colnames(final_output) <- c("Humidity", "Wind Speed", "Expected Temperature", "Predicted Temperature")

# Calculate percent error
final_output$Percent_Error <- abs((final_output$`Expected Temperature` - final_output$`Predicted Temperature`) / final_output$`Expected Temperature`) * 100

# Calculate Mean Percentage Error (MPE)
mpe <- mean(final_output$Percent_Error)
print(paste("Mean Percentage Error (MPE):", mpe))

# Print the final output with percent error
print(final_output)

#print(model)

plot(model)

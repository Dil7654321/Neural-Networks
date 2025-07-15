rm(list = ls())
# Load the necessary library
#install.packages("neuralnet")
library("neuralnet")

# Read the input file
mydata_pre <- read.csv("C:/Users/Dilshan/Desktop/Portfolio Projects/Neural Networks/weatherHistory.csv", header = TRUE)
mydata <- mydata_pre[1:100, ]

# Verify the structure of the data to ensure column names are correct
print(names(mydata))  # This will show the column names in the dataset

# Check if the expected columns exist in the data
if (!("Humidity" %in% names(mydata)) || !("Wind.Speed..km.h." %in% names(mydata)) || !("Temperature..C." %in% names(mydata))) {
  stop("The data frame does not contain the expected columns. Please check the column names.")
}

# Train a neural network model using Humidity and Wind Speed to predict Temperature
model <- neuralnet(
  Temperature..C. ~ Humidity + `Wind.Speed..km.h.`,  # Using Humidity and Wind Speed as inputs, and Temperature as output
  data = mydata,
  hidden = c(5, 3),           # Two hidden layers with 5 and 3 neurons respectively
  threshold = 0.01,           # Set an appropriate threshold
  stepmax = 1e+06             # Increase the maximum steps for convergence if needed
)

# Print model summary
print(model)

# Plot the neural network model
plot(model)

# Generate predictions
final_output <- cbind(mydata$Humidity, mydata$`Wind.Speed..km.h.`, mydata$`Temperature..C.`, as.data.frame(model$net.result))
colnames(final_output) <- c("Humidity", "Wind Speed", "Expected Temperature", "Predicted Temperature")
print(final_output)

rm(list = ls())
# Load the necessary library
#install.packages("readxl")
library(readxl)
#install.packages("neuralnet")
library("neuralnet")

# Read the input file
mydata <- read_excel("C:/Users/Dilshan/Desktop/Portfolio Projects/Neural Networks/CyclePlant.xlsx")

# Verify the structure of the data to ensure column names are correct
print(names(mydata))  # This will display "AT", "V", "AP", "RH", "PE"

# Check if the expected columns exist in the data
if (!("AT" %in% names(mydata)) || !("V" %in% names(mydata)) || !("AP" %in% names(mydata)) || !("PE" %in% names(mydata))) {
  stop("The data frame does not contain the expected columns. Please check the column names.")
}

# Train a neural network model using the columns from CyclePlant data
model <- neuralnet(
  PE ~ AT + V + AP,  # Using "AT", "V", and "AP" as inputs, and "PE" as output
  data = mydata,
  hidden = c(20, 15, 10),    # Three hidden layers with 20, 15, and 10 neurons respectively
  threshold = 0.01,         # Lower threshold for higher precision
  stepmax = 1e+07            # Increase the maximum steps for convergence
)

# Print model summary
print(model)

# Plot the neural network model
plot(model)

# Check the data - actual and predicted
final_output <- cbind(mydata$AT, mydata$V, mydata$AP, mydata$PE,
                      as.data.frame(model$net.result))
colnames(final_output) <- c("AT", "V", "AP", "Expected PE", "Neural Net Predicted PE")
print(final_output)

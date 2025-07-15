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

# Normalize the data
maxs <- apply(mydata, 2, max)
mins <- apply(mydata, 2, min)
mydata_scaled <- as.data.frame(scale(mydata, center = mins, scale = maxs - mins))

# Re-train the model on normalized data
model <- neuralnet(
  PE ~ AT + V + AP,
  data = mydata_scaled,
  hidden = c(20, 15, 10),
  threshold = 0.001,
  stepmax = 1e+07
)

# After prediction, reverse normalization on output
predicted_scaled <- as.data.frame(model$net.result)
predicted <- predicted_scaled * (max(mydata$PE) - min(mydata$PE)) + min(mydata$PE)
final_output <- cbind(mydata, "Neural Net Predicted PE" = predicted)

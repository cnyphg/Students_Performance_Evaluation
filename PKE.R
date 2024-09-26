# Phang Khong Eer
#------------------------------------------------------------------------------------------------------------------------------------------------------
# DATA PREPARATION
# Install and load require packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("polycor")

library(ggplot2)
library(dplyr)
library(polycor)

# File declaration
df<-read.csv("/Users/conny/Library/CloudStorage/OneDrive-AsiaPacificUniversity/A Y2/R Programming/Assignment/student_prediction.csv")
# View data
df

# Checking null value
sum(is.na(df))


#------------------------------------------------------------------------------------------------------------------------------------------------------
# DATA ANALYSIS
# Factories the attributes name
dfc <- df # Create a copy of df to store factors
dfc$NOTES <- factor(dfc$NOTES,levels = c(1,2,3), 
                    labels =c("Never","Sometimes","Always"), 
                    ordered = TRUE)
dfc$CUML_GPA <- factor(dfc$CUML_GPA,levels = c(1,2,3,4,5), 
                       labels =c("Below 2.0","2.0 - 2.49", "2.5 - 2.99","3.0 - 3.49","Above 3.5"), 
                       ordered = TRUE)
dfc$PREP_EXAM <- factor(dfc$PREP_EXAM, levels = c(1, 2, 3), 
                        labels = c("Close To Exam", "Regularly", "Never"), 
                        ordered = TRUE)

# 3.4.1: UNIVARIANT ANALYSIS
# Convert the counts into a dfc frame
counts <- table(dfc$NOTES)
notes_counts_data <- data.frame(NOTES = names(counts), Count = as.numeric(counts))

# Pie Chart
ggplot(notes_counts_data, aes(x = "", y = Count, fill = NOTES)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(round(Count/sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "black", 
            vjust = "center", check_overlap = TRUE) +
  coord_polar("y", start = 0) +
  labs(title = "Number of Students by Taking Notes in Class") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void() +  # Removes axis labels and gridlines
  theme(plot.title = element_text(face = "bold", hjust = 0.6)) +
  theme(legend.title = element_text(face = "bold", size = 10))


# Bar Chart
ggplot(notes_counts_data, aes(x = NOTES, y = Count, fill = NOTES)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  labs(title = "Number of Students by Taking Notes in Class", 
       x = "Taking Notes in Class", 
       y = "Number of Students") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +  # Removes unnecessary background elements and grid lines
  theme(plot.title = element_text(face = "bold", hjust = 0.6)) +
  theme(legend.title = element_text(face = "bold", size = 10))


# 3.4.2: BIVARIANT ANALYSIS
counts_data <- dfc %>%
  count(NOTES, CUML_GPA)

# Group Bar Chart
ggplot(counts_data, aes(x = NOTES, y = n, fill = CUML_GPA)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.9) +
  geom_text(aes(label = paste(n,"(", scales::percent(n/sum(n)),")")), 
            position = position_dodge(width = 0.9), 
            vjust =-0.1, 
            size = 2.5, 
            color = "black") +
  labs(title = "Impact of Taking Notes on Student's CGPA", 
       x = "Taking Notes in Class", 
       y = "Number of Students", 
       fill = "CGPA") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.6)) +
  theme(legend.title = element_text(face = "bold", size = 10))  
  

# 3.4.3: MULTIVARIANT ANALYSIS
# Calculate counts of students for each combination of CUML_GPA, NOTES, and PREP_EXAM
countdata <- dfc %>%
  count(CUML_GPA, NOTES, PREP_EXAM)

# Grouped Bar Chart
ggplot(countdata, aes(x = CUML_GPA, y = n, fill = PREP_EXAM)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.7), size = 3, vjust = 0.5) +
  facet_grid(NOTES ~ .) +
  labs(title = "CGPA by Exam Preparation for Each Level of Taking Notes", 
       x = "CGPA", 
       y = "Number of Students", 
       fill = "Preparation for Exam") + 
  scale_fill_brewer(palette = "Pastel1") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(legend.position = "right") +
  theme(legend.title = element_text(face = "bold", size = 10, hjust = 0.6))


# 3.4.4: CHI-SQUARE TEST
table(dfc$NOTES,dfc$CUML_GPA) 
chisq.test(dfc$NOTES,dfc$CUML_GPA)

# 3.4.5: CORRELATOIN (POLYCHORIC) 
polychor(dfc$NOTES, dfc$CUML_GPA)


#------------------------------------------------------------------------------------------------------------------------------------------

# ADDITIONAL FEATURE - Logistic Regression Model

#STEP 1: Load the Data
# Install and load necessary library
install.packages("tidyverse")
install.packages("caret")
install.packages("car")
install.packages("ROCR")
install.packages("pROC")
install.packages("pscl")

library(tidyverse)
library(caret)
library(car)
library(ROCR)
library(pROC)
library(pscl)

# Load the dataset
data <- read.csv("/Users/conny/Library/CloudStorage/OneDrive-AsiaPacificUniversity/A Y2/R Programming/Assignment/student_prediction.csv")
# View summary of dataset
summary(data)
str(data)

# Convert to binary variables (High = 1 or Low = 0)
data$CUML_GPA <- ifelse(data$CUML_GPA %in% c(4,5), "1", "0")     # Above 3.0 = 1, Below 3.0 = 0
data$NOTES <- ifelse(data$NOTES %in% c(3), "1", "0")             # Always = 1, Sometimes/Never = 0
data$PREP_EXAM <- ifelse(data$PREP_EXAM %in% c(2), "1", "0")     # Regularly = 1, Close/Never  = 0
data$LISTENS <- ifelse(data$LISTENS %in% c(3), "1", "0")         # Always = 1, Sometimes/Never = 0
data$ATTEND <- ifelse(data$ATTEND %in% c(1), "1", "0")           # Always = 1, Sometimes/Never = 0

# Convert categorical variables to factors
data$NOTES <- as.factor(data$NOTES)
data$PREP_EXAM <- as.factor(data$PREP_EXAM)
data$LISTENS <- as.factor(data$LISTENS)
data$ATTEND <- as.factor(data$ATTEND)
data$CUML_GPA <- as.factor(data$CUML_GPA)

#STEP 2: Create Training and Test Samples
# Make this example reproducible
set.seed(1)

# Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[-sample, ]  
str(test)

#STEP 3: Fit the Logistic Regression Model
# Build the logistic regression model
model <- glm(CUML_GPA ~ NOTES + PREP_EXAM + LISTENS + ATTEND,
             data = train,
             family = "binomial")

# View model summary
summary(model)

# Assessing Model Fit
# Evaluate the McFadden's pseudo R-squared
pscl::pR2(model)["McFadden"]

# Compute variable importance
caret::varImp(model)

# Calculate VIF values
car::vif(model)

#STEP 4: Use the Model to Make Predictions
# Make predictions on the test set
predictions <- predict(model, newdata = test, type = "response")

# Set levels of predicted_classes based on levels of actual classes
predicted_classes <- factor(ifelse(predictions > 0.5, "1", "0"), levels = levels(test$CUML_GPA))

# Confusion Matrix
conf_matrix <- confusionMatrix(predicted_classes, test$CUML_GPA)
conf_matrix

# ROC Curve
roc_obj <- roc(test$CUML_GPA, predictions)

# Reverse the specificity values
auc <- auc(roc_obj)

# Plotting ROC Curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc(roc_obj), 2), ")"), 
     col = "blue", 
     lwd = 2, 
     legacy.axes = TRUE, 
     asp = 1)

#------------------------------------------------------------------------------------------------------------------------------------------------------

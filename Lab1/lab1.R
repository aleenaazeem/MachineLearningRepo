install.packages("factoextra")

# 1. Import the Heart Attack dataset
df <- read.csv("Heart_Attack.csv")

# 2. Summarize the dataset
summary(df)

# 3. Show structure and dimension
str(df)
dim(df)

# 4. First 8 rows and last 5 rows
head(df, 8)
tail(df, 5)

# 5. Column names
colnames(df)

# 6. Identify class variable
# Let's assume it's 'output' (commonly the target variable in heart datasets)
unique(df$output)
table(df$output)

# 7. Check datatype
class(df$output)

# 8. Convert to factor
df$output <- as.factor(df$output)
str(df$output)

# 9. Sum of missing values
sum(is.na(df))

# 10. Columns with missing values
colSums(is.na(df))

# 11. Replace missing values with 0
df[is.na(df)] <- 0
sum(is.na(df))  # Should return 0

# 12. Rename 'sex' from 0/1 to Female/Male
df$sex <- ifelse(df$sex == 1, "Male", "Female")

numeric_df <- df[, sapply(df, is.numeric)]
scaled_df <- scale(numeric_df)

#Use the Elbow Method to decide number of clusters
wss <- vector()
for (k in 1:10) {
  wss[k] <- sum(kmeans(scaled_df, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters K",
     ylab = "Total Within Sum of Squares",
     main = "Elbow Method for Optimal K")
set.seed(123)
km_result <- kmeans(scaled_df, centers = 2, nstart = 25)

# Check cluster sizes
table(km_result$cluster)
library(factoextra)
# Use only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]
scaled_df <- scale(numeric_df)
set.seed(123)
km_result <- kmeans(scaled_df, centers = 2, nstart = 25)
fviz_cluster(km_result, data = scaled_df,
             ellipse.type = "norm",
             palette = "jco",
             ggtheme = theme_minimal())


install.packages("plotly")
library(plotly)

# Example with first 3 principal components
pca <- prcomp(scaled_df)
pc_df <- data.frame(pca$x[,1:3])
pc_df$cluster <- factor(km_result$cluster)

plot_ly(pc_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, colors = c('#FF5733','#33C1FF')) %>%
  add_markers()
#question 14
# Only numeric variables
numeric_df <- df[, sapply(df, is.numeric)]

# Correlation matrix
cor_matrix <- cor(numeric_df)

# View the matrix
round(cor_matrix, 2)
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

#plotting
# Scatter plot of Age vs Cholesterol
plot(df$age, df$chol,
     main = "Age vs Cholesterol",
     xlab = "Age",
     ylab = "Cholesterol",
     col = "blue",
     pch = 1)  # Open circles

# Add open red triangles on top
points(df$age, df$chol,
       col = "red",
       pch = 2)  # Open triangles
# ggplot
# Load ggplot2 library
library(ggplot2)

# ggplot scatter plot with filled square points (pch = 22)
ggplot(df, aes(x = age, y = trtbps)) +
  geom_point(shape = 22, fill = "lightblue", color = "black", size = 3) +
  labs(title = "Scatter Plot of Age vs Resting Blood Pressure",
       x = "Age",
       y = "Resting Blood Pressure") +
  theme_minimal()
# example 2
ggplot(df, aes(x = age, y = thalachh)) +
  geom_point(shape = 22, fill = "blue") +
  labs(title = "Age vs Maximum Heart Rate",
       x = "Age",
       y = "Max heart rate") +
  theme_minimal()
# bar plot
# Create a frequency table of age
age_counts <- table(df$age)

# Barplot of age distribution
barplot(age_counts,
        main = "Distribution of Age",
        xlab = "Age",
        ylab = "Number of Patients",
        col = "lightblue",
        border = "black")
#histogram 
# a. Find the minimum and maximum of 'cp'
min_cp <- min(df$cp, na.rm = TRUE)
max_cp <- max(df$cp, na.rm = TRUE)

# Print min and max values
min_cp
max_cp

# b & c. Create the histogram with seq() for breaks
hist(df$cp,
     breaks = seq(min_cp, max_cp, 1),  # Breaks at 1-unit intervals
     col = "skyblue",
     main = "Chest Pain Type",
     xlab = "Chest Pain Category",
     ylab = "Number of Patients",
     border = "black")
#boxplot
boxplot(df$age,
        main = "Boxplot of Age",
        ylab = "Age",
        col = "lightgreen",
        border = "darkgreen")

# Convert output to numeric if it's a factor
if (is.factor(df$output)) {
  df$output <- as.numeric(as.character(df$output))
}
# Load library
library(corrplot)

# Select only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]

# Compute correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Plot correlation matrix
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.8,
         title = "Correlation Plot of Heart Attack Dataset",
         mar = c(0,0,1,0))


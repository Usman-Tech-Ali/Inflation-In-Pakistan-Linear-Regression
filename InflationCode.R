#GDP, ImfDebt,  


# Task 1
# importing the Inflation file from the Local device
inflation <- read.csv("D:/4 Semester/P&S/Project/Variables.csv")

# Display the default number of rows from the Happy
head(inflation)
# Use the 'dim()' function to obtain the dimensions of the Happy
# This returns a vector with the number of rows and columns in Happy
dim(inflation)
# Use the 'names()' function to get the names of the columns in the Happy
names(inflation)


#Task 2
# Use the 'summary()' function to generate a statistical summary of the Happy
# This provides a quick overview of the data, including min, max, mean, and quartiles for numeric data
summary(inflation)
#for calculating Mode 
# Function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#calculating mode of each variable
Imports_Score <- get_mode(inflation$Imports)
Expenditure_Score <- get_mode(inflation$Expenditure)
MilitaryExpenditure_Score <- get_mode(inflation$MilitaryExpenditure)
ExternalDebt_Score <- get_mode(inflation$ExternalDebt)
TotalReserves_Score <- get_mode(inflation$TotalReserves)
ExterDebtLn_Score <- get_mode(inflation$ExterDebtLn)
TotalReservesLn_Score <- get_mode(inflation$TotalReservesLn)
Exports_Score <- get_mode(inflation$Exports)
EmploymentInIndustry_Score <- get_mode(inflation$EmploymentInIndustry)
EmploymentInServices_Score <- get_mode(inflation$EmploymentInServices)
TotalReservesDebt_Score <- get_mode(inflation$TotalReservesDebt)

Imports_Score
Expenditure_Score
MilitaryExpenditure_Score
ExternalDebt_Score
TotalReserves_Score
ExterDebtLn_Score
TotalReservesLn_Score
Exports_Score
EmploymentInIndustry_Score
EmploymentInServices_Score
TotalReservesDebt_Score


Inflation <- data$Inflation
Imports <- data$Imports
Exports <- data$Exports
TotalReserves <- data$TotalReservesDebt

print(Inflation)
print(Imports)
print(Exports)
print(TotalReserves)

dataFrame <- data.frame(Inflation, Imports,Exports,TotalReserves);

print(dataFrame)


#Task 3
# Create boxplots
# boxplot
boxplot(dataFrame, 
        outline = TRUE,
        horizontal = TRUE,
        main = "Horizontal Boxplot of Inflation, Imports,Exports,TotalReserves",
        col = c("blue", "green", "red","yellow"),
        xlab = "Values")

#Task 4
# Create scatterplot with color and shape by continent
# scatter plot
pairs(Inflation ~ Imports+Exports+TotalReserves,
      data= data,
      main = "Scatter Plot Matrix for Inflation, Imports,Export,TotalReserves",
      col = c("blue", "green", "red","yellow"))

#Task 5
#
model <- lm(Inflation ~  Imports+Exports+TotalReserves, data=inflation)
model
summary(model)


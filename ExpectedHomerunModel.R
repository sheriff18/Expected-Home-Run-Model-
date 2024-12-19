#### Read important packages into R
library(dplyr)
library(readr)
library(mice)
library(VIM)
library(lattice)

#### Data Cleaning, Processing and Manipulation
# Load data
Events <- read_csv("Events.csv")
Pitches <- read_csv("Pitches.csv")

# Check data structure
str(Events)
str(Pitches)

# Convert to numeric variables
numeric_vars <- c("Hit_ExitVelo", "Hit_SprayAngle", "Hit_LaunchAngle", "Hit_SpinRate", "Hit_Distance", "Hit_HangTime")
Pitches[numeric_vars] <- lapply(Pitches[numeric_vars], function(x) as.numeric(as.character(x)))

# Join data sets using inner join to retain all events
df <- inner_join(Events, Pitches, by = c("GameID", "Sequence"))

# Check missing data percentage
missing_count <- colSums(is.na(df))
missing_percentage <- (missing_count / nrow(df)) * 100
print(missing_percentage)


# Convert categorical variables to factors
impute_data$Result <- as.factor(impute_data$Result)

# Prepare data for imputation - Prediction Matrix
impute_data <- df %>%
  select(
    Hit_ExitVelo, Hit_LaunchAngle, Hit_SprayAngle, Hit_SpinRate,
    Hit_Distance, Hit_HangTime, Result,
    
  )



# Visualize missing data
md.pattern(impute_data)
aggr_plot <- aggr(impute_data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(impute_data), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))

# Define imputation methods
methods <- make.method(impute_data)

# Use predictive mean matching
methods[c("Hit_ExitVelo", "Hit_LaunchAngle", "Hit_SprayAngle", "Hit_SpinRate", "Hit_Distance", "Hit_HangTime")] <- "pmm"

# Perform imputation
set.seed(123)
imputed <- mice(impute_data, m = 5, method = methods, maxit = 50, seed = 500)

# Diagnose imputation
summary(imputed)
plot(imputed)
densityplot(imputed)

# Complete the data
completed_data <- complete(imputed, 1)


# LASSO for specific variables
methods_lasso <- make.method(impute_data)
methods_lasso[c("Hit_ExitVelo", "Hit_LaunchAngle", "Hit_SprayAngle", "Hit_SpinRate", "Hit_Distance", "Hit_HangTime")] <- "lasso.norm"

# Perform imputation with LASSO
set.seed(123)
imputed_lasso <- mice(impute_data, m = 5, method = methods_lasso, maxit = 50, seed = 500)

# Diagnose and visualize results
summary(imputed_lasso)
densityplot(imputed_lasso)
# Complete the data with LASSO
completed_data_lasso <- complete(imputed_lasso, 1)

# Integrate back into original dataframe if necessary
df_imputed <- df
df_imputed$Hit_ExitVelo_pmm <- completed_data$Hit_ExitVelo
df_imputed$Hit_LaunchAngle_pmm <- completed_data$Hit_LaunchAngle
df_imputed$Hit_SprayAngle_pmm <- completed_data$Hit_SprayAngle
df_imputed$Hit_SpinRate_pmm <- completed_data$Hit_SpinRate
df_imputed$Hit_Distance_pmm <- completed_data$Hit_Distance
df_imputed$Hit_HangTime_pmm <- completed_data$Hit_HangTime

# Add LASSO-imputed columns
df_imputed$Hit_ExitVelo_LASSO <- completed_data_lasso$Hit_ExitVelo
df_imputed$Hit_LaunchAngle_LASSO <- completed_data_lasso$Hit_LaunchAngle
df_imputed$Hit_SprayAngle_LASSO <- completed_data_lasso$Hit_SprayAngle
df_imputed$Hit_SpinRate_LASSO <- completed_data_lasso$Hit_SpinRate
df_imputed$Hit_Distance_LASSO <- completed_data_lasso$Hit_Distance
df_imputed$Hit_HangTime_LASSO <- completed_data_lasso$Hit_HangTime

# Verify whether the imputation worked by comparing with original data
tail(df_imputed$Hit_Distance,5)
tail(df_imputed$Hit_Distance_LASSO,5)
tail(df_imputed$Hit_Distance_pmm,5)



###### Model Building
# Analyze the Result Variable(Dependent variable).
table(df_imputed$Result)

# Install ggplot2 if not already installed
# install.packages("ggplot2")

# Load ggplot2 library
library(ggplot2)

# Basic Count Plot
ggplot(df_imputed, aes(x = Result)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Hit Results",
       x = "Result Type",
       y = "Count")

# Create HomeRun column to be compatible with modeling
df_imputed$HomeRun <- ifelse(df_imputed$Result == "HomeRun", 1, 0)

# Build GAM 

library(mgcv)
library(nlme)
model1 <- gam(  
  HomeRun ~ s(Hit_ExitVelo_pmm, Hit_LaunchAngle_LASSO,
              Hit_SprayAngle_LASSO,Hit_HangTime_pmm),
  family = binomial,
  data = df_imputed
)

summary(model1)

# Calculate AIC and BIC
model_aic <- AIC(model1)
model_bic <- BIC(model1)

# Display results
model_aic
model_bic

# Get Predictions

df_imputed$Predicted_Prob_HR <- predict(model1, type = "response")

# Summary statistics of predicted probabilities
summary(df_imputed$Predicted_Prob_HR)

leaderboard <- df_imputed %>%
  mutate(HomeRun_numeric = as.numeric(as.character(HomeRun))) %>%  # Convert factor to numeric
  group_by(BatterID, BattingTeamID) %>%
  summarise(ExpectedHomeRuns = sum(Predicted_Prob_HR, na.rm = TRUE),
            ActualHomeruns = sum(HomeRun_numeric, na.rm = TRUE)) %>%
  mutate(Performance = case_when(
    ActualHomeruns > round(ExpectedHomeRuns) ~ "Overperformer",
    ActualHomeruns < round(ExpectedHomeRuns) ~ "Underperformer",
    TRUE ~ "As Expected"
  )) %>%
  arrange(desc(ExpectedHomeRuns)) 

# View the top over performers and under performers
print(leaderboard)
# Save the leaderboard to a CSV file
write.csv(leaderboard, "leaderboard.csv", row.names = FALSE)


##### Get visualizations for further analysis

# Home Run Probability by Pitch Velocity and Type
ggplot(df_imputed, aes(x = Pitch_Velo, y = Predicted_Prob_HR, color = PitchType)) +
  geom_smooth(method = "loess") +
  labs(title = "Home Run Probability by Pitch Velocity and Type",
       x = "Pitch Velocity (mph)", y = "Predicted Probability of Home Run") +
  theme_minimal()

# Home Run Probability by Launch Angle and Pitch Type
ggplot(df_imputed, aes(x = Hit_LaunchAngle, y = Predicted_Prob_HR, color = PitchType)) +
  geom_smooth(method = "gam") +
  labs(title = "Home Run Probability by Launch Angle and Pitch Type",
       x = "Launch Angle (degrees)", y = "Predicted Probability of Home Run") +
  theme_minimal()

# What is the optimal launch angle? 
# Filtered
df_imputed |>
  filter(Hit_ExitVelo_pmm >= 100, Hit_ExitVelo_pmm <= 105) |>
  ggplot(aes(x = Hit_LaunchAngle_LASSO, y = Predicted_Prob_HR)) +
  geom_smooth(method = "gam") +
  scale_y_continuous(
    "Probability of a Home Run",
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    "Launch Angle (degrees)",
    limits = c(10, 50)
  )

# Unfiltered

df_imputed |>
  ggplot(aes(x = Hit_LaunchAngle_LASSO, y = Predicted_Prob_HR)) +
  geom_smooth(method = "gam") +
  scale_y_continuous(
    "Probability of a Home Run",
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    "Launch Angle (degrees)",
    limits = c(10, 50)
  )

# Spray Angle vs Hit Distance
data_hr <- df_imputed |> filter(HomeRun == 1)
ggplot(data_hr,aes(Hit_SprayAngle_LASSO,Hit_Distance_pmm)) + 
  geom_point(alpha = 0.27)


# Estimates of spray angle of homeruns hit by left and right handed hitters  
ggplot(data_hr,aes(Hit_SprayAngle_LASSO)) + geom_density() +
  facet_wrap(vars(BatSide),ncol = 1)


# Estimates of spray angle of homeruns hit by left and right handed hitters - without imputation  
ggplot(data_hr,aes(Hit_SprayAngle)) + geom_density() +
  facet_wrap(vars(BatSide),ncol = 1)


# Spray Angle vs Hit Distance - without imputations
data_hr <- df_imputed |> filter(HomeRun == 1)
ggplot(data_hr,aes(Hit_SprayAngle,Hit_Distance)) + 
  geom_point(alpha = 0.27)


### Are home runs about pitcher or hitter?
library(lme4)
library(nlme)
model_lme = glmer(
  HomeRun ~ (1|PitcherID) + (1|BatterID),
  data = df_imputed, family = binomial()
)
VarCorr(model_lme)
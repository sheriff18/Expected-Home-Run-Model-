
**Expected Home Run Model for Minor League Baseball Batters**

This project builds a model to predict the expected number of home runs for minor league baseball batters based on parameters like exit velocity, launch angle, spray angle, and hanging time. The goal is to identify batters who deserve more home runs by modeling home run probability and evaluating underachieving or overachieving players.

**Key Steps**:
Data Processing: Datasets were merged, and missing values for key batting parameters were imputed using Lasso and Predictive Mean Matching (PMM) techniques.
Model Building: A Generalized Additive Model (GAM) was used to predict home run probability, with exit velocity, launch angle, spray angle, and hanging time as key predictors.
Results: Model 1, which included smooth terms for all predictors, showed the best performance with a deviance explained percentage of 70.4%.
Key Takeaways:
The optimal launch angle for home runs is around 30°.
Spray angles between -25° and +25° and distances between 350-450 feet are typical for home runs.
Batters have a more significant impact on home run probability than pitchers.

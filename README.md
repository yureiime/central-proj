SMU DSA211 Project AY23/24
The data set Central2024P.csv records the information of 2500 private property resale transactions in the Central District of Singapore during 2023 (from REALIS database). 
We were to construct a predictive model to predict the _Price_ (dollars) of private property with respect to 5 independent variables, _Area_ (sqft), _Age_ (years), _Tenure_ (Freehold or Leasehold), _Purchaser_ (HDB or private), _Region_ (15 different regions in Central district of Singapore) 

Comments
Method 1: Multiple Linear and Polynomial Regression
Various combinations of interaction terms and polynomials of each non-categorical variable were tried, and models were compared using Adjusted R-squared. MSE was then calculated on the final best model for ease of comparison against others.

Methods 3, 4, 5: Ridge, Lasso and Elastic Net
Cross validation was performed on each to determine the best lambda and then MSE was calculated with the best lambda value.

Method 6: K-nearest Neighbors
Initially we tried normalizing all variables (even the categorical ones converted to numeric), but our resultant MSE was magnitudes lower than every other model, so we decided not to scale instead. We understand that this could cause problems in KNN since 
KNN uses distance measuring and not scaling would cause one variable to possibly dominate the others. We acknowledge that not every dataset and variable can be scaled also, hence affecting performance metrics relating to KNN, including but not limited to MSE.


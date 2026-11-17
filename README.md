# Linear Models
12NOV26 Data Science

Regression
  -used for simple (t-tests) to complex (automated variable selection via penalization

Linear models
  -intercept regression coefficient = true effect of predictor one
  -outcome you observe is not just coefficient, intercepts and predictors. Also errors
  -assumptions: residuals have mean zero, variance sigma^2
    -helpful to think that errors are normally distributed

  
Predictors
  - outcome is continuous
  -predictor can be anything 
    -numerical, categorical, binary, date variables, true/false vars
    -prefer outcome and predictor/ covariate vs (independent, dependent variable)
  -complication: continuous predictors (put predictive variables in model)
    -if categorical: creating indicator variables (not dummy variable)
      -indicates whether an observation has a particular group
        -collection of 0/1 variables: reference category and subsequent comparisons back to reference category
  -categorical predictor:
    -have to have a reference vector
    
Testing
  -for a single regression coeff you can construct a test statistic using t distribution
  -for larger has a normal distribution
    -(estimate - null) / standard error of estimate
  -Bhat estimate -from ordinary least squares
  -multiple coefficients: induces 3 indicator variables: test whether if it's significant have to test if all indicator variables taken as a collection is a significant improvement to original model using F test / Anova 

Diagnostics
  -many model assumptions: is model good enough : check by looking at residuals
    -difference between fitted value and actual data point
      -assumed centered at 0, assume a non-skewed distribution, outlying residuals (outliers are great indicators for model fit)
    -structure in pattern: have you fit the line how you needed to fit: do you have skews: low variance on one side while high variance on the other
      -departures from these assumptions > stats done (hypothesis test that you have made and interpretations) if model isn't right then these aren't right either: have to do a lot of diagnostic checks
  -major departures from assumptions: look at diagnostic: plot residual values
Generalized Linear models
  
  -lm for linear models
    -outcome that is 0 or 1 doesn't fit great in linear model: therefore have to fit in generalized linear model: continuous model to account
    -logistic regression: odds that Y=1 given that Y actually =0 and maps it onto a straight line
    -B1 is an odds ratio
    -then use glm
      
  -glm for generalized linear models
    -very useful
  -Arguments include:
    -formula: y~ x1+x1
    -in lm have to input formula you will use. on right hand side give predictors you will use
    -variable you will use
    -glm: have to tell a binary or count outcome
    
  - output is complex: lot's of stuff that you want to have: messy output is not what you want in some cases
    -broom package takes output of lm gives pieces that you are most interested in and organize it into a dataframe
      

Broom package
  -works for a lot packages and analyses
  -takes output of analyses and tidys up using broom
  -define data frame, tidy results (use broom for this)

# Statistics-Simulations

To see the application online click: https://mariared.shinyapps.io/Non-linear_models_dignostics/

The application is free to use and distibute. 

The application is a self-learning tool as well as a simulation tool, which allows us to see how the linear regression model performance changes if we violate the assumption of linearity in linear regression. 

The application allows us to dynamically change the model parameters, as well as the sample characteristics, and at the same time monitor how much will the prediction diverge from the "real values" with the parameter changes. The visualizations of the model, the summary tables and the residuals visualization, allow the user to gain intuition regarding "good" and "bad" behavior of the model.

The apllication is specifically created to learn the importance of checking for non-linear relationships between the dependant and independent variables. In social sciences possiblity of nonlinearity is often ignored. In research, since the samples sizes are often relatively small and we never have acess to the "real" values, we may never truely develp an understanding and intuition for what happens with our preidction and the vaidity of out model if we ignore non-linearity. I hope that this tool will be useful for anyone who want to better understand thier data in future research.  

*Suggestions* Where to start? 
I suggest trying to answer the following questions:
     *Suggestion* Play around and see which dignostic is the most "conservative" and prefers the simpler models, and which dignostic needs less eveidance for the more complex models. You can decrease the complexity by setting the True Model Quadratic Term and True Model Cubic Term to 0 in the Model Paramters panel. 
      *Suggestion* See, what happens to the residual plots when the sample corresponds to the true model? That is the shape of residuals histogram? What is the shape of the redsidual dependence plot (the second plot out of the three) if we try to fit a linear model, when the true model has a quadratic component? What happens to the scale-location plot if the we violate the homoscedasticity assumption (you can do that in the "What happens if you violate the assumptions?" panel)?
      *Suggestion*: Play around and see how does the spread of your independant variable influances the detectability of non-lieanrity. 
      

What can you do with the applications:

Tabs:
1. Model Analysis:
   1. a. Visualiztion
   In the main tab we can see the visualization of linear model fit, quadratic model fit and cubic model fit. The dashed blue line indicates the "real" model and the solid line indicated the model fit based on the gereated sample. The equation of the real model can be changed in Model Paramters panel on the left. The x-axis and y-axis name, minimum and maximum displayed values can be changed in the Graph Details panel.
  1. b. Diagnostics For One Sample
     The table contains the diagnostic stastiics used to comapre the fit of different models for one random sample from all generated samples . Green color shows which model it preferd by the given dignositc. E.g. if in the AIC column the green cells corresponds to linear model row, it means that according to AIC, linear model is the best fit.
     *Suggestion* Play around and see which dignostic is the most "conservative" and prefers the simpler models, and which dignostic needs less eveidance for the more complex models.
      
  1. c Model Comparison Across Samples
     The table contains diagnostics for all generated samples. The procentages correspond to for how many samples out of all generated samples, the diagnostics prefered given model. E.g. if AIC for linear regression is 60%, it mean that AIC was "the bes", that is indicated the best model fit for linear regerssion fro 60% of the generated samples.         
2. Assumptions Check
   There are four assumptions in learner models. We can check them by looking at the reisidual plots for the models. These are very useful diagnositc tools. We can get intution on how to read them by playing around model parameters and sample characteristics and monitor who it influances the residual plots.
   *Suggestion* See, what happens to the residual plots when the sample corresponds to the true model? That is the shape of residuals histogram? What is the shape of the redsidual dependence plot (the second plot out of the three) if we try to fit a linear model, when the true model has a quadratic component? What happens to the scale-location plot if the we violate the homoscedasticity assumption (you can do that in the "What happens if you violate the assumptions?" panel)?
     
3. Prediction
   The simulations also generate the prediction of the model. In the Prediction setting panel you can change for what values you of the dependent variable ("x") you want the predicitons ("y") to be generated. You can also change the width of both the Prediction Intervals and the Confidence Intervals by changing the alpha level. The cells will have green colors if the "true value" is contained in the Conficende Interaval or Prediction Interval. You can change if you want Confidence Intervals or Prediction Intervals displayed by changing the Interval Type in the Prediction panel.  


Panels:
1. Sample Specifics
   Allows the user to change the sample size, the numer of samples, as well, the mean and standard deviation of the independed variable. It also allows for intriduction of more or less noice though the changes to  the error mean and standard deviation.
   *Suggestion*: Play around and see how does the spread of your independant variable influances the detectability of non-lieanrity. 
   
2. Model Parameters
4. Violation of Homoscedascitiy
5. Graph Details
6. Predication Settings



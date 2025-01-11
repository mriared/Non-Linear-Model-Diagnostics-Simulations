# Non-Linear Model Diagnostics Simulations
Online Learning Application
---

To see the application online go to the link: https://mariared.shinyapps.io/Non-linear_models_dignostics/

## **Instructions**

### **Tabs Overview**

### **1. Model Analysis**

#### **Visualizations - The PLOTS**
In this tab, you can view visualizations of the linear model fit, quadratic model fit, and cubic model fit. 

- **Blue Dashed Line**: Represents the "true" model.
- **Solid Lines**: Represent the model fits based on the generated sample.

You can adjust the coefficients and the intercept of the true model  equation

$$
y = \text{intercept} + \beta_1 x + \beta_2 x^2 + \beta_3 x^3
$$

in the **Model Parameters** panel on the left. The x-axis and y-axis labels, along with their minimum and maximum displayed values, can be modified in the **Graph Details** panel.

---

#### **Diagnostics for One Sample**
This table displays diagnostic statistics used to compare the fit of different models for one random sample from all generated samples.

- **Green Cells**: Indicate the model preferred by a given diagnostic.  
   - For example, if the green cell in the AIC column corresponds to the linear model row, it means AIC prefers the linear model as the best fit.

**Suggestion**: Experiment with the diagnostics to explore which ones are more "conservative" (favor simpler models) and which are more likely to support complex models.

---

#### **Model Comparison Across Samples**
This table summarizes diagnostics for all generated samples.  

- **Percentages**: Indicate how often a specific model was preferred across all samples.  
   - For example, if AIC for the linear regression model is 60%, it means that AIC identified the linear regression model as the best fit for 60% of the generated samples.

---

### **2. Assumptions Check**

Linear models have four key assumptions. In this tab, you can investigate violations of three of them:
- **Linearity**
- **Homoscedasticity**
- **Normality**

This is achieved by examining the **residual plots** for the models. These plots serve as valuable diagnostic tools to help you develop intuition about model behavior.

**Suggestion**:
  - **Residual Histogram**: What shape does it take when the sample based model matches the true model?
  - **Residual Dependence Plot**: If you fit a linear model when the true model has a quadratic component, how does this plot look?
  - **Scale-Location Plot**: What happens if you violate the homoscedasticity assumption? (to violate this assumption go to the **"What happens if you violate the assumptions?"** panel on the left.)

---

### **3. Prediction**

This tab allows you to explore model predictions generated from the simulations.

- **Prediction Settings Panel**: Adjust the dependent variable ("x") values for which predictions ("y") are generated.
- **Alpha Level**: Change the width of the **Prediction Intervals** and **Confidence Intervals**.
- **Green Cells**: Highlighted if the "true value" is contained within the Confidence Interval or Prediction Interval.

**Additional Features**:
- Switch between displaying **Confidence Intervals** or **Prediction Intervals** by changing the **Interval Type** in the Prediction panel.

---

**Suggestions**
- Experiment with the settings in all tabs to observe how the models behave under different conditions.
- Explore how violating assumptions or changing parameters influences model fit, diagnostics, and predictions.

This tool is designed to enhance your understanding of linear models and provide intuition for diagnostics and prediction in real-world data scenarios.

---

## **Instructions**

### **Tabs Overview**

### **1. Model Analysis**

#### **Visualizations - The PLOTS**
In this tab, you can view visualizations of the linear model fit, quadratic model fit, and cubic model fit. 

- **Blue Dashed Line**: Represents the "true" model.
- **Solid Lines**: Represent the model fits based on the generated sample.

You can adjust the equation of the true model in the **Model Parameters** panel on the left. The x-axis and y-axis labels, along with their minimum and maximum displayed values, can be modified in the **Graph Details** panel.

---

#### **Diagnostics for One Sample**
This table displays diagnostic statistics used to compare the fit of different models for a single random sample from all generated samples.

- **Green Cells**: Indicate the model preferred by a given diagnostic.  
   - For example, if the green cell in the AIC column corresponds to the linear model row, it means AIC prefers the linear model as the best fit.

**Suggestion**: Experiment with the diagnostics to explore which ones are more "conservative" (favor simpler models) and which are more likely to support complex models.

---

#### **Model Comparison Across Samples**
This table summarizes diagnostics for all generated samples.  

- **Percentages**: Indicate how often a specific model was preferred across all samples.  
   - For example, if AIC for the linear regression model is 60%, it means that AIC identified the linear regression model as the best fit for 60% of the generated samples.

---

### **2. Assumptions Check**

Linear models have four key assumptions. In this tab, you can investigate violations of three of them:
- **Linearity**
- **Homoscedasticity**
- **Normality**

This is achieved by examining the **residual plots** for the models. These plots serve as valuable diagnostic tools to help you develop intuition about model behavior.

**Suggestion**:
- Observe how the residual plots change when the sample matches the true model.
  - **Residual Histogram**: What shape does it take?
  - **Residual Dependence Plot**: If you fit a linear model when the true model has a quadratic component, how does this plot look?
  - **Scale-Location Plot**: What happens if you violate the homoscedasticity assumption? (You can explore this in the **"What happens if you violate the assumptions?"** panel.)

---

### **3. Prediction**

This tab allows you to explore model predictions generated from the simulations.

- **Prediction Settings Panel**: Adjust the dependent variable ("x") values for which predictions ("y") are generated.
- **Alpha Level**: Change the width of the **Prediction Intervals** and **Confidence Intervals**.
- **Green Cells**: Highlighted if the "true value" is contained within the Confidence Interval or Prediction Interval.

**Additional Features**:
- Switch between displaying **Confidence Intervals** or **Prediction Intervals** by changing the **Interval Type** in the Prediction panel.


**Suggestions**
- Experiment with the settings in all tabs to observe how the models behave under different conditions.
- Explore how violating assumptions or changing parameters influences model fit, diagnostics, and predictions.

---

## **Where to Start?**

You can begin by exploring the following questions:

---

### **1. Model Diagnostics**
**Suggestion:**  
Experiment with the diagnostics to discover which ones are the most "conservative" and tend to prefer simpler models, and which diagnostics require less evidence to support more complex models.  

- **Tip**: You can simplify the model by setting the **True Model Quadratic Term** and **True Model Cubic Term** to `0` in the **Model Parameters** panel.

---

### **2. Residual Plots**
**Suggestion:**  
Observe how the residual plots behave when the sample matches the true model. Consider the following:

- **Residual Histogram**: What shape does it take when the model corresponds to the true data-generating process?
- **Residual Dependence Plot** (the second plot of the three): What happens if you fit a linear model when the true model includes a quadratic component?
- **Scale-Location Plot**: Explore what happens when the homoscedasticity assumption is violated.  
  - You can simulate this in the **"What happens if you violate the assumptions?"** panel.

---

### **3. Influence of Independent Variable Spread**
**Suggestion:**  
Experiment with how the spread of your independent variable affects the detectability of non-linearity.  

- Adjust the range and distribution of the independent variable in the **Model Parameters** panel to observe its impact on model diagnostics and residual plots.

---
--- 



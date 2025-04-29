## Corn Yield Explorer

Corn Yield Explorer is an interactive Shiny dashboard designed to help agronomists and data scientists visualize, 
explore, and predict cotton yield (Mg ha⁻¹) across multiple sites and seasons.

---

## Project Background

We began by assembling a **merged** dataset that brings together:

- **Weather summaries**  
  Seasonal averages of precipitation, solar radiation, temperature, vapor pressure, and more.  
- **Soil measurements**  
  pH, organic matter, nutrient levels, plus engineered features such as squared terms and nutrient ratios.  
- **Management details**  
  Days until harvest, previous crop, variety/hybrid, and geographic coordinates.

Early experimentation included two modeling approaches:

1. **Multilayer Perceptron (MLP)**  
   A feed-forward neural network implemented via **nnet**, tuned with cross-validation.  
2. **XGBoost**  
   A gradient-boosted tree model.  

After rigorous cross-validation and hyperparameter tuning, the **Multilayer Perceptron (MLP) model** emerged as our best performer 
in terms of both R² and RMSE. It therefore serves as the production engine in this app.

---

## App Structure

### 1. Overview  
- **Introduction text** (this section)  
- **Yield Histogram (Train)**  
  Displays the stratified 70/30 training-set distribution of yield.  
- **Correlation Panel**  
  Side-by-side scatterplots, histograms, and a full Pearson correlation matrix to reveal strong predictor relationships.

### 2. EDA  
- **Yield Histogram**  
  Adjustable bins for deeper inspection of yield distribution.  
- **Predictor vs. Yield**  
  Dynamic scatterplot (for continuous predictors) or boxplot (for categorical predictors).

### 3. Model VIP  
- **Variable Importance**  
  Explore the top N features driving XGBoost predictions.

### 4. Predictions  
- **Test-set performance**  
  R² and RMSE metrics on hold-out data.  
- **Observed vs. Predicted**  
  Interactive scatterplot to assess model fit and bias.

---

## Contributors

- Data wrangling & feature engineering — **Ashbin Bhat and Comfort Adegbenro**
- Model development & tuning — **Ashbin Bhat and Comfort Adegbenro** 
- Shiny UI/UX & visualization — **Ashbin Bhat and Comfort Adegbenro** 
- Project coordination & reporting — **Ashbin Bhat and Comfort Adegbenro** 

## Acknowledgments

This work benefited from code-refinement and guidance provided by **ChatGPT (OpenAI)**.

*April 2025 – Corn Yield Prediction (Machine Learning) Challenge, 2025 cohort*

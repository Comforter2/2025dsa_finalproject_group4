
## Maize Yield Prediction Using Machine Learning

**Corn Yield Explorer** is an end-to-end project demonstrating how modern data science workflows can be applied to 
predict maize yield (Mg ha⁻¹) across multiple sites and seasons. In addition to in-depth analysis and model development,
we have deployed an interactive Shiny dashboard for exploration and prediction.

---

## Project Overview

1. **Data Assembly**  
   - **Weather summaries**: Seasonal averages of precipitation, solar radiation, temperature, vapor pressure, etc.  
   - **Soil measurements**: pH, organic matter, nutrient levels, plus engineered features (squared terms, 
   nutrient ratios).  
   - **Management details**: Days until harvest, previous crop, hybrid, geographic coordinates.

2. **Feature Engineering & Preprocessing**  
   - Merged, cleaned, and factored data into a single “merged_factored” table.  
   - Created derived variables (e.g., DAP, nutrient indices).  
   - Handled missing values, zero-variance predictors, and normalization.

3. **Modeling Approaches**  
   - **Multilayer Perceptron (MLP)**  
     – Tuned number of hidden units and weight decay with 10-fold cross-validation.  
   - **XGBoost**  
     – Tuned trees, depth, learning rate, and minimum node size via grid search and racing methods.

4. **Model Selection**  
   - Evaluated on stratified 70/30 train/test splits using RMSE and R².  
   - Multilayer Perceptron (MLP) emerged as the top performer and serves as our production model.

---

## Results

- **XGBoost Test Performance**  
  - R²: *[0.50]*  
  - RMSE: *[5.75]*  
- **MLP (nnet) Test Performance**  
  - R²: *[0.50]*  
  - RMSE: *[1.88]*  

---

## Interactive Shiny App

Explore the data, diagnostics, and predictions yourself:

**Corn Yield Explorer Dashboard**  
https://your-shiny-app-url.example.com

---

## Contributors

- Data wrangling & feature engineering — **Ashbin Bhat and Comfort Adegbenro**
- Model development & tuning — **Ashbin Bhat and Comfort Adegbenro** 
- Shiny UI/UX & visualization — **Ashbin Bhat and Comfort Adegbenro** 
- Project coordination & reporting — **Ashbin Bhat and Comfort Adegbenro**   

---

## Acknowledgments

This work benefited from code-refinement and guidance provided by **ChatGPT (OpenAI)**.  
*April 2025 – Maize Yield Prediction Challenge, Data Science 2025 cohort*  
```
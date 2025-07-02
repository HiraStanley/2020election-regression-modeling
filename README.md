# Statistical Modeling of the 2020 U.S. Presidential Election Results

This project uses statistical modeling to explore the factors that influenced the percentage of votes for Joe Biden in the 2020 U.S. Presidential election at the county level. The project is divided into two parts: data cleaning and exploratory analysis (Part 1) and regression modeling (Part 2).

---

## 📂 Project Structure

- **Part 1:** Data Cleaning and Processing
- **Part 2:** Linear Regression Modeling and Feature Selection

---

## 📊 Part 1: Data Cleaning and Processing

### Data Overview
- **Rows:** 4,954 initially, reduced to 3,142 after cleaning
- **Columns:** Election results, demographic data, COVID-19 data, geographic features
- **Key Features:**
  - 2016 and 2020 election results
  - COVID cases and deaths
  - Demographic indicators (race, gender, income)
  - Turnout difference between 2016 and 2020

### Cleaning Process
- Removed **unassigned COVID cases** and **state-level catch-all entries**
- Fixed **county mismatches** due to data sources combining town and county data
- Removed **duplicate and town-level entries** for certain states (ME, MA, VT, NH, CT, AK, VA, RI)
- Handled missing data in 2016 and 2020 election records

### Feature Engineering
- `turnout_difference`: Difference in voter turnout between 2016 and 2020
- `case_pct`: COVID cases as a percentage of county population
- `death_pct`: COVID deaths as a percentage of COVID cases
- `men_pct`: Male population percentage
- `2016_county_winner` and `2020_county_winner`: Labels for potential classification tasks

---

## 🔎 Part 2: Linear Regression Model

### Problem
Political analysts seek to understand the key drivers of the 2020 election outcome at the county level using demographic, geographic, and COVID-19 data.

### Model Selection
- **Model Type:** Linear regression (percentage of vote share for Joe Biden)
- **Key Decision:** Chose regression over classification to understand magnitude of vote share changes.

### Final Model Formula
```r
percentage20_Joe_Biden ~ factor(state) * NonWhite + case_pct * Black + turnout_difference + Income + Men_pct + Transit

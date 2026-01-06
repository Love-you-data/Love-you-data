.

Hi I'm Priyadarshini.  
I work as a financial data analyst in Brightcom Group Ltd for the past 2.5 years. I have always been interested in how finance and programming works together. 
I have always tried to read and try to practice differet strategies of finance using finance. 
For ex, i would download stock data and try to apply different strategies in derivatives to arrive at profit.

Im currently learning Google Data analytics course ad is doing a case study to for analyzing the data from strat to end.

### Bellabeat Case Study 

The Bellabeat.Rmd is a -R markdown file showing the complete analysis of Bellabeat Case Study.   
Before making it into a r markdown file, i have created individual files for different ares and then later merged into it.  
The individual files are present in
Individual files of R folder.

<!---
Love-you-data/Love-you-data is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->

# Config-Driven KPI Engine

## Overview
This project implements a reusable, configuration-driven analytics engine
for calculating business KPIs from transactional data.

All KPI formulas, aggregation logic, and data quality rules are defined
externally via YAML configuration files, eliminating hard-coded logic.

## Key Features
- Config-driven KPI definitions and aggregation
- Data Quality validation with audit-friendly reporting
- Command-line interface for flexible execution
- Clear separation of data, logic, and configuration

## Folder Structure
config-driven-kpi-engine/
├── data/ # Input datasets
├── config/ # KPI definitions and aggregation rules
├── dq/ # Data quality rules
├── src/ # Core analytics engine
├── output/ # KPI output and data quality reports

## How to Run
```bash
pip install -r requirements.txt
python src/kpi_engine.py --data data/sample_data.csv --config config/kpi_config.yaml
```
## Output

kpi_output.csv: Aggregated KPI metrics

data_quality_report.txt: Data quality validation report

## Business Value

This approach mirrors real enterprise analytics systems where business logic
changes frequently and must remain auditable, reusable, and scalable.

- Record-level data quality validation including:
  - Null checks
  - Negative value checks
  - Strict data type enforcement
  - Business semantic rules (e.g., integer-only quantities)



## 🤖 Market-Regime-Driven Stock Options Strategy Agent

An AI-driven analytics system that detects market regimes using NIFTY index data and recommends stock options strategy profiles based on how such strategies historically performed under similar market conditions.

⚠️ This project is for educational and analytical purposes only.It does not provide trading advice or execute trades.


🔍 Problem Statement

Options traders often struggle with choosing the right strategy for the current market environment.
Using the wrong strategy in the wrong regime (e.g., selling options in high volatility) can lead to poor outcomes.

This project addresses that problem by:

Automatically identifying market regimes using machine learning

Mapping each regime to appropriate stock options strategies

Benchmarking strategy behavior against NIFTY buy-and-hold

Presenting insights in a clear, explainable UI

🧠 Key Idea

Market regime → Strategy suitability

Instead of predicting prices, this system answers:

“Given the current market environment, which stock option strategies have historically worked better?”





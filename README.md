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

🏗️ Architecture Overview

NIFTY Historical Data
        ↓
Feature Engineering
        ↓
ML-Based Regime Detection (Unsupervised)
        ↓
Market Regime Interpretation
        ↓
Stock Options Strategy Mapping
        ↓
Regime-Based Benchmarking vs NIFTY
        ↓
AI Insights + Confidence
        ↓
Streamlit UI


📊 Market Regimes Detected

Using unsupervised ML (KMeans), the system identifies distinct market regimes such as:

Bullish

Bullish–Volatile

Bearish

Crash / Stress

These regimes are inferred purely from NIFTY index behavior, which provides a clean, noise-reduced view of overall market conditions.

🧩 Strategy Modeling Approach (Important)

This project does not price options using Black–Scholes or Greeks.

Instead, it uses strategy behavior modeling:

Each option strategy (e.g., Long Call, Iron Condor) is represented by a proxy return profile

Proxy returns are derived from underlying index returns

This allows:

Clean comparison

Explainable logic

Regime-based performance analysis

This approach is commonly used in early-stage quant research and regime studies.

📈 Strategy Benchmarking

For each market regime, the system evaluates:

Average strategy return

Outperformance vs NIFTY buy-and-hold

Win rate (% days strategy beat NIFTY)

Example insight:

“In Bullish–Volatile regimes, Long Call–type stock option strategies outperformed NIFTY with a 66% win rate.”

🖥️ Interactive UI (Streamlit)

The Streamlit UI allows users to:

View current market regime

See recommended stock options strategy profiles

Understand why a strategy is suggested

Review historical regime-based insights

The stock symbol input is optional and intended only as an execution reference,
not as a driver of market regime detection.

⚖️ What This Project Is / Is Not
✅ This project IS

Market-regime driven

ML-based (unsupervised learning)

Explainable and benchmarked

Focused on stock options strategies

❌ This project is NOT

A stock picker

An options pricing engine

A trade execution system

A buy/sell signal generator

🛠️ Tech Stack

Python

Pandas / NumPy

scikit-learn

Yahoo Finance (data source)

Streamlit (UI)

🚀 How to Run

pip install -r requirements.txt
streamlit run ui/app.py

🧠 Key Takeaway

This project demonstrates how AI and ML can be used for market context awareness and strategy selection, rather than price prediction 




**Asymmetric Factor Volatility During COVID-19: Evidence for EGARCH over GARCH**

[![MIT License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
<!-- [![Python](https://img.shields.io/badge/Python-3776AB?logo=python&logoColor=white)](https://www.python.org/) -->

This paper documents a critical market inefficiency in factor-level volatility forecasting during systemic crises. 
Traditional symmetric GARCH(1,1) models, despite effectively capturing autocorrelation dynamics, systematically misspecify factor-level risk through two mechanisms: 
- (1) violation of normality assumptions creating extreme fat tails, and 
- (2) asymmetric shock responses

**Methodology** 
- Initial reflexion of the project based on EDHEC Risk Institute paper (Hasaj, M. & Sherer, B., 2021; "Covid-19 and Smart-Beta: A Case Study on theRole of Sectors”. EDHEC-Risk Institute Working Paper., pp. 1-35.)
- We analyse the behavior of factors volatility during the Covid-19 period across 374 trading days.
- Tested on MSCI US equity factors (Size, Value, Quality, Momentum, Minimum Volatility) and using S&P 500 as the market aggregate
- Used Pagano timeline taxonomy (2020) COVID-19 timeframe decomposition 
  
**Insights**:

- (1) all factors exhibit significant asymmetric volatility drifts (γ = 0.388-0.554, p < 0.05), with defensive factors showing stronger asymmetry than Value
- (2) GARCH systematically underpredicts tail volatility by 40-260 basis points during crisis peaks
- (3) EGARCH reduces this misspecification by 20% when forecasting the Momentum factor and show overconfidence in prediction when assessing market aggregate (S&P 500) with statistically significant out-of-sample improvements (p < 0.001)
- The finding that defensive factors do not escape asymmetric responses challenges conventional portfolio diversification wisdom
- The findings presented in this paper suggest practitioners consider alternative models to capture volatility such as EGARCH compared to standard Gaussian GARCH model

**Technologies**: R, RStudio, rugarch 
**Methods**: Econometrics (GARCH modelling) 
**Data**: MSCI factor indices (2019-2021, Refinitiv Eikon)

📄 [Published Paper (SSRN)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4677442) | 100+ downloads

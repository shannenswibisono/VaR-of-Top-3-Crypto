# Value at Risk of The Top 3 Cryptocurrencies
Hi everyone!˚ʚ♡ɞ˚  
This is a collaboration work of me and Vira during our third year of university. As the mathematic students, we tried to predict the Value at Risk (VaR) of the top 3 cryptocurrencies using 3 simulations using RStudio Code. Let's begin!   

## Background
This project was based on the post-information technology era that successfully driven new innovations in global-scale payment methods, such as cryptocurrency. The development of cryptocurrencies continues to grow rapidly each year, making them a preferred choice for many investors. However, predicting the prices of cryptocurrencies is challenging due to their dynamic volatility.   

Therefore, this project aims to analyze the Value at Risk (VaR) of three widely used cryptocurrencies: Bitcoin, Ethereum, and Tether. The VaR calculation process will be performed using the Monte Carlo, Historical, and Variance-Covariance methods.
    
## Data 
Data is selected from the time period starting from January 1, 2019, to November 27, 2023, downloaded from a credible historical stock data provider, namely https://finance.yahoo.com. This time frame is chosen to provide a broad overview of price fluctuations and market risks over the past several years, covering critical periods and significant changes in the cryptocurrency market.

Using the adjusted close data for each cryptocurrency obtained, we processes the data to calculate the return values for each cryptocurrency using arithmetic. To obtain the Value at Risk (VaR), we did tests various confidence levels: 90%, 95%, and 99%, for more accurate results.

## Assumption
- To use the three methods to calculate each Value at Risk (VaR), we assume that the data have normal distributions.
- We only use one day for the holding period, assuming that investors hold their assets only for a day.

## Result of Project
### Daily return of the 3 cryptocurrencies:
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/66c2f263-dd61-4365-847f-fdfaed65aa7e)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/961d8fdb-86a8-4c30-a0a6-cc9f68e0d01d)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/bbbee10a-e6b0-42c2-943c-c5e18334fc57)
### Value at Risk Using Monte Carlo Simulations
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/4c0b4173-8052-4113-b8fb-a6749deeee9e)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/5008d2cf-27d1-4b3d-b6d3-1b3c1b2b9bcd)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/fd0f4edf-3f67-470f-87b1-9ba65327efe8)
### Value at Risk Using Historical Method
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/3414815b-4b6f-4172-8a1c-539dd8c74fca)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/d8cd109e-08c7-4760-9d7a-4c0b11e1520d)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/3995b886-d79f-4b78-9fd0-550b44cc2d63)
### Value at Risk Using Variance-Covariance Method
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/2155cd55-89d3-434f-a76f-4052f3e24715)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/89e6f4b9-94ab-4ed4-bc2b-faf4dc5369ab)
![image](https://github.com/shannenswibisono/VaR-of-Top-3-Crypto/assets/155773383/a3ad2b32-aab0-43ef-92a1-0cc875c54432)

## Conclusion
In this study, three Value at Risk (VaR) calculation methods have been implemented for the cryptocurrency assets: Bitcoin, Ethereum, and Tether. 

The VaR values for Ethereum consistently exceed those for Bitcoin (the second-largest VaR) and Tether (the smallest VaR). This consistency across implemented methods suggests robust VaR results. Overall, the Historical VaR method produces higher VaR values compared to other methods. The study demonstrates that different significance levels can yield different VaR values but lead to a consistent conclusion. Higher confidence levels result in higher estimates of maximum loss.

The researcher observes that the VaR results align with descriptive statistical tests conducted before implementing the VaR methods. For instance, Ethereum, with the highest average daily return, also exhibits the highest VaR, followed by Bitcoin and Tether. This indicates that high returns are associated with higher risks.

## Things to Improve
- Assets do not follow a normal distribution, as evidenced by the non-normal distribution of daily return results calculated in descriptive statistical tests. Assuming that the data remains normally distributed can impact the resulting VaR, as overestimation or underestimation may occur. Therefore, it is necessary to transform the daily return data so that its distribution approximates normality before calculating VaR.
- Typically, investors rarely hold assets for just one day; it might be worth exploring longer time frames.
- Value at Risk considers assets individually; it would be beneficial to create a portfolio with assigned weights to assess the magnitude of risk more comprehensively.

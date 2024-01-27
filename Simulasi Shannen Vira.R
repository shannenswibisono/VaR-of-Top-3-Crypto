library(quantmod) #Dipake buat ambil data Yahoo Finance, dll
library(ggplot2)
# install.packages(c("dplyr", "knitr", "fBasics"))
library(fBasics) # Untuk deskriptif statistik
library(dplyr)
library(knitr) # Untuk membuat table 

# Plot hasil VaR
library(ggplot2)
library(gridExtra)
library(scales)  # Untuk menggunakan percent_format

#---------------------------Statistik Deskriptif--------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Bitcoin
# Langkah 1: Ambil data dari Yahoo Finance
# Symbol crypto yang ingin diambil datanya
bitcoin <- "BTC-USD"

# Tentukan rentang waktu (Dari 1 Januari 2019 - 27 November 2023)
start_date <- as.Date("2019-01-01")  # Tanggal awal
end_date <- as.Date("2023-11-27")    # Tanggal akhir

# Gunakan fungsi getSymbols untuk ambil data Bitcoin dari rentang waktu
getSymbols(bitcoin, src = "yahoo", from = start_date, to = end_date)

# Cari daily return dari adjusted close
daily_return_BTC = dailyReturn(Ad(get(bitcoin)), type = "arithmetic")
# Itung daily return dari adj close menggunakan rumus aritmatika

# Mengganti nama kolom daily return
colnames(daily_return_BTC)[1] <- "BTCDailyReturn"

# Menggabungkan BTC USD dengan daily return yang didapatkan dan menamakannya dengan bitcoin
bitcoin <- cbind(`BTC-USD`, daily_return_BTC)

# Menjadikan bitcoin menjadi data frame
bitcoin = as.data.frame(bitcoin)

# Cek data nya udah sesuai belum -> numerik
is_numeric <- is.numeric(bitcoin$BTCDailyReturn)
print(is_numeric) # True

# Memuat vektor tanggal dari rentang waktu yang diinginkan
date_vector = seq(start_date, end_date, by = "days")

# Tambahkan kolom "Date" dengan vektor tanggal yang dibuat
bitcoin$Date = date_vector

# Langkah 2: Membuat descriptive stats
attach(bitcoin) # Ambil data frame BTC
Min_BTC = min(bitcoin$BTCDailyReturn)
Max_BTC = max(bitcoin$BTCDailyReturn)
Mean_BTC = mean(bitcoin$BTCDailyReturn)
Std_BTC = sd(bitcoin$BTCDailyReturn)
Skewness_BTC = skewness(bitcoin$BTCDailyReturn)
Kurtosis_BTC = kurtosis(bitcoin$BTCDailyReturn)
Jangkauan_BTC = Max_BTC - Min_BTC
hasil_statistik_BTC = data.frame(
  Min_BTC,
  Max_BTC,
  Mean_BTC,
  Std_BTC,
  Skewness_BTC,
  Kurtosis_BTC,
  Jangkauan_BTC
) # Masukkan dalam 1 data frame
kable(hasil_statistik_BTC, caption = "Statistik Return Bitcoin") 
# Menampilkan tabel dari data frame

# Hasil histogram dan ggplot
hist(bitcoin$BTCDailyReturn)
str(bitcoin)
ggplot(bitcoin, aes(x = bitcoin$Date, y = bitcoin$BTCDailyReturn)) +
  geom_line(color = "blue", size = 2) +
  labs(title = "Daily Return Bitcoin", x = "Date", y = "Daily Return")

# Uji normalitas menggunakan KS
ks.test(bitcoin$BTCDailyReturn, "pnorm", mean = Mean_BTC, sd = Std_BTC)
# Menolak H0 (pvalue=1.121e-14) -> data tdk berdistribusi normal

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Ethereum
# Langkah 1: Ambil data dari Yahoo Finance
# Symbol crypto yang ingin diambil datanya
ethereum <- "ETH-USD"

# Tentukan rentang waktu (Dari 1 Januari 2019 - 27 November 2023)
start_date <- as.Date("2019-01-01")  # Tanggal awal
end_date <- as.Date("2023-11-27")    # Tanggal akhir

# Gunakan fungsi getSymbols untuk ambil data Ethereum dari rentang waktu
getSymbols(ethereum, src = "yahoo", from = start_date, to = end_date)

# Cari daily return dari adjusted close
daily_return_ETH = dailyReturn(Ad(get(ethereum)), type = "arithmetic") #Itung daily return dr adj close

# Mengganti nama kolom daily return
colnames(daily_return_ETH)[1] <- "ETHDailyReturn"

# Bikin data yg diambil dr Yahoo & daily return sebagai data frame yg terpisah
ethereum = cbind(`ETH-USD`,daily_return_ETH)

# Menjadikan ethereum menjadi data frame
ethereum = as.data.frame(ethereum)

# Tambahkan kolom "Date" ke dataframe Ethereum dengan vektor tanggal yang dibuat (di baris 40)
ethereum$Date = date_vector

# Langkah 2: Membuat descriptive stats
attach(ethereum) # Ambil data frame ETH

Min_ETH = min(ethereum$ETHDailyReturn)
Max_ETH = max(ethereum$ETHDailyReturn)
Mean_ETH = mean(ethereum$ETHDailyReturn)
Std_ETH = sd(ethereum$ETHDailyReturn)
Skewness_ETH = skewness(ethereum$ETHDailyReturn)
Kurtosis_ETH = kurtosis(ethereum$ETHDailyReturn)
Jangkauan_ETH = Max_ETH - Min_ETH
hasil_statistik_ETH = data.frame(
  Min_ETH,
  Max_ETH,
  Mean_ETH,
  Std_ETH,
  Skewness_ETH,
  Kurtosis_ETH,
  Jangkauan_ETH
) # Masukkan ke 1 data frame (Table)
kable(hasil_statistik_ETH, caption = "Statistik Return Ethereum") 
# Menampilkan tabel dari data frame

# Hasil histogram dan ggplot
hist(ethereum$ETHDailyReturn)
str(ethereum)
ggplot(ethereum, aes(x = ethereum$Date, y = ethereum$ETHDailyReturn)) +
  geom_line(color = "red", size = 2) +
  labs(title = "Daily Return Ethereum", x = "Date", y = "Daily Return")

# Uji normalitas menggunakan KS
ks.test(ethereum$ETHDailyReturn, "pnorm", mean = Mean_ETH, sd = Std_ETH)
# Menolak H0 (p-value = 1.237e-11) -> data tdk berdistribusi normal

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Tether
# Langkah 1: Ambil data dari Yahoo Finance
# Symbol crypto yang ingin diambil datanya
tether <- "USDT-USD"

# Tentukan rentang waktu (Dari 1 Januari 2019 - 27 November 2023)
start_date <- as.Date("2019-01-01")  # Tanggal awal
end_date <- as.Date("2023-11-27")    # Tanggal akhir

# Gunakan fungsi getSymbols untuk ambil data Tether dari rentang waktu
getSymbols(tether, src = "yahoo", from = start_date, to = end_date)

# Cari daily return dari adjusted close
daily_return_USDT = dailyReturn(Ad(get(tether)), type = "arithmetic") 

# Mengganti nama kolom daily return
colnames(daily_return_USDT)[1] <- "USDTDailyReturn"

# Bikin data yg diambil dr Yahoo & daily return sebagai data frame yg terpisah
tether = cbind(`USDT-USD`,daily_return_USDT)

# Menjadikan ethereum menjadi data frame
tether = as.data.frame(tether)

# Tambahkan kolom "Date" ke dataframe Tether dengan vektor tanggal yang dibuat (di baris 40)
tether$Date = date_vector

# Langkah 2: Membuat descriptive stats
attach(tether) #Ambil data frame USDT

Min_USDT = min(tether$USDTDailyReturn)
Max_USDT = max(tether$USDTDailyReturn)
Mean_USDT = mean(tether$USDTDailyReturn)
Std_USDT = sd(tether$USDTDailyReturn)
Skewness_USDT = skewness(tether$USDTDailyReturn)
Kurtosis_USDT = kurtosis(tether$USDTDailyReturn)
Jangkauan_USDT = Max_USDT - Min_USDT
hasil_statistik_USDT = data.frame(
  Min_USDT,
  Max_USDT,
  Mean_USDT,
  Std_USDT,
  Skewness_USDT,
  Kurtosis_USDT,
  Jangkauan_USDT
) # Masukkan ke 1 data frame (Table)
kable(hasil_statistik_USDT, caption = "Statistik Return Tether") 
# Menampilkan tabel dari data frame

# Histogram dan plot dari Tether
hist(tether$USDTDailyReturn)
ggplot(tether, aes(x = tether$Date, y = tether$USDTDailyReturn)) +
  geom_line(color = "purple", size = 2) +
  labs(title = "Daily Return Tether", x = "Date", y = "Daily Return")

# Uji normalitas menggunakan KS
ks.test(tether$USDTDailyReturn, "pnorm", mean = Mean_USDT, sd = Std_USDT, exact = TRUE)
# Menolak H0 (pvalue= 3.009e-14) -> data tdk berdistribusi normal


#---------------------------Monte Carlo Simulation------------------------------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Bitcoin
# Set the number of simulations and forecast period
num_simulations <- 100
forecast_period <- 100

# Extract the last observed price
last_price_BTC <- as.numeric(tail(bitcoin$BTC.USD.Adjusted, 1))

# Generate Monte Carlo simulations
simulations_BTC <- matrix(NA, nrow = forecast_period, ncol = num_simulations)

for (i in 1:num_simulations) {
  simulations_BTC[, i] <- cumprod(1 + rnorm(forecast_period, mean = Mean_BTC, sd = Std_BTC))
}

# Multiply the last observed price to the simulations generated
simulated_prices_BTC <- last_price_BTC * simulations_BTC

# Calculating the simulated returns from the simulated prices
simulated_returns_BTC <- matrix(NA, nrow = forecast_period - 1, ncol = num_simulations)

for (i in 1:num_simulations) {
  for (j in 2:forecast_period) {
    simulated_returns_BTC[j - 1, i] <- (simulated_prices_BTC[j, i] - simulated_prices_BTC[j - 1, i]) / simulated_prices_BTC[j - 1, i]
  }
}

# Plot the simulations
matplot(simulated_prices_BTC, type = "l", main = paste("Monte Carlo Simulation for Bitcoin"),
        xlab = "Forecast Period", ylab = "Price", ylim = c(min(simulated_prices_BTC), max(simulated_prices_BTC)))

# Calculate mean and standard deviation of simulated returns for Bitcoin
mean_return_BTC <- apply(simulated_returns_BTC, 2, mean)
std_dev_BTC <- apply(simulated_returns_BTC, 2, sd)

# Set the confidence levels
alpha_90 <- 0.1
alpha_95 <- 0.05
alpha_99 <- 0.01

# Calculate the Z values
Z_90 <- qnorm(1 - alpha_90)
Z_95 <- qnorm(1 - alpha_95)
Z_99 <- qnorm(1 - alpha_99)

# Calculate VaR for Bitcoin
VaR_BTC_90 <- mean_return_BTC - (Z_90 * std_dev_BTC)
VaR_BTC_95 <- mean_return_BTC - (Z_95 * std_dev_BTC)
VaR_BTC_99 <- mean_return_BTC - (Z_99 * std_dev_BTC)

# Calculate the average VaR for Bitcoin
average_VaR_BTC_90 <- mean(VaR_BTC_90)
average_VaR_BTC_95 <- mean(VaR_BTC_95)
average_VaR_BTC_99 <- mean(VaR_BTC_99)

# Display the average VaR for Bitcoin
print(paste("Average VaR at 90% Confidence Level for Bitcoin:", average_VaR_BTC_90))
print(paste("Average VaR at 95% Confidence Level for Bitcoin:", average_VaR_BTC_95))
print(paste("Average VaR at 99% Confidence Level for Bitcoin:", average_VaR_BTC_99))

# Data frame untuk plot VaR Monte Carlo
VaR_Monte_data1 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(average_VaR_BTC_90, average_VaR_BTC_95, average_VaR_BTC_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Monte_data1)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_BTC, sd = Std_BTC), 
                  color = "blue") +
    geom_vline(xintercept = Mean_BTC + VaR_Monte_data1$VaR[i], color = "red") +
    labs(title = paste("VaR Monte Carlo at", VaR_Monte_data1$Confidence_Level[i], "Confidence Level for Bitcoin =", percent(VaR_Monte_data1$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Ethereum
# Set the number of simulations and forecast period
num_simulations <- 100
forecast_period <- 100

# Extract the last observed price
last_price_ETH <- as.numeric(tail(ethereum$ETH.USD.Adjusted, 1))

# Generate Monte Carlo simulations
simulations_ETH <- matrix(NA, nrow = forecast_period, ncol = num_simulations)

for (i in 1:num_simulations) {
  simulations_ETH[, i] <- cumprod(1 + rnorm(forecast_period, mean = Mean_ETH, sd = Std_ETH))
}

# Multiply the last observed price to the simulations generated
simulated_prices_ETH <- last_price_ETH * simulations_ETH

# Calculating the simulated returns from the simulated prices
simulated_returns_ETH <- matrix(NA, nrow = forecast_period - 1, ncol = num_simulations)

for (i in 1:num_simulations) {
  for (j in 2:forecast_period) {
    simulated_returns_ETH[j - 1, i] <- (simulated_prices_ETH[j, i] - simulated_prices_ETH[j - 1, i]) / simulated_prices_ETH[j - 1, i]
  }
}

# Plot the simulations
matplot(simulated_prices_ETH, type = "l", main = paste("Monte Carlo Simulation for Ethereum"),
        xlab = "Forecast Period", ylab = "Price", ylim = c(min(simulated_prices_ETH), max(simulated_prices_ETH)))

# Calculate mean and standard deviation of simulated returns for Ethereum
mean_return_ETH <- apply(simulated_returns_ETH, 2, mean)
std_dev_ETH <- apply(simulated_returns_ETH, 2, sd)

# Set the confidence levels
alpha_90 <- 0.1
alpha_95 <- 0.05
alpha_99 <- 0.01

# Calculate the Z values
Z_90 <- qnorm(1 - alpha_90)
Z_95 <- qnorm(1 - alpha_95)
Z_99 <- qnorm(1 - alpha_99)

# Calculate VaR for Ethereum
VaR_ETH_90 <- mean_return_ETH - (Z_90 * std_dev_ETH)
VaR_ETH_95 <- mean_return_ETH - (Z_95 * std_dev_ETH)
VaR_ETH_99 <- mean_return_ETH - (Z_99 * std_dev_ETH)

# Calculate the average VaR for Ethereum
average_VaR_ETH_90 <- mean(VaR_ETH_90)
average_VaR_ETH_95 <- mean(VaR_ETH_95)
average_VaR_ETH_99 <- mean(VaR_ETH_99)

# Display the average VaR for Ethereum
print(paste("Average VaR at 90% Confidence Level for Ethereum:", average_VaR_ETH_90))
print(paste("Average VaR at 95% Confidence Level for Ethereum:", average_VaR_ETH_95))
print(paste("Average VaR at 99% Confidence Level for Ethereum:", average_VaR_ETH_99))

# Data frame untuk plot VaR Monte Carlo
VaR_Monte_data2 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(average_VaR_ETH_90, average_VaR_ETH_95, average_VaR_ETH_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Monte_data2)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_ETH, sd = Std_ETH), 
                  color = "blue") +
    geom_vline(xintercept = Mean_ETH + VaR_Monte_data2$VaR[i], color = "red") +
    labs(title = paste("VaR Monte Carlo at", VaR_Monte_data2$Confidence_Level[i], "Confidence Level for Ethereum =", percent(VaR_Monte_data2$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Tether
# Setting a variable with the cryptocurrency symbol in it
# Set the number of simulations and forecast period
num_simulations = 100
forecast_period = 100

# Extract the last observed price
last_price_USDT <- as.numeric(tail(tether$USDT.USD.Adjusted, 1))

# Generate Monte Carlo simulations
simulations_USDT <- matrix(NA, nrow = forecast_period, ncol = num_simulations)

for (i in 1:num_simulations) {
  simulations_USDT[, i] <- cumprod(1 + rnorm(forecast_period, mean = Mean_USDT, sd = Std_USDT))
}

# Multiply the last observed price to the simulations generated
simulated_prices_USDT <- last_price_USDT * simulations_USDT

# Calculating the simulated returns from the simulated prices
simulated_returns_USDT <- matrix(NA, nrow = forecast_period - 1, ncol = num_simulations)

for (i in 1:num_simulations) {
  for (j in 2:forecast_period) {
    simulated_returns_USDT[j - 1, i] <- (simulated_prices_USDT[j, i] - simulated_prices_USDT[j - 1, i]) / simulated_prices_USDT[j - 1, i]
  }
}

# Plot the simulations
matplot(simulated_prices_USDT, type = "l", main = paste("Monte Carlo Simulation for Tether"),
        xlab = "Forecast Period", ylab = "Price", ylim = c(min(simulated_prices_USDT), max(simulated_prices_USDT)))

# Calculate mean and standard deviation of simulated returns
mean_return_USDT <- apply(simulated_returns_USDT, 2, mean)
std_dev_USDT <- apply(simulated_returns_USDT, 2, sd)

# Set the confidence levels
alpha_90 <- 0.1
alpha_95 <- 0.05
alpha_99 <- 0.01

# Calculate the Z values
Z_90 <- qnorm(1 - alpha_90)
Z_95 <- qnorm(1 - alpha_95)
Z_99 <- qnorm(1 - alpha_99)

# Calculate VaR for each number of simulations
VaR_USDT_90 <- mean_return_USDT - (Z_90 * std_dev_USDT)
VaR_USDT_95 <- mean_return_USDT - (Z_95 * std_dev_USDT)
VaR_USDT_99 <- mean_return_USDT - (Z_99 * std_dev_USDT)

# Calculate the average VaR
average_VaR_USDT_90 <- mean(VaR_USDT_90)
average_VaR_USDT_95 <- mean(VaR_USDT_95)
average_VaR_USDT_99 <- mean(VaR_USDT_99)

# Display the average VaR
print(paste("Average VaR at 90% Confidence Level for Tether:", average_VaR_USDT_90))
print(paste("Average VaR at 95% Confidence Level for Tether:", average_VaR_USDT_95))
print(paste("Average VaR at 99% Confidence Level for Tether:", average_VaR_USDT_99))

# Data frame untuk plot VaR Monte Carlo
VaR_Monte_data3 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(average_VaR_USDT_90, average_VaR_USDT_95, average_VaR_USDT_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Monte_data3)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_USDT, sd = Std_USDT), 
                  color = "blue") +
    geom_vline(xintercept = Mean_USDT + VaR_Monte_data3$VaR[i], color = "red") +
    labs(title = paste("VaR Monte Carlo at", VaR_Monte_data3$Confidence_Level[i], "Confidence Level for Tether =", percent(VaR_Monte_data3$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

#---------------------------Historical Simulation-------------------------------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Bitcoin
# Setting holding period
holding_period <- 1

# Calculating VaR using historical method
historical_var_BTC_90 <- quantile(bitcoin$BTCDailyReturn, 1 - 0.90) # 90% CI
historical_var_BTC_95 <- quantile(bitcoin$BTCDailyReturn, 1 - 0.95) # 95% CI
historical_var_BTC_99 <- quantile(bitcoin$BTCDailyReturn, 1 - 0.99) # 99% CI

# Calculate VaR after the holding period
var_hist_BTC_90 <- historical_var_BTC_90 * sqrt(holding_period)
var_hist_BTC_95 <- historical_var_BTC_95 * sqrt(holding_period)
var_hist_BTC_99 <- historical_var_BTC_99 * sqrt(holding_period)

# Display the average VaR
print(paste("VaR at 90% Confidence Level for Bitcoin:", var_hist_BTC_90))
print(paste("VaR at 95% Confidence Level for Bitcoin:", var_hist_BTC_95))
print(paste("VaR at 99% Confidence Level for Bitcoin:", var_hist_BTC_99))

# Data frame untuk plot VaR Monte Carlo for plot
VaR_Hist_data1 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(var_hist_BTC_90, var_hist_BTC_95, var_hist_BTC_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Hist_data1)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_BTC, sd = Std_BTC), 
                  color = "blue") +
    geom_vline(xintercept = Mean_BTC + VaR_Hist_data1$VaR[i], color = "red") +
    labs(title = paste("VaR Historical at", VaR_Hist_data1$Confidence_Level[i], "Confidence Level for Bitcoin =", percent(VaR_Hist_data1$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Ethereum
# Setting holding period
holding_period <- 1

# Calculating VaR using historical method
historical_var_ETH_90 <- quantile(ethereum$ETHDailyReturn, 1 - 0.90) # 90% CI
historical_var_ETH_95 <- quantile(ethereum$ETHDailyReturn, 1 - 0.95) # 95% CI
historical_var_ETH_99 <- quantile(ethereum$ETHDailyReturn, 1 - 0.99) # 99% CI

# Calculate VaR after the holding period
var_hist_ETH_90 <- historical_var_ETH_90 * sqrt(holding_period)
var_hist_ETH_95 <- historical_var_ETH_95 * sqrt(holding_period)
var_hist_ETH_99 <- historical_var_ETH_99 * sqrt(holding_period)

# Display the average VaR
print(paste("VaR at 90% Confidence Level for Ethereum:", var_hist_ETH_90))
print(paste("VaR at 95% Confidence Level for Ethereum:", var_hist_ETH_95))
print(paste("VaR at 99% Confidence Level for Ethereum:", var_hist_ETH_99))

# Data frame untuk plot VaR Historical for plot
VaR_Hist_data2 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(var_hist_ETH_90, var_hist_ETH_95, var_hist_ETH_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Hist_data2)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_ETH, sd = Std_ETH), 
                  color = "blue") +
    geom_vline(xintercept = Mean_ETH + VaR_Hist_data2$VaR[i], color = "red") +
    labs(title = paste("VaR Historical at", VaR_Hist_data2$Confidence_Level[i], "Confidence Level for Ethereum =", percent(VaR_Hist_data2$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Tether
# Setting holding period
holding_period <- 1

# Calculating VaR using historical method
historical_var_USDT_90 <- quantile(tether$USDTDailyReturn, 1 - 0.90) # 90% CI
historical_var_USDT_95 <- quantile(tether$USDTDailyReturn, 1 - 0.95) # 95% CI
historical_var_USDT_99 <- quantile(tether$USDTDailyReturn, 1 - 0.99) # 99% CI

# Calculate VaR after the holding period
var_hist_USDT_90 <- historical_var_USDT_90 * sqrt(holding_period)
var_hist_USDT_95 <- historical_var_USDT_95 * sqrt(holding_period)
var_hist_USDT_99 <- historical_var_USDT_99 * sqrt(holding_period)

# Display the average VaR
print(paste("VaR at 90% Confidence Level for Tether:", var_hist_USDT_90))
print(paste("VaR at 95% Confidence Level for Tether:", var_hist_USDT_95))
print(paste("VaR at 99% Confidence Level for Tether:", var_hist_USDT_99))

# Data frame untuk plot VaR Historical for plot
VaR_Hist_data3 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(var_hist_USDT_90, var_hist_USDT_95, var_hist_USDT_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Hist_data3)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_USDT, sd = Std_USDT), 
                  color = "blue") +
    geom_vline(xintercept = Mean_USDT + VaR_Hist_data3$VaR[i], color = "red") +
    labs(title = paste("VaR Historial at", VaR_Hist_data3$Confidence_Level[i], "Confidence Level for Tether =", percent(VaR_Hist_data3$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

#----------------------------Var-Covar Simulation-------------------------------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Bitcoin
# Memegang saham (Holding Period) selama 1 hari.
hp = 1

# Menghitung VaR (Parametric, Variance-Covariance) dengan alfa yang berbeda2
# Note: Std = Volatility
VaR_Par_BTC_90 = Mean_BTC + qnorm(alpha_90)*Std_BTC*sqrt(hp)
VaR_Par_BTC_95 = Mean_BTC + qnorm(alpha_95)*Std_BTC*sqrt(hp)
VaR_Par_BTC_99 = Mean_BTC + qnorm(alpha_99)*Std_BTC*sqrt(hp)

# Display the average VaR
print(paste("VaR at 90% Confidence Level for Bitcoin:", VaR_Par_BTC_90))
print(paste("VaR at 95% Confidence Level for Bitcoin:", VaR_Par_BTC_95))
print(paste("VaR at 99% Confidence Level for Bitcoin:", VaR_Par_BTC_99))

# Data frame untuk plot VaR Historical for plot
VaR_Par_data1 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(VaR_Par_BTC_90, VaR_Par_BTC_95, VaR_Par_BTC_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Par_data1)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_BTC, sd = Std_BTC), 
                  color = "blue") +
    geom_vline(xintercept = Mean_BTC + VaR_Par_data1$VaR[i], color = "red") +
    labs(title = paste("VaR Parametric at", VaR_Par_data1$Confidence_Level[i], "Confidence Level for Bitcoin =", percent(VaR_Par_data1$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Ethereum
# Memegang saham (Holding Period) selama 1 hari.
hp = 1

# Menghitung VaR (Parametric, Variance-Covariance) dengan alfa yang berbeda2
# Note: Std = Volatility
VaR_Par_ETH_90 = Mean_ETH + qnorm(alpha_90)*Std_ETH*sqrt(hp)
VaR_Par_ETH_95 = Mean_ETH + qnorm(alpha_95)*Std_ETH*sqrt(hp)
VaR_Par_ETH_99 = Mean_ETH + qnorm(alpha_99)*Std_ETH*sqrt(hp)

# Display the average VaR
print(paste("VaR at 90% Confidence Level for Ethereum:", VaR_Par_ETH_90))
print(paste("VaR at 95% Confidence Level for Ethereum:", VaR_Par_ETH_95))
print(paste("VaR at 99% Confidence Level for Ethereum:", VaR_Par_ETH_99))

# Data frame untuk plot VaR Historical for plot
VaR_Par_data2 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(VaR_Par_ETH_90, VaR_Par_ETH_95, VaR_Par_ETH_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Par_data2)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_ETH, sd = Std_ETH), 
                  color = "blue") +
    geom_vline(xintercept = Mean_ETH + VaR_Par_data2$VaR[i], color = "red") +
    labs(title = paste("VaR Parametric at", VaR_Par_data2$Confidence_Level[i], "Confidence Level for Ethereum =", percent(VaR_Par_data2$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Tether
# Memegang saham (Holding Period) selama 1 hari.
hp = 1

# Menghitung VaR (Parametric, Variance-Covariance) dengan alfa yang berbeda2
# Note: Std = Volatility
VaR_Par_USDT_90 = Mean_USDT + qnorm(alpha_90)*Std_USDT*sqrt(hp)
VaR_Par_USDT_95 = Mean_USDT + qnorm(alpha_95)*Std_USDT*sqrt(hp)
VaR_Par_USDT_99 = Mean_USDT + qnorm(alpha_99)*Std_USDT*sqrt(hp)

# Display the average VaR
print(paste("VaR at 90% Confidence Level for Tether:", VaR_Par_USDT_90))
print(paste("VaR at 95% Confidence Level for Tether:", VaR_Par_USDT_95))
print(paste("VaR at 99% Confidence Level for Tether:", VaR_Par_USDT_99))

# Menyimpan VaR dan alpha dalam satu dataframe
VaR_Par_data <- data.frame(
  Confidence_Level = c(90, 95, 99),
  VaR = c(VaR_Par_USDT_90, VaR_Par_USDT_95, VaR_Par_USDT_99)
)

# Data frame untuk plot VaR Parametric
VaR_Par_data3 <- data.frame(
  Confidence_Level = c("90%", "95%", "99%"),
  VaR = c(VaR_Par_USDT_90, VaR_Par_USDT_95, VaR_Par_USDT_99)
)

# List untuk menyimpan plot
g <- list()

# Loop untuk setiap level kepercayaan
for (i in 1:nrow(VaR_Par_data3)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = Mean_USDT, sd = Std_USDT), 
                  color = "blue") +
    geom_vline(xintercept = Mean_USDT + VaR_Par_data3$VaR[i], color = "red") +
    labs(title = paste("VaR Parametric at", VaR_Par_data3$Confidence_Level[i], "Confidence Level for Tether =", percent(VaR_Par_data3$VaR[i], accuracy = 0.01)),
         x = "Return",
         y = "Density") +
    scale_x_continuous(limits = c(-0.2, 0.2))
}

# Menggabungkan dan menampilkan plot
do.call("grid.arrange", c(g, nrow = 3))

# ============================================================================
#        TATA CONSULTANCY SERVICES (TCS) - COMPLETE R PROJECT
#        Company: Tata Consultancy Services Limited (TCS.NS)
#        All Parts Combined in One Script
# ============================================================================
# CONTENTS:
#   PART 01A : Financial Data Acquisition & Handling (CSV, Excel, API, Cleaning)
#   PART 01B : Data Visualisation (Line, Bar, Financial Charts using ggplot2)
#   PART 01C : Basic Time Series Analysis (ARIMA, Trends, Decomposition)
#   PART 02  : Algorithmic Trading
# ============================================================================
# ===========================================================================
#                     INSTALL & LOAD ALL PACKAGES
# ===========================================================================
packages <- c(
  "quantmod", "tidyverse", "ggplot2", "readxl", "writexl",
  "zoo", "lubridate", "scales", "gridExtra", "reshape2",
  "forecast", "tseries", "urca",
  "TTR", "PerformanceAnalytics", "janitor"
)
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
cat("\n=== All packages loaded successfully ===\n\n")
# ===========================================================================
#  PART 01A: FINANCIAL DATA ACQUISITION & HANDLING
# ===========================================================================
cat("############################################################\n")
cat("#  PART 01A: FINANCIAL DATA ACQUISITION & HANDLING         #\n")
cat("############################################################\n\n")
# --- 1. Download Data from Yahoo Finance API ---
cat("--- Downloading TCS data from Yahoo Finance API ---\n")
symbol     <- "TCS.NS"
start_date <- Sys.Date() - (365 * 10)
end_date   <- Sys.Date()
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
# Convert xts to data frame
tcs_xts <- get(symbol)
tcs_df  <- data.frame(Date = index(tcs_xts), coredata(tcs_xts))
# Clean column names (remove "TCS.NS." prefix)
colnames(tcs_df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
cat("Data downloaded successfully!\n")
cat("Date Range :", as.character(min(tcs_df$Date)), "to", as.character(max(tcs_df$Date)), "\n")
cat("Total Rows :", nrow(tcs_df), "\n")
cat("Total Cols :", ncol(tcs_df), "\n\n")
# --- 2. View Data Structure ---
cat("--- Data Structure ---\n")
str(tcs_df)
cat("\n--- First 6 Rows ---\n")
print(head(tcs_df))
cat("\n--- Last 6 Rows ---\n")
print(tail(tcs_df))
cat("\n--- Summary Statistics ---\n")
print(summary(tcs_df))
# --- 3. Save Data to CSV ---
csv_path <- "TCS_Stock_Data.csv"
write.csv(tcs_df, file = csv_path, row.names = FALSE)
cat("\nData saved to CSV:", csv_path, "\n")
# --- 4. Save Data to Excel ---
excel_path <- "TCS_Stock_Data.xlsx"
write_xlsx(tcs_df, path = excel_path)
cat("Data saved to Excel:", excel_path, "\n")
# --- 5. Read Data Back from CSV ---
cat("\n--- Reading Data from CSV ---\n")
tcs_csv <- read.csv(csv_path, stringsAsFactors = FALSE)
tcs_csv$Date <- as.Date(tcs_csv$Date)
cat("CSV rows:", nrow(tcs_csv), "| Cols:", ncol(tcs_csv), "\n")
print(head(tcs_csv, 3))
# --- 6. Read Data Back from Excel ---
cat("\n--- Reading Data from Excel ---\n")
tcs_excel <- read_excel(excel_path)
tcs_excel$Date <- as.Date(tcs_excel$Date)
cat("Excel rows:", nrow(tcs_excel), "| Cols:", ncol(tcs_excel), "\n")
print(head(tcs_excel, 3))
# --- 7. Data Cleaning ---
cat("\n\n========== DATA CLEANING ==========\n")
# 7a. Check for missing values
cat("\n--- Missing Values per Column ---\n")
missing_counts <- colSums(is.na(tcs_df))
print(missing_counts)
cat("Total missing values:", sum(missing_counts), "\n")
# 7b. Handle missing values using forward fill (na.locf)
tcs_clean <- tcs_df
tcs_clean$Open     <- na.locf(tcs_clean$Open, na.rm = FALSE)
tcs_clean$High     <- na.locf(tcs_clean$High, na.rm = FALSE)
tcs_clean$Low      <- na.locf(tcs_clean$Low, na.rm = FALSE)
tcs_clean$Close    <- na.locf(tcs_clean$Close, na.rm = FALSE)
tcs_clean$Volume   <- na.locf(tcs_clean$Volume, na.rm = FALSE)
tcs_clean$Adjusted <- na.locf(tcs_clean$Adjusted, na.rm = FALSE)
# Remove any remaining NAs
tcs_clean <- na.omit(tcs_clean)
cat("\nAfter cleaning - Missing values:", sum(is.na(tcs_clean)), "\n")
cat("After cleaning - Total rows:", nrow(tcs_clean), "\n")
# 7c. Check for duplicate dates
cat("\n--- Duplicate Dates Check ---\n")
dup_count <- sum(duplicated(tcs_clean$Date))
cat("Duplicate dates found:", dup_count, "\n")
if (dup_count > 0) {
  tcs_clean <- tcs_clean[!duplicated(tcs_clean$Date), ]
  cat("Duplicates removed. Rows remaining:", nrow(tcs_clean), "\n")
}
# 7d. Check data types
cat("\n--- Data Types ---\n")
print(sapply(tcs_clean, class))
# 7e. Check for negative prices (anomalies)
cat("\n--- Anomaly Check (Negative Prices) ---\n")
cat("Negative Open prices:", sum(tcs_clean$Open < 0, na.rm = TRUE), "\n")
cat("Negative Close prices:", sum(tcs_clean$Close < 0, na.rm = TRUE), "\n")
cat("Negative Volume:", sum(tcs_clean$Volume < 0, na.rm = TRUE), "\n")
# 7f. Sort by date
tcs_clean <- tcs_clean %>% arrange(Date)
# 7g. Add derived columns
tcs_clean <- tcs_clean %>%
  mutate(
    Daily_Return     = (Close - lag(Close)) / lag(Close) * 100,
    Daily_Return_Pct = Daily_Return,
    Log_Return       = log(Close / lag(Close)) * 100,
    Price_Range      = High - Low,
    Year             = year(Date),
    Month            = month(Date, label = TRUE),
    Day_of_Week      = wday(Date, label = TRUE),
    Quarter          = quarter(Date)
  )
cat("\n--- Derived Columns Added ---\n")
cat("New columns: Daily_Return, Log_Return, Price_Range, Year, Month, Day_of_Week, Quarter\n")
print(head(tcs_clean))
# 7h. Save cleaned data
write.csv(tcs_clean, file = "TCS_Clean_Data.csv", row.names = FALSE)
write_xlsx(tcs_clean, path = "TCS_Clean_Data.xlsx")
cat("\nCleaned data saved to CSV and Excel.\n")
cat("\n========== PART 01A COMPLETE ==========\n\n")
# ===========================================================================
#  PART 01B: DATA VISUALISATION (ggplot2)
# ===========================================================================
cat("############################################################\n")
cat("#  PART 01B: DATA VISUALISATION (ggplot2)                  #\n")
cat("############################################################\n\n")
# Use cleaned data going forward
tcs <- tcs_clean
# Custom theme for all plots (Premium Slate/Tata Blue Palette for TCS)
theme_tcs <- theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5, color = "#111827"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title    = element_text(size = 11, face = "bold", color = "#1E293B"),
    axis.text     = element_text(size = 9, color = "#475569"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey93")
  )
# ---- CHART 1: Closing Price Line Chart ----
cat("--- Chart 1: Closing Price Line Chart ---\n")
p1 <- ggplot(tcs, aes(x = Date, y = Close)) +
  geom_line(color = "#004B87", linewidth = 0.6) + # TCS Deep Blue
  labs(
    title    = "TCS - Closing Price Over Time",
    subtitle = paste(min(tcs$Date), "to", max(tcs$Date)),
    x = "Date", y = "Closing Price (INR)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)
ggsave("TCS_Chart01_Closing_Price_Line.png", p1, width = 12, height = 6, dpi = 300)
# ---- CHART 2: OHLC Line Chart ----
cat("--- Chart 2: OHLC Line Chart ---\n")
ohlc_long <- tcs %>%
  select(Date, Open, High, Low, Close) %>%
  pivot_longer(cols = c(Open, High, Low, Close), names_to = "Price_Type", values_to = "Price")
p2 <- ggplot(ohlc_long, aes(x = Date, y = Price, color = Price_Type)) +
  geom_line(linewidth = 0.4, alpha = 0.75) +
  scale_color_manual(values = c("Open" = "#10B981", "High" = "#3B82F6",
                                "Low" = "#EF4444", "Close" = "#004B87")) +
  labs(
    title = "TCS - OHLC Prices",
    x = "Date", y = "Price (INR)", color = "Price Type"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)
ggsave("TCS_Chart02_OHLC_Lines.png", p2, width = 12, height = 6, dpi = 300)
# ---- CHART 3: Monthly Volume Bar Plot ----
cat("--- Chart 3: Monthly Volume Bar Plot ---\n")
monthly_vol <- tcs %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Avg_Volume = mean(Volume, na.rm = TRUE), .groups = "drop")
p3 <- ggplot(monthly_vol, aes(x = YearMonth, y = Avg_Volume)) +
  geom_bar(stat = "identity", fill = "#334155", alpha = 0.85) + # Slate Blue-Gray
  labs(
    title = "TCS - Average Monthly Trading Volume",
    x = "Month", y = "Average Volume"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)
ggsave("TCS_Chart03_Monthly_Volume_Bar.png", p3, width = 12, height = 6, dpi = 300)
# ---- CHART 4: Yearly Average Closing Price ----
cat("--- Chart 4: Yearly Average Closing Price ---\n")
yearly_avg <- tcs %>%
  group_by(Year) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE), .groups = "drop")
p4 <- ggplot(yearly_avg, aes(x = factor(Year), y = Avg_Close)) +
  geom_bar(stat = "identity", fill = "#004B87", alpha = 0.8) +
  geom_text(aes(label = round(Avg_Close, 0)), vjust = -0.5, size = 3.5, fontface = "bold", color = "#1E293B") +
  labs(
    title = "TCS - Yearly Average Closing Price",
    x = "Year", y = "Average Closing Price (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_tcs
print(p4)
ggsave("TCS_Chart04_Yearly_Avg_Close_Bar.png", p4, width = 10, height = 6, dpi = 300)
# ---- CHART 5: Daily Returns ----
cat("--- Chart 5: Daily Returns Line Chart ---\n")
p5 <- ggplot(tcs, aes(x = Date, y = Daily_Return)) +
  geom_line(color = "#005691", linewidth = 0.3, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "TCS - Daily Returns (%)",
    x = "Date", y = "Daily Return (%)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p5)
ggsave("TCS_Chart05_Daily_Returns.png", p5, width = 12, height = 6, dpi = 300)
# ---- CHART 6: Returns Distribution Histogram ----
cat("--- Chart 6: Returns Distribution Histogram ---\n")
p6 <- ggplot(tcs %>% filter(!is.na(Daily_Return)), aes(x = Daily_Return)) +
  geom_histogram(bins = 80, fill = "#005691", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#1E293B", linewidth = 0.8) +
  labs(
    title = "TCS - Distribution of Daily Returns",
    x = "Daily Return (%)", y = "Frequency"
  ) +
  theme_tcs
print(p6)
ggsave("TCS_Chart06_Returns_Histogram.png", p6, width = 10, height = 6, dpi = 300)
# ---- CHART 7: Moving Averages (50 & 200 Day) ----
cat("--- Chart 7: Moving Averages ---\n")
tcs <- tcs %>%
  arrange(Date) %>%
  mutate(
    MA_50  = rollmean(Close, k = 50, fill = NA, align = "right"),
    MA_200 = rollmean(Close, k = 200, fill = NA, align = "right")
  )
p7 <- ggplot(tcs, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.4) +
  geom_line(aes(y = MA_50, color = "50-Day MA"), linewidth = 0.7) +
  geom_line(aes(y = MA_200, color = "200-Day MA"), linewidth = 0.7) +
  scale_color_manual(values = c("Close" = "grey60", "50-Day MA" = "#00A4E4", "200-Day MA" = "#004B87")) +
  labs(
    title = "TCS - 50-Day & 200-Day Moving Averages",
    x = "Date", y = "Price (INR)", color = "Legend"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7)
ggsave("TCS_Chart07_Moving_Averages.png", p7, width = 12, height = 6, dpi = 300)
# ---- CHART 8: Candlestick Chart (Last 90 Days) ----
cat("--- Chart 8: Candlestick Chart ---\n")
recent <- tcs %>% tail(90) %>%
  mutate(Direction = ifelse(Close >= Open, "Up", "Down"))
p8 <- ggplot(recent) +
  geom_segment(aes(x = Date, xend = Date, y = Low, yend = High, color = Direction), linewidth = 0.4) +
  geom_rect(aes(xmin = Date - 0.3, xmax = Date + 0.3,
                ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                fill = Direction), color = NA) +
  scale_fill_manual(values = c("Up" = "#10B981", "Down" = "#EF4444")) +
  scale_color_manual(values = c("Up" = "#10B981", "Down" = "#EF4444")) +
  labs(
    title = "TCS - Candlestick Chart (Last 90 Trading Days)",
    x = "Date", y = "Price (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_tcs
print(p8)
ggsave("TCS_Chart08_Candlestick.png", p8, width = 12, height = 6, dpi = 300)
# ---- CHART 9: Monthly Returns Box Plot ----
cat("--- Chart 9: Monthly Returns Box Plot ---\n")
p9 <- ggplot(tcs %>% filter(!is.na(Daily_Return)), aes(x = Month, y = Daily_Return, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "TCS - Monthly Distribution of Daily Returns",
    x = "Month", y = "Daily Return (%)"
  ) +
  theme_tcs +
  theme(legend.position = "none")
print(p9)
ggsave("TCS_Chart09_Monthly_Boxplot.png", p9, width = 10, height = 6, dpi = 300)
# ---- CHART 10: Price and Volume Combined ----
cat("--- Chart 10: Price and Volume Combined ---\n")
scale_factor <- max(tcs$Close, na.rm = TRUE) / max(tcs$Volume, na.rm = TRUE)
p10 <- ggplot(tcs, aes(x = Date)) +
  geom_bar(aes(y = Volume * scale_factor), stat = "identity", fill = "#E2E8F0", alpha = 0.5) +
  geom_line(aes(y = Close), color = "#004B87", linewidth = 0.5) +
  scale_y_continuous(
    name = "Closing Price (INR)", labels = comma,
    sec.axis = sec_axis(~ . / scale_factor, name = "Volume", labels = comma)
  ) +
  labs(title = "TCS - Price vs Volume", x = "Date") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p10)
ggsave("TCS_Chart10_Price_Volume.png", p10, width = 12, height = 6, dpi = 300)
cat("\n========== PART 01B COMPLETE: 10 Charts Saved ==========\n\n")
# ===========================================================================
#  PART 01C: BASIC TIME SERIES ANALYSIS
# ===========================================================================
cat("############################################################\n")
cat("#  PART 01C: BASIC TIME SERIES ANALYSIS                    #\n")
cat("############################################################\n\n")
# --- Monthly Average Data ---
monthly_data <- tcs %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Avg_Close  = mean(Close, na.rm = TRUE),
    Avg_Volume = mean(Volume, na.rm = TRUE),
    .groups    = "drop"
  )
# ---- TREND ANALYSIS ----
cat("========== TREND ANALYSIS ==========\n\n")
monthly_data$Time_Index <- 1:nrow(monthly_data)
trend_model <- lm(Avg_Close ~ Time_Index, data = monthly_data)
cat("--- Linear Trend Model ---\n")
print(summary(trend_model))
cat("\nTrend Slope (per month):", coef(trend_model)[2], "INR\n")
cat("R-squared:", summary(trend_model)$r.squared, "\n\n")
p_trend <- ggplot(monthly_data, aes(x = YearMonth, y = Avg_Close)) +
  geom_line(color = "#004B87", linewidth = 0.6) +
  geom_smooth(method = "lm", color = "#334155", linetype = "dashed", se = TRUE, alpha = 0.2) +
  labs(
    title = "TCS - Long-term Price Trend",
    subtitle = "Monthly Average Closing Price with Linear Trend",
    x = "Date", y = "Average Close (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_trend)
ggsave("TS_TCS_Chart01_Trend_Analysis.png", p_trend, width = 12, height = 6, dpi = 300)
# ---- SEASONAL DECOMPOSITION ----
cat("========== SEASONAL DECOMPOSITION ==========\n\n")
close_ts <- ts(monthly_data$Avg_Close,
               start = c(year(min(monthly_data$YearMonth)), month(min(monthly_data$YearMonth))),
               frequency = 12)
# Additive Decomposition
decomp_add <- decompose(close_ts, type = "additive")
png("TS_TCS_Chart02_Decomposition_Additive.png", width = 1200, height = 800, res = 150)
plot(decomp_add, col = "#004B87")
title(main = "TCS - Additive Seasonal Decomposition", col.main = "black")
dev.off()
cat("Additive decomposition saved.\n")
# Multiplicative Decomposition
decomp_mult <- decompose(close_ts, type = "multiplicative")
png("TS_TCS_Chart03_Decomposition_Multiplicative.png", width = 1200, height = 800, res = 150)
plot(decomp_mult, col = "#005691")
title(main = "TCS - Multiplicative Seasonal Decomposition", col.main = "black")
dev.off()
cat("Multiplicative decomposition saved.\n")
# STL Decomposition
stl_decomp <- stl(close_ts, s.window = "periodic")
png("TS_TCS_Chart04_STL_Decomposition.png", width = 1200, height = 800, res = 150)
plot(stl_decomp, col = "#10B981")
title(main = "TCS - STL Decomposition", col.main = "black")
dev.off()
cat("STL decomposition saved.\n")
# Seasonal Component Bar Plot
seasonal_vals <- data.frame(
  Month    = factor(month.abb, levels = month.abb),
  Seasonal = as.numeric(decomp_add$figure)
)
p_seasonal <- ggplot(seasonal_vals, aes(x = Month, y = Seasonal, fill = Seasonal > 0)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#10B981", "FALSE" = "#EF4444")) +
  labs(title = "TCS - Seasonal Component (Monthly)", x = "Month", y = "Seasonal Effect") +
  theme_tcs + theme(legend.position = "none")
print(p_seasonal)
ggsave("TS_TCS_Chart05_Seasonal_Component.png", p_seasonal, width = 10, height = 6, dpi = 300)
# ---- STATIONARITY TESTING ----
cat("\n========== STATIONARITY TESTING ==========\n\n")
cat("--- ADF Test: Raw Closing Price ---\n")
adf_raw <- adf.test(na.omit(close_ts))
print(adf_raw)
cat("Result:", ifelse(adf_raw$p.value < 0.05, "STATIONARY", "NON-STATIONARY"), "\n\n")
diff_close <- diff(close_ts)
cat("--- ADF Test: First Differenced ---\n")
adf_diff <- adf.test(na.omit(diff_close))
print(adf_diff)
cat("Result:", ifelse(adf_diff$p.value < 0.05, "STATIONARY", "NON-STATIONARY"), "\n\n")
png("TS_TCS_Chart06_Stationarity.png", width = 1200, height = 800, res = 150)
par(mfrow = c(2, 1))
plot(close_ts, main = "Original Monthly Close Price", ylab = "Price (INR)", col = "#004B87")
plot(diff_close, main = "First Differenced Close Price", ylab = "Differenced", col = "#005691")
par(mfrow = c(1, 1))
dev.off()
# ---- ACF & PACF ----
cat("========== ACF & PACF ==========\n\n")
png("TS_TCS_Chart07_ACF_PACF.png", width = 1200, height = 800, res = 150)
par(mfrow = c(2, 2))
acf(na.omit(close_ts), main = "ACF - Original", lag.max = 36, col = "#004B87")
pacf(na.omit(close_ts), main = "PACF - Original", lag.max = 36, col = "#004B87")
acf(na.omit(diff_close), main = "ACF - Differenced", lag.max = 36, col = "#005691")
pacf(na.omit(diff_close), main = "PACF - Differenced", lag.max = 36, col = "#005691")
par(mfrow = c(1, 1))
dev.off()
cat("ACF/PACF plots saved.\n")
# ---- ARIMA MODELLING ----
cat("\n========== ARIMA MODELLING ==========\n\n")
arima_model <- auto.arima(close_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
cat("ARIMA Model Summary:\n")
print(summary(arima_model))
png("TS_TCS_Chart08_ARIMA_Residuals.png", width = 1200, height = 800, res = 150)
checkresiduals(arima_model)
dev.off()
cat("Residual diagnostics saved.\n")
cat("\n--- Ljung-Box Test ---\n")
lb_test <- Box.test(residuals(arima_model), type = "Ljung-Box", lag = 20)
print(lb_test)
cat("Result:", ifelse(lb_test$p.value > 0.05, "Residuals independent (GOOD)", "Autocorrelation present (BAD)"), "\n")
# ---- ARIMA FORECAST ----
cat("\n========== ARIMA FORECAST (12 Months) ==========\n\n")
forecast_result <- forecast(arima_model, h = 12)
print(forecast_result)
png("TS_TCS_Chart09_ARIMA_Forecast.png", width = 1200, height = 600, res = 150)
plot(forecast_result, main = "TCS - ARIMA Forecast (12 Months)",
     xlab = "Time", ylab = "Price (INR)", col = "#004B87", fcol = "#005691",
     shadecols = c("#E0F2FE", "#BAE6FD"))
grid(col = "grey90")
dev.off()
fc_df <- data.frame(
  Date     = seq(max(monthly_data$YearMonth) + months(1), by = "month", length.out = 12),
  Forecast = as.numeric(forecast_result$mean),
  Lo80 = as.numeric(forecast_result$lower[, 1]),
  Hi80 = as.numeric(forecast_result$upper[, 1]),
  Lo95 = as.numeric(forecast_result$lower[, 2]),
  Hi95 = as.numeric(forecast_result$upper[, 2])
)
p_forecast <- ggplot() +
  geom_line(data = monthly_data, aes(x = YearMonth, y = Avg_Close), color = "#004B87", linewidth = 0.5) +
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lo95, ymax = Hi95), fill = "#E0F2FE", alpha = 0.5) +
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lo80, ymax = Hi80), fill = "#BAE6FD", alpha = 0.5) +
  geom_line(data = fc_df, aes(x = Date, y = Forecast), color = "#005691", linewidth = 0.8, linetype = "dashed") +
  geom_point(data = fc_df, aes(x = Date, y = Forecast), color = "#005691", size = 2) +
  labs(
    title = "TCS - ARIMA Price Forecast (Next 12 Months)",
    x = "Date", y = "Monthly Avg Close (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_forecast)
ggsave("TS_TCS_Chart10_ARIMA_Forecast_ggplot.png", p_forecast, width = 12, height = 6, dpi = 300)
cat("\n--- Model Accuracy ---\n")
print(accuracy(arima_model))
cat("\n========== PART 01C COMPLETE ==========\n\n")
# ===========================================================================
#  PART 02: ALGORITHMIC TRADING
# ===========================================================================
cat("############################################################\n")
cat("#  PART 02: ALGORITHMIC TRADING                            #\n")
cat("############################################################\n\n")
# ---- STRATEGY 1: SMA 50/200 CROSSOVER ----
cat("========== STRATEGY 1: SMA 50/200 CROSSOVER ==========\n\n")
tcs <- tcs %>%
  mutate(
    SMA_50  = SMA(Close, n = 50),
    SMA_200 = SMA(Close, n = 200)
  )
tcs <- tcs %>%
  mutate(
    SMA_Signal = case_when(
      SMA_50 > SMA_200 & lag(SMA_50) <= lag(SMA_200) ~  1,
      SMA_50 < SMA_200 & lag(SMA_50) >= lag(SMA_200) ~ -1,
      TRUE ~ 0
    ),
    SMA_Position = ifelse(SMA_50 > SMA_200, 1, 0)
  )
tcs <- tcs %>%
  mutate(
    Daily_Ret      = Close / lag(Close) - 1,
    SMA_Strategy   = lag(SMA_Position) * Daily_Ret,
    SMA_Cumulative = cumprod(1 + ifelse(is.na(SMA_Strategy), 0, SMA_Strategy)),
    BH_Cumulative  = cumprod(1 + ifelse(is.na(Daily_Ret), 0, Daily_Ret))
  )
cat("Buy Signals (Golden Cross):", sum(tcs$SMA_Signal == 1, na.rm = TRUE), "\n")
cat("Sell Signals (Death Cross):", sum(tcs$SMA_Signal == -1, na.rm = TRUE), "\n")
buy_pts  <- tcs %>% filter(SMA_Signal == 1)
sell_pts <- tcs %>% filter(SMA_Signal == -1)
p_sma <- ggplot(tcs %>% filter(!is.na(SMA_200)), aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.4) +
  geom_line(aes(y = SMA_50, color = "SMA 50"), linewidth = 0.6) +
  geom_line(aes(y = SMA_200, color = "SMA 200"), linewidth = 0.6) +
  geom_point(data = buy_pts, aes(x = Date, y = Close), color = "#10B981", shape = 24, size = 3, fill = "#10B981") +
  geom_point(data = sell_pts, aes(x = Date, y = Close), color = "#EF4444", shape = 25, size = 3, fill = "#EF4444") +
  scale_color_manual(values = c("Close" = "grey60", "SMA 50" = "#00A4E4", "SMA 200" = "#004B87")) +
  labs(
    title = "Strategy 1: SMA 50/200 Crossover - Buy & Sell Signals",
    subtitle = "Green = Buy (Golden Cross) | Red = Sell (Death Cross)",
    x = "Date", y = "Price (INR)", color = "Legend"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_sma)
ggsave("Algo_TCS_Chart01_SMA_Crossover.png", p_sma, width = 14, height = 7, dpi = 300)
p_sma_cum <- ggplot(tcs %>% filter(!is.na(SMA_200)), aes(x = Date)) +
  geom_line(aes(y = SMA_Cumulative, color = "SMA Strategy"), linewidth = 0.7) +
  geom_line(aes(y = BH_Cumulative, color = "Buy & Hold"), linewidth = 0.7) +
  scale_color_manual(values = c("SMA Strategy" = "#10B981", "Buy & Hold" = "#EF4444")) +
  labs(
    title = "Strategy 1: Cumulative Returns - SMA Crossover vs Buy & Hold",
    x = "Date", y = "Growth of Rs.1", color = "Strategy"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_sma_cum)
ggsave("Algo_TCS_Chart02_SMA_Cumulative.png", p_sma_cum, width = 12, height = 6, dpi = 300)
# ---- STRATEGY 2: RSI ----
cat("\n========== STRATEGY 2: RSI (14) ==========\n\n")
tcs <- tcs %>% mutate(RSI_14 = RSI(Close, n = 14))
tcs <- tcs %>%
  mutate(
    RSI_Signal = case_when(
      RSI_14 < 30 & lag(RSI_14) >= 30 ~  1,
      RSI_14 > 70 & lag(RSI_14) <= 70 ~ -1,
      TRUE ~ 0
    ),
    RSI_Position = case_when(
      RSI_14 < 30  ~ 1,
      RSI_14 > 70  ~ 0,
      TRUE ~ NA_real_
    )
  )
tcs$RSI_Position <- na.locf(tcs$RSI_Position, na.rm = FALSE)
tcs$RSI_Position[is.na(tcs$RSI_Position)] <- 0
tcs <- tcs %>%
  mutate(
    RSI_Strategy   = lag(RSI_Position) * Daily_Ret,
    RSI_Cumulative = cumprod(1 + ifelse(is.na(RSI_Strategy), 0, RSI_Strategy))
  )
cat("RSI Buy Signals:", sum(tcs$RSI_Signal == 1, na.rm = TRUE), "\n")
cat("RSI Sell Signals:", sum(tcs$RSI_Signal == -1, na.rm = TRUE), "\n")
p_rsi <- ggplot(tcs %>% filter(!is.na(RSI_14)), aes(x = Date, y = RSI_14)) +
  geom_line(color = "#7C3AED", linewidth = 0.4) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#EF4444", linewidth = 0.6) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "#10B981", linewidth = 0.6) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey50") +
  annotate("rect", xmin = min(tcs$Date), xmax = max(tcs$Date), ymin = 70, ymax = 100, fill = "#EF4444", alpha = 0.05) +
  annotate("rect", xmin = min(tcs$Date), xmax = max(tcs$Date), ymin = 0, ymax = 30, fill = "#10B981", alpha = 0.05) +
  labs(
    title = "Strategy 2: RSI (14-Day)", subtitle = "Red Zone > 70 (Sell) | Green Zone < 30 (Buy)",
    x = "Date", y = "RSI Value"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_rsi)
ggsave("Algo_TCS_Chart03_RSI.png", p_rsi, width = 12, height = 6, dpi = 300)
# ---- STRATEGY 3: MACD ----
cat("\n========== STRATEGY 3: MACD (12,26,9) ==========\n\n")
macd_vals <- MACD(tcs$Close, nFast = 12, nSlow = 26, nSig = 9)
tcs$MACD      <- macd_vals[, "macd"]
tcs$MACD_Sig  <- macd_vals[, "signal"]
tcs$MACD_Hist <- tcs$MACD - tcs$MACD_Sig
tcs <- tcs %>%
  mutate(
    MACD_Signal   = case_when(
      MACD > MACD_Sig & lag(MACD) <= lag(MACD_Sig) ~  1,
      MACD < MACD_Sig & lag(MACD) >= lag(MACD_Sig) ~ -1,
      TRUE ~ 0
    ),
    MACD_Position = ifelse(MACD > MACD_Sig, 1, 0),
    MACD_Strategy   = lag(MACD_Position) * Daily_Ret,
    MACD_Cumulative = cumprod(1 + ifelse(is.na(MACD_Strategy), 0, MACD_Strategy))
  )
cat("MACD Buy Signals:", sum(tcs$MACD_Signal == 1, na.rm = TRUE), "\n")
cat("MACD Sell Signals:", sum(tcs$MACD_Signal == -1, na.rm = TRUE), "\n")
p_macd <- ggplot(tcs %>% filter(!is.na(MACD)), aes(x = Date)) +
  geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.5) +
  geom_line(aes(y = MACD_Sig, color = "Signal Line"), linewidth = 0.5) +
  geom_bar(aes(y = MACD_Hist, fill = MACD_Hist > 0), stat = "identity", alpha = 0.4) +
  scale_color_manual(values = c("MACD" = "#3B82F6", "Signal Line" = "#EF4444")) +
  scale_fill_manual(values = c("TRUE" = "#10B981", "FALSE" = "#EF4444"), guide = "none") +
  labs(title = "Strategy 3: MACD (12, 26, 9)", x = "Date", y = "MACD Value", color = "Legend") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_macd)
ggsave("Algo_TCS_Chart04_MACD.png", p_macd, width = 12, height = 6, dpi = 300)
# ---- ALL STRATEGIES COMPARISON ----
cat("\n========== STRATEGY COMPARISON ==========\n\n")
p_compare <- ggplot(tcs %>% filter(!is.na(SMA_200)), aes(x = Date)) +
  geom_line(aes(y = BH_Cumulative, color = "Buy & Hold"), linewidth = 0.7) +
  geom_line(aes(y = SMA_Cumulative, color = "SMA 50/200"), linewidth = 0.7) +
  geom_line(aes(y = RSI_Cumulative, color = "RSI (14)"), linewidth = 0.7) +
  geom_line(aes(y = MACD_Cumulative, color = "MACD"), linewidth = 0.7) +
  scale_color_manual(values = c("Buy & Hold" = "#EF4444", "SMA 50/200" = "#10B981",
                                "RSI (14)" = "#7C3AED", "MACD" = "#F59E0B")) +
  labs(
    title = "TCS - Strategy Comparison: Cumulative Returns",
    subtitle = "Growth of Rs.1 invested", x = "Date", y = "Cumulative Return", color = "Strategy"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_tcs +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_compare)
ggsave("Algo_TCS_Chart05_Strategy_Comparison.png", p_compare, width = 14, height = 7, dpi = 300)
# ---- PERFORMANCE METRICS ----
cat("\n========== PERFORMANCE METRICS ==========\n\n")
calc_metrics <- function(returns, name) {
  ret <- na.omit(returns)
  total_ret  <- prod(1 + ret) - 1
  annual_ret <- (1 + total_ret)^(252 / length(ret)) - 1
  annual_vol <- sd(ret) * sqrt(252)
  sharpe     <- annual_ret / annual_vol
  max_dd     <- maxDrawdown(xts(ret, order.by = tcs$Date[!is.na(returns)]))
  win_rate   <- sum(ret > 0) / length(ret) * 100
  
  data.frame(
    Strategy = name, Total_Return = round(total_ret * 100, 2),
    Annual_Return = round(annual_ret * 100, 2), Annual_Volatility = round(annual_vol * 100, 2),
    Sharpe_Ratio = round(sharpe, 3), Max_Drawdown = round(max_dd * 100, 2),
    Win_Rate = round(win_rate, 2)
  )
}
metrics <- bind_rows(
  calc_metrics(tcs$Daily_Ret, "Buy & Hold"),
  calc_metrics(tcs$SMA_Strategy, "SMA 50/200"),
  calc_metrics(tcs$RSI_Strategy, "RSI (14)"),
  calc_metrics(tcs$MACD_Strategy, "MACD (12,26,9)")
)
cat("--- Performance Table ---\n")
print(metrics, row.names = FALSE)
write.csv(metrics, "Algo_TCS_Performance_Metrics.csv", row.names = FALSE)
# ---- BOLLINGER BANDS ----
cat("\n========== BOLLINGER BANDS ==========\n\n")
bb <- BBands(tcs$Close, n = 20, sd = 2)
tcs$BB_Upper  <- bb[, "up"]
tcs$BB_Middle <- bb[, "mavg"]
tcs$BB_Lower  <- bb[, "dn"]
recent_bb <- tcs %>% tail(365)
p_bb <- ggplot(recent_bb, aes(x = Date)) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "#EFF6FF", alpha = 0.6) + # Soft Blue tint
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.5) +
  geom_line(aes(y = BB_Upper, color = "Upper Band"), linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = BB_Middle, color = "Middle Band"), linewidth = 0.4) +
  geom_line(aes(y = BB_Lower, color = "Lower Band"), linewidth = 0.4, linetype = "dashed") +
  scale_color_manual(values = c("Close" = "#004B87", "Upper Band" = "#3B82F6",
                                "Middle Band" = "#F59E0B", "Lower Band" = "#10B981")) +
  labs(title = "TCS - Bollinger Bands (Last 1 Year)", x = "Date", y = "Price (INR)", color = "Legend") +
  scale_y_continuous(labels = comma) +
  theme_tcs
print(p_bb)
ggsave("Algo_TCS_Chart06_Bollinger_Bands.png", p_bb, width = 12, height = 6, dpi = 300)
# ===========================================================================
#                         FINAL SUMMARY
# ===========================================================================
cat("\n\n============================================================\n")
cat("   TATA CONSULTANCY SERVICES (TCS) - COMPLETE R PROJECT FINISHED!\n")
cat("============================================================\n")
cat(" Company  : Tata Consultancy Services Limited (TCS.NS)\n")
cat(" Data     : 10 years from Yahoo Finance\n")
cat("------------------------------------------------------------\n")
cat(" PART 01A : Data downloaded, saved (CSV + Excel), cleaned\n")
cat(" PART 01B : 10 ggplot2 visualisation charts created\n")
cat(" PART 01C : Trend, Decomposition, ADF, ACF/PACF, ARIMA done\n")
cat(" PART 02  : 3 Algo strategies + Bollinger Bands backtested\n")
cat("------------------------------------------------------------\n")
cat(" Total Charts : 26\n")
cat(" Total Files  : CSV, Excel, PNG charts, Performance metrics\n")
cat("============================================================\n")

# Minimal documentation

RCandles -- R bindings to HighCharts for producing beautiful candlesticks charts

## Installation

```{r}
library(devtools)
devtools::install_github("redmode/RCandles")
```

## Usage

### Data load


```{r}
library(RCandles)

# Motifs------------------------------------------------------------------------

# Loads backtesting data
bt_file <- system.file("examples/bt_data.rda", package = "RCandles")
bt <- get(load(bt_file)) %>% tbl_df()

ar_file <- system.file("examples/ar_trades.Rdata", package = "RCandles")
ar <- get(load(ar_file)) %>% tbl_df()

# Loads trade logs
logs_file <- system.file("examples/tradelogs.csv", package = "RCandles")
trades <- ar %>%
  select(TimeOpen, TimeClose, Type, OpenPrice, ClosePrice, SLprice, TPprice, Profit_Perc, rankfiltered)

# Loads motifs
motifs_file <- system.file("examples/motifs.RData", package = "RCandles")
motifs <- load(motifs_file)
motifs <- get(motifs[2]) %>% tbl_df()

# Extracts prices
price_data <- bt %>%
  select(Date, Open, High, Low, Close)

# Adds fake volume
volume_data <- bt %>%
  select(Date) %>%
  rowwise() %>%
  mutate(Volume = round(runif(1, 1, 10))) %>%
  ungroup()

# Extracts StopLosses
ind_sl_buy <- bt %>%
  select(Date, Value = STOPLOSS_BUY)

# Plots motifs
RMotifs(
  price_data = price_data,
  motifs = motifs,
  trades = trades,
  motif_names = c("dcba dcaa", "dcab dbab", "aacd abcd abdd", "babd bacd"),
  title = "Demo (AAPL) Motifs",
  width = 800,
  height = 600
)

RMotifs(
  price_data = price_data,
  motifs = motifs,
  motif_top = 20,
  trades = trades,
  title = "Demo (AAPL) Motifs",
  width = 800,
  height = 600
)
```

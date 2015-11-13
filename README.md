# Minimal documentation

RCandles -- R bindings to HighCharts for producing beautiful candlesticks charts

## Installation

```{r}
library(devtools)
devtools::install_github("redmode/RCandles")
```

## Usage

```{r}
library(RCandles)

file_data <- system.file("examples/demo_data.RData", package = "RCandles")
RCandles(file_data)
```

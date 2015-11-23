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
RCandles(file_data,
         title = "AAPL Chart",
         width = 640,
         height = 480,
         background_color = "#002240",
         line_color = "darkgreen",
         up_color = "yellow",
         vertical_lines = c("2014-04-03 13:00:00 FET", "2014-04-03 15:00:00 FET", "2014-04-03 17:00:00 FET"),
         symbols = list(
           list(from = "2014-04-03 13:00:00 FET", to = "2014-04-03 15:00:00 FET", text = "6.5%", y = -60),
           list(from = "2014-04-03 15:00:00 FET", to = "2014-04-03 17:00:00 FET", text = "\u21E7", y = -90)
         ))
```

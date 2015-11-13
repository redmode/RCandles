# Minimal documentation

TechanJS -- minimal R bindings to JavaScript library [TechanJS](http://techanjs.org/)

## Installation

```{r}
library(devtools)
devtools::install_github("redmode/TechanJS")
```

## Usage

```{r}
library(TechanJS)

file_data <- system.file("examples/example_data.csv", package = "TechanJS")

TechanJS(file_data,
         title = "Candlesticks data",
         width = 700,
         height = 450,
         type = "candlestick",
         trendlines = list(
           list(start = list(date = dateToJS("2014-02-07"), value = 70.50),
                end = list(date = dateToJS("2015-05-09"), value = 63.34)),
           list(start = list(date = dateToJS("2013-10-26"), value = 43.7),
                end = list(date = dateToJS("2014-02-07"), value = 70.50)),
           list(start = list(date = dateToJS("2013-10-26"), value = 43.7),
                end = list(date = dateToJS("2015-05-09"), value = 63.34))
         ))
```

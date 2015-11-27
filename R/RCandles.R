#' @export
RCandles <- function(filename,
                     title = "",
                     width = 640,
                     height = 480,
                     background_color = "black",
                     line_color = "green",
                     up_color = "white",
                     down_color = "rgba(255, 255, 255, 0)",
                     vertical_lines = NULL,
                     symbols = NULL,
                     trendlines = NULL) {

  # forward options using x
  x <- list(
    title = title,
    width = width,
    height = height,
    background_color = background_color,
    line_color = line_color,
    up_color = up_color,
    down_color = down_color,
    vertical_lines = vertical_lines,
    symbols = symbols,
    trendlines = trendlines)

  RCandlesEnv$filename <- normalizePath(filename)
  RCandlesEnv$x <- x

  # create widget
  htmlwidgets::createWidget(
    name = 'RCandles',
    x,
    width = width,
    height = height,
    package = 'RCandles'
  )
}


#' @export
RCandles_html <- function(id, style, class, ...) {

  # Gets JSON from file
  .data <- RCandlesEnv$filename

  prices <- readRDS(file = RCandlesEnv$filename) %>%
    mutate(Date = as.numeric(Date)) %>%
    select(-Volume)

  data_str <- toJSON(prices %>% set_colnames(NULL), pretty = TRUE)

# Creates HEAD script-----------------------------------------------------------
  .head <- ""

# Body script-------------------------------------------------------------------
  .script <- "
  $(function () {
  data = <DATA>;

  // Creates the chart
  $('#container').highcharts('StockChart', {

  // Background code
  chart: {
    backgroundColor: '<BACKGROUND-COLOR>',
  },

  // Range definition
  rangeSelector : {
    buttons : [{
      type : 'hour',
      count : 1,
      text : '1h'
    }, {
      type : 'day',
      count : 1,
      text : '1D'
    }, {
      type : 'all',
      count : 1,
      text : 'All'
    }],
    selected : 2,
    inputEnabled : false
  },

  // Candlesticks colors and styles
  plotOptions: {
    candlestick: {
      lineWidth: 1,
      lineColor: '<LINE-COLOR>',
      color: '<DOWN-COLOR>',
      upColor: '<UP-COLOR>'
    }
  },

  // Chart title
  title: {
    text: '<TITLE>',
    style: {
      color: 'white',
      fontWeight: 'bold'
    }
  },

  xAxis: {
    plotLines: <VERTICAL_LINES>,
    plotBands: <SYMBOLS>
  },

  yAxis: [{
    labels: {
      align: 'left',
      x: 10,
      style: {
        color: 'white'
      }
    },
    title: {
      text: ''
    },
    height: '70%',
    lineWidth: 1,
    gridLineWidth: 1,
    minorGridLineWidth: 0
  }, {
    labels: {
      enabled: false
    },
    title: {
      text: 'BUY ENTRY<br/>BUY EXIT<br/>HOLD BUY',
      x: +20,
      rotation: 0,
      style: {
        color: 'white',
        fontSize: '10px'
      }
    },
    top: '65%',
    height: '35%',
    offset: 0,
    lineWidth: 1,
    gridLineWidth: 0
  }],

  series: [{
    type: 'candlestick',
    name: 'AAPL',
    data: data
  }, {
    name: 'Boxes',
    type: 'polygon',
    lineWidth: 2,
    lineColor: 'red',
    color: 'rgba(255, 255, 255, 0)',
    data: [[1396526400, 65.5],
           [1396548000, 65.5],
           [1396548000, 66],
           [1396526400, 66]]
  }, {
    name: 'Triangles',
    type: 'polygon',
    lineWidth: 0,
    color: 'rgba(255, 153, 0, 0.5)',
    data: [[1396580800, 66.4],
           [1396620800, 65],
           [1396580800, 65]]
  }, {
    name: 'Buy-Sell-Red',
    type: 'polygon',
    lineWidth: 0,
    color:'red',
    data: [[1396526400, 65],
           [1396526400, 65.5],
           [1396548000, 65.5],
           [1396548000, 65]],
    yAxis: 1
  }, {
    name: 'Buy-Sell-Yellow',
    type: 'polygon',
    lineWidth: 0,
    color:'yellow',
    data: [[1396530400, 64],
           [1396530400, 64.5],
           [1396578000, 64.5],
           [1396578000, 64]],
    yAxis: 1
  }]
  });
  });
"

# Imputing----------------------------------------------------------------------
  impute <- function(txt, pattern, replacement) {
    stri_replace_all_fixed(txt, pattern, replacement)
  }

  .script %<>%
    impute(pattern = "<DATA>", replacement = data_str) %>%
    impute(pattern = "<TITLE>", replacement = RCandlesEnv$x$title) %>%
    impute(pattern = "<BACKGROUND-COLOR>", replacement = RCandlesEnv$x$background_color) %>%
    impute(pattern = "<LINE-COLOR>", replacement = RCandlesEnv$x$line_color) %>%
    impute(pattern = "<UP-COLOR>", replacement = RCandlesEnv$x$up_color) %>%
    impute(pattern = "<DOWN-COLOR>", replacement = RCandlesEnv$x$down_color)

  # Adds vertical lines
  if (is.null(RCandlesEnv$x$vertical_lines)) {
    .script %<>% impute(pattern = "plotLines: <VERTICAL_LINES>,", replacement = "")
  } else {
    .vline <- list(
      color = 'white',
      width = 1,
      dashStyle = 'dash',
      value = NA
    )

    # Creates
    all_vlines <- llply(RCandlesEnv$x$vertical_lines, function(v) {
      vline <- .vline
      vline$value <- as.numeric(as.POSIXct(v, origin = "1970-01-01"))
      vline
    }) %>% toJSON(auto_unbox = TRUE)

    # Imputes vertical lines
    .script %<>% impute("<VERTICAL_LINES>", all_vlines)
  }

  # Adds symbols
  if (is.null(RCandlesEnv$x$symbols)) {
    .script %<>% impute(pattern = "plotBands: <SYMBOLS>", replacement = "")
  } else {
    .symbol <- list(
      from = NA,
      to = NA,
      label = list(
        text = NA,
        align = "center",
        verticalAlign = "bottom",
        y = NA,
        style = list(
          color = "white"
        )
      )
    )

    # Creates symbols
    all_symbols <- llply(RCandlesEnv$x$symbols, function(symb) {
      symbol <- .symbol
      symbol$from <- as.numeric(as.POSIXct(symb$from, origin = "1970-01-01"))
      symbol$to <- as.numeric(as.POSIXct(symb$to, origin = "1970-01-01"))
      symbol$label$text <- symb$text
      symbol$label$y <- symb$y
      symbol
    }) %>% toJSON(auto_unbox = TRUE)

    # Imputes vertical lines
    .script %<>% impute("<SYMBOLS>", all_symbols)
  }

  # Returns list of tags
  tagList(
    tags$head(HTML(.head)),
    tags$div("",
             id = "container",
             style = sprintf("height: %dpx; width: %dpx", RCandlesEnv$x$height, RCandlesEnv$x$width)),
    tags$script(HTML(.script))
  )
}

#' Widget output function for use in Shiny
#'
#' @export
RCandlesOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId, 'id_RCandles', width, height, package = 'RCandles')
}

#' Widget render function for use in Shiny
#'
#' @export
renderRCandles <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, RCandlesOutput, env, quoted = TRUE)
}


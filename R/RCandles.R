#' @export
RCandles <- function(price_data,
                     volume_data = NULL,
                     title = "",
                     width = 640,
                     height = 480,
                     background_color = "black",
                     line_color = "green",
                     up_color = "white",
                     down_color = "rgba(255, 255, 255, 0)",
                     enable_hover = FALSE,
                     enable_lower_window = FALSE,
                     indicators = NULL,
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
    enable_hover = enable_hover,
    enable_lower_window = enable_lower_window,
    indicators = indicators,
    vertical_lines = vertical_lines,
    symbols = symbols,
    trendlines = trendlines)

  RCandlesEnv$price_data <- price_data
  RCandlesEnv$volume_data <- volume_data
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
  prices <- RCandlesEnv$price_data
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

  // Hover
  tooltip: {
    enabled: <HOVER>
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
      type : 'week',
      count : 1,
      text : '1W'
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
    plotBands: <SYMBOLS>,

    type: 'datetime',

    // http://api.highcharts.com/highstock#xAxis
    dateTimeLabelFormats: {
                second: '%Y-%m-%d<br/>%H:%M:%S',
                minute: '%Y-%m-%d<br/>%H:%M',
                hour: '%Y-%m-%d<br/>%H:%M',
                day: '%Y<br/>%m-%d',
                week: '%Y<br/>%m-%d',
                month: '%Y-%m',
                year: '%Y'
            }
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
    height: '<HEIGHT_MAIN>%',
    lineWidth: 1,
    gridLineWidth: 1,
    minorGridLineWidth: 1,
    floor: 0,
    gridLineColor: '<BACKGROUND-COLOR>'
  }, <VOLUME>
   , <LOWER_WINDOW>


/*, {
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
    top: '70%',
    height: '30%',
    offset: 0,
    lineWidth: 1,
    gridLineWidth: 0
  }*/],

  series: [
    <INDICATORS>,
    <VOLUME_SERIES>,

    {
      type: 'candlestick',
      name: 'AAPL',
      data: data
    }, {
      name: 'Boxes',
      type: 'polygon',
      lineWidth: 2,
      lineColor: 'orange',
      color: 'rgba(255, 255, 255, 0)',
      data: [[1.396397e+12, 23.2],
             [1.396310e+12, 23.2],
             [1.396310e+12, 22.9],
             [1.396397e+12, 22.9]]
    }, {
    name: 'Triangles',
    type: 'polygon',
    lineWidth: 0,
    color: 'rgba(255, 153, 0, 0.5)',
    data: [[1.396310e+12, 23.1],
           [1.396397e+12, 23.15],
           [1.396310e+12, 22.9]]
    }

/*, {
    name: 'Buy-Sell-Red',
    type: 'polygon',
    lineWidth: 0,
    color:'red',
    data: [[1396526400000, 65],
           [1396526400000, 65.5],
           [1396548000000, 65.5],
           [1396548000000, 65]],
    yAxis: 1
  }, {
    name: 'Buy-Sell-Yellow',
    type: 'polygon',
    lineWidth: 0,
    color:'yellow',
    data: [[1396530400000, 64],
           [1396530400000, 64.5],
           [1396578000000, 64.5],
           [1396578000000, 64]],
    yAxis: 1
  }*/]
  });
  });
"

# Imputing----------------------------------------------------------------------
  impute <- function(txt, pattern, replacement) {
    stri_replace_all_fixed(txt, pattern, replacement)
  }

  # Basic replacements
  .script %<>%
    impute(pattern = "<DATA>", replacement = data_str) %>%
    impute(pattern = "<TITLE>", replacement = RCandlesEnv$x$title) %>%
    impute(pattern = "<BACKGROUND-COLOR>", replacement = RCandlesEnv$x$background_color) %>%
    impute(pattern = "<LINE-COLOR>", replacement = RCandlesEnv$x$line_color) %>%
    impute(pattern = "<UP-COLOR>", replacement = RCandlesEnv$x$up_color) %>%
    impute(pattern = "<DOWN-COLOR>", replacement = RCandlesEnv$x$down_color) %>%
    impute(pattern = "<HOVER>", replacement = stri_trans_tolower(as.character(RCandlesEnv$x$enable_hover)))

  # Is lower window?
  if (RCandlesEnv$x$enable_lower_window) {
    .script %<>%
      impute(pattern = "<HEIGHT_MAIN>", replacement = "70")

    .lower <- list(
      top = '70%',
      height = '30%',
      gridLineWidth = 1,
      gridLineColor = RCandlesEnv$x$background_color
    ) %>% toJSON(auto_unbox = TRUE)

    .script %<>% impute(pattern = "<LOWER_WINDOW>", replacement = .lower)
  } else {
    .script %<>%
      impute(pattern = "<HEIGHT_MAIN>", replacement = "100") %>%
      impute(pattern = ", <LOWER_WINDOW>", replacement = "")
  }

  # Adds volume
  if (is.null(RCandlesEnv$volume_data)) {
    .script %<>%
      impute(pattern = ", <VOLUME>", replacement = "") %>%
      impute(pattern = "<VOLUME_SERIES>,", replacement = "")
  } else {
    .volume <- list(
      labels = list(
        enabled = "false"
      ),
      top = sprintf('%d%%', ifelse(RCandlesEnv$x$enable_lower_window, 60, 90)),
      height = '10%',
      offset = 0,
      lineWidth = 2,
      gridLineWidth = 0
    )
    .volume %<>% toJSON(auto_unbox = TRUE) %>% stri_replace_all_fixed('"false"', 'false')

    # Imputes volume axis
    .script %<>% impute("<VOLUME>", .volume)

    # Imputes volume series
    .volume_data <- list(
      type = 'column',
      data = RCandlesEnv$volume_data %>% set_colnames(NULL),
      color = '#4444aa',
      yAxis = 1
    ) %>% toJSON(auto_unbox = TRUE)
    .script %<>% impute("<VOLUME_SERIES>", .volume_data)
  }

  # Adds indicators
  if (is.null(RCandlesEnv$x$indicators)) {
    .script %<>% impute(pattern = "<INDICATORS>,", replacement = "")
  } else {
    .indicator <- list(
      name = '',
      type = 'line',
      color = 'white',
      dashStyle = 'longdash',
      lineWidth = 1,
      yAxis = 0,
      data = NA
    )

    # Creates array of indicators
    all_indicators <- ""
    l_ply(RCandlesEnv$x$indicators, function(i) {
      indicator <- .indicator

      # Constructs indicator
      l_ply(names(i), function(name_i) {
        indicator[[name_i]] <<- i[[name_i]]
      })
      indicator$data %<>% set_colnames(NULL)

      indicator %<>% toJSON(auto_unbox = TRUE)
      all_indicators <<- stri_c(all_indicators,
                                ifelse(stri_length(all_indicators) == 0, "", ","),
                                indicator)
    })

    # Imputes vertical lines
    .script %<>% impute("<INDICATORS>", all_indicators)
  }

  # Adds vertical lines
  if (is.null(RCandlesEnv$x$vertical_lines)) {
    .script %<>% impute(pattern = "plotLines: <VERTICAL_LINES>,", replacement = "")
  } else {
    .vline <- list(
      color = '#222222',
      width = 1,
      dashStyle = 'dash',
      value = NA
    )

    # Creates array of vlines
    all_vlines <- llply(RCandlesEnv$x$vertical_lines, function(v) {
      vline <- .vline
      vline$value <- v
      vline
    }) %>% toJSON(auto_unbox = TRUE)

    # Imputes vertical lines
    .script %<>% impute("<VERTICAL_LINES>", all_vlines)
  }

  # Adds symbols
  if (is.null(RCandlesEnv$x$symbols)) {
    .script %<>% impute(pattern = "plotBands: <SYMBOLS>,", replacement = "")
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

    # Creates array of symbols
    all_symbols <- llply(RCandlesEnv$x$symbols, function(symb) {
      symbol <- .symbol
      symbol$from <- as.numeric(as.POSIXct(symb$from, origin = "1970-01-01")) * 1000
      symbol$to <- as.numeric(as.POSIXct(symb$to, origin = "1970-01-01")) * 1000
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
RCandlesOutput <- function(outputId, width = '100%', height = sprintf('%dpx', RCandlesEnv$x$height)) {
  shinyWidgetOutput(outputId, 'id_RCandles', width, height, package = 'RCandles')
}

#' Widget render function for use in Shiny
#'
#' @export
renderRCandles <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, RCandlesOutput, env, quoted = TRUE)
}


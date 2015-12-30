#' @export
RBacktesting <- function(price_data,
                         volume_data = NULL,
                         stop_loss = NULL,
                         indicators = NULL,
                         trades = NULL,
                         from = "2014-01-01 00:00",
                         to = "2014-03-31 00:00",
                         title = "Backtesting",
                         width = 1000,
                         height = 800,
                         background_color = "black",
                         line_color = "green",
                         up_color = "rgba(255, 255, 255, 0)",
                         down_color = "white",
                         enable_hover = TRUE,
                         enable_lower_window = FALSE) {

  # Creates dates range---------------------------------------------------------
  from <- as.numeric(ymd_hm(from)) * 1000
  to <- as.numeric(ymd_hm(to)) * 1000

  # Identify days (for vertical lines)------------------------------------------
  vertical_lines <- price_data %>%
    mutate(num_date = as.numeric(ymd_hm(Date)) * 1000) %>%
    filter(between(num_date, from, to)) %>%
    mutate(Date = ymd_hm(Date),
           year = year(Date),
           month = month(Date),
           day = day(Date)) %>%
    group_by(year, month, day) %>%
    summarise(Date = ymd_hms(sprintf("%4d-%2d-%2d 00:00:00", year[1], month[1], day[1])) %>%
                as.numeric() %>% `*`(1000)) %$% Date

  # Transforms price date to ticks----------------------------------------------
  price_data %<>%
    mutate(Date = as.numeric(ymd_hm(Date)) * 1000) %>%
    filter(between(Date, from, to))

  # Filters stop-loss data------------------------------------------------------
  if (!is.null(stop_loss)) {
    # Converts to format compatible with indicators
    stop_loss <- list(name = "Stoploss",
                      data = stop_loss,
                      color = "#4444aa",
                      dashStyle = "dash",
                      lineWidth = 2)

    # Filters data
    stop_loss$data %<>%
      mutate(Date = as.numeric(ymd_hm(Date)) * 1000) %>%
      filter(between(Date, from, to))
  }

  # Processes indicators--------------------------------------------------------
  if (!is.null(indicators)) {
    indicators %<>% llply(function(ind) {
      # Transforms and filters dates
      ind$data %<>%
        mutate(Date = as.numeric(ymd_hm(Date)) * 1000) %>%
        filter(between(Date, from, to))

      ind
    })
  }

  # Processes tradelogs---------------------------------------------------------
  if (!is.null(trades)) {
    trades %<>%
      # Backtesting data adjusting
      mutate(TimeOpen = as.numeric(ymd_hm(TimeOpen)) * 1000,
             TimeClose = as.numeric(ymd_hm(TimeClose)) * 1000,
             Profit_Perc = sprintf("%.2f%%", Profit_Perc * 100)) %>%
      filter(TimeOpen >= from, TimeClose <= to) %>%
      rowwise()

    # Extracts symbols
    symbols <- trades %>%
      select(from = TimeOpen, to = TimeClose, text = Profit_Perc) %>%
      do(symb = {
        .
      }) %$% symb

    # Triangles
    triangles <- trades %>%
      do(tri = {
        .trade <- .

        list(name = .trade$Profit_Perc,
             type = "polygon",
             lineWidth = 0,
             color = ifelse(stri_sub(.trade$Profit_Perc, 1, 1) == "-",
                            "rgba(255, 0, 0, 0.3)",
                            "rgba(255, 153, 0, 0.3)"),
             zIndex = 10,
             data = list(c(.trade$TimeOpen, .trade$OpenPrice),
                         c(.trade$TimeClose, .trade$OpenPrice),
                         c(.trade$TimeClose, .trade$ClosePrice))
        )
      }) %$% tri

    # Boxes
    trades %<>%
      do(box = {
        .trade <- .
        .high <- ifelse(.trade$Type == "SELL", .trade$SLprice, .trade$ClosePrice)
        .low <- ifelse(.trade$Type == "SELL", .trade$ClosePrice, .trade$SLprice)

        list(name = .trade$Profit_Perc,
             type = "polygon",
             lineWidth = 1,
             lineColor = "orange",
             color = "rgba(255, 255, 255, 0)",
             data = list(c(.trade$TimeOpen, .high),
                         c(.trade$TimeClose, .high),
                         c(.trade$TimeClose, .low),
                         c(.trade$TimeOpen, .low))
        )
      }) %$% box
  } else {
    trades <- NULL
    symbols <- NULL
    triangles <- NULL
  }

  # Processes volume------------------------------------------------------------
  if (!is.null(volume_data)) {
    volume_data %<>%
      mutate(Date = as.numeric(ymd_hm(Date)) * 1000) %>%
      filter(between(Date, from, to))

    enable_lower_window <- TRUE
  }

  # Forwards options to widget--------------------------------------------------
  x <- list(
    title = title,
    width = width,
    height = height,
    background_color = background_color,
    line_color = line_color,
    up_color = up_color,
    down_color = down_color,
    enable_hover = enable_hover,
    enable_lower_window = enable_lower_window)

  RCandlesEnv$price_data <- price_data
  RCandlesEnv$volume_data <- volume_data
  RCandlesEnv$stop_loss <- stop_loss
  RCandlesEnv$indicators <- indicators
  RCandlesEnv$trades <- trades
  RCandlesEnv$triangles <- triangles
  RCandlesEnv$symbols <- symbols
  RCandlesEnv$vertical_lines <- vertical_lines

  RCandlesEnv$x <- x

  # Creates widget--------------------------------------------------------------
  htmlwidgets::createWidget(
    name = 'RBacktesting',
    x,
    width = width,
    height = height,
    package = 'RCandles'
  )
}


#' @export
RBacktesting_html <- function(id, style, class, ...) {

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
    enabled: <HOVER>,
    shared: false,
    useHTML: false,
    headerFormat: '',
    pointFormat: '{series.name}'
  },

  // Range definition
  rangeSelector : {
    buttons : [{
      type : 'hour',
      count : 4,
      text : '4h'
    }, {
      type : 'day',
      count : 1,
      text : '1D'
    }, {
      type : 'day',
      count : 3,
      text : '3D'
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
    },
    polygon: {
      dataLabels: {
        enabled: false,
        borderRadius: 5,
        backgroundColor: 'rgba(252, 255, 197, 0.5)',
        borderWidth: 1,
        borderColor: '#AAA',
        y: -6,
        format: '{series.name}'
      }
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
 ,{
    labels: {
      enabled: true
    },
    title: {
      text: '',
      x: +20,
      rotation: 0,
      style: {
        color: 'white',
        fontSize: '10px'
      }
    },
    top: '90%',
    height: '10%',
    offset: 0,
    lineWidth: 1,
    gridLineWidth: 0
  }],

  series: [
    <INDICATORS>,
    <STOPLOSS>,
    <VOLUME_SERIES>,
    <BOXES>,
    <TRIANGLES>,

    {
      type: 'candlestick',
      name: '.',
      data: data,
      zIndex: 0
    }
  ]

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
      impute(pattern = "<HEIGHT_MAIN>", replacement = "90")

    .lower <- list(
      top = '90%',
      height = '10%',
      gridLineWidth = 1,
      gridLineColor = RCandlesEnv$x$background_color
    ) %>% toJSON(auto_unbox = TRUE)

    .script %<>% impute(pattern = "<LOWER_WINDOW>", replacement = .lower)
  } else {
    .script %<>%
      impute(pattern = "<HEIGHT_MAIN>", replacement = "100") %>%
      impute(pattern = ", <LOWER_WINDOW>", replacement = "")
  }

  # Adds volume-----------------------------------------------------------------
  if (is.null(RCandlesEnv$volume_data)) {
    .script %<>%
      impute(pattern = ", <VOLUME>", replacement = "") %>%
      impute(pattern = "<VOLUME_SERIES>,", replacement = "")
  } else {
    .volume <- list(
      top = '90%',
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
      name = 'Volume',
      data = RCandlesEnv$volume_data %>% set_colnames(NULL),
      color = '#4444aa',
      yAxis = 1
    ) %>% toJSON(auto_unbox = TRUE)
    .script %<>% impute("<VOLUME_SERIES>", .volume_data)
  }

  # Adds indicators-------------------------------------------------------------
  if (is.null(RCandlesEnv$indicators)) {
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
    l_ply(RCandlesEnv$indicators, function(i) {
      indicator <- .indicator

      # Constructs indicator
      l_ply(names(i), function(name_i) {
        indicator[[name_i]] <<- i[[name_i]]
      })
      indicator$data %<>% set_colnames(NULL)

      indicator %<>% toJSON(auto_unbox = TRUE, pretty = TRUE)
      all_indicators <<- stri_c(all_indicators,
                                ifelse(stri_length(all_indicators) == 0, "", ","),
                                indicator)
    })

    # Imputes code for indicators
    all_indicators %<>% stri_replace_all_fixed('"name"', 'name')
    .script %<>% impute("<INDICATORS>", all_indicators)
  }

  # Adds stoplosses-------------------------------------------------------------
  if (is.null(RCandlesEnv$stop_loss)) {
    .script %<>% impute(pattern = "<STOPLOSS>,", replacement = "")
  } else {
    .stoploss <- list(
      name = '',
      type = 'arearange',
      linkedTo = ':previous',
      fillOpacity = 0.0,
      zIndex = 0,
      color = 'white',
      lineWidth = 1,
      yAxis = 0,
      data = NA
    )

    # Updates stoploss data
    stoploss <- .stoploss
    l_ply(names(RCandlesEnv$stop_loss), function(name_i) {
      stoploss[[name_i]] <<- RCandlesEnv$stop_loss[[name_i]]
    })
    stoploss$data %<>% set_colnames(NULL)
    stoploss %<>% toJSON(auto_unbox = TRUE, pretty = TRUE)

    # Imputes code for stoploss
    .script %<>% impute("<STOPLOSS>", stoploss)
  }

  # Adds vertical lines---------------------------------------------------------
  if (is.null(RCandlesEnv$vertical_lines)) {
    .script %<>% impute(pattern = "plotLines: <VERTICAL_LINES>,", replacement = "")
  } else {
    .vline <- list(
      color = '#222222',
      width = 1,
      dashStyle = 'dash',
      value = NA
    )

    # Creates array of vlines
    all_vlines <- llply(RCandlesEnv$vertical_lines, function(v) {
      vline <- .vline
      vline$value <- v
      vline
    }) %>% toJSON(auto_unbox = TRUE)

    # Imputes vertical lines
    .script %<>% impute("<VERTICAL_LINES>", all_vlines)
  }

  # Adds symbols----------------------------------------------------------------
  if (is.null(RCandlesEnv$symbols)) {
    .script %<>% impute(pattern = "plotBands: <SYMBOLS>,", replacement = "")
  } else {
    .symbol <- list(
      from = NA,
      to = NA,
      label = list(
        text = NA,
        align = "center",
        verticalAlign = "top",
        rotation = 0,
        style = list(
          color = "white",
          `font-size` = "10px"
        )
      )
    )

    # Creates array of symbols
    all_symbols <- llply(RCandlesEnv$symbols, function(symb) {
      symbol <- .symbol
      symbol$from <- symb$from
      symbol$to <- symb$to
      symbol$label$text <- symb$text
      symbol
    }) %>% toJSON(auto_unbox = TRUE)

    # Imputes vertical lines
    .script %<>% impute("<SYMBOLS>", all_symbols)
  }

  # Adds boxes------------------------------------------------------------------
  if (is.null(RCandlesEnv$trades)) {
    .script %<>%
      impute(pattern = "<BOXES>,", replacement = "") %>%
      impute(pattern = "<TRIANGLES>,", replacement = "")
  } else {
    .boxes <- toJSON(RCandlesEnv$trades, pretty = TRUE, auto_unbox = TRUE) %>%
      stri_replace_first_fixed("[", "") %>%
      stri_replace_last_fixed("]", "")

    .tri <- toJSON(RCandlesEnv$triangles, pretty = TRUE, auto_unbox = TRUE) %>%
      stri_replace_first_fixed("[", "") %>%
      stri_replace_last_fixed("]", "")

    # Imputes volume axis
    .script %<>% impute("<BOXES>", .boxes)
    .script %<>% impute("<TRIANGLES>", .tri)
  }

  # Returns list of tags--------------------------------------------------------
  tagList(
    tags$head(HTML(.head)),
    tags$div("",
             id = "container",
             style = sprintf("height: %dpx; width: %dpx",
                             RCandlesEnv$x$height, RCandlesEnv$x$width)),
    tags$script(HTML(.script))
  )
}

#' Widget output function for use in Shiny
#'
#' @export
RBacktestingOutput <- function(outputId, width = '100%', height = sprintf('%dpx', RCandlesEnv$x$height)) {
  shinyWidgetOutput(outputId, 'id_RBacktesting', width, height, package = 'RCandles')
}

#' Widget render function for use in Shiny
#'
#' @export
renderRBacktesting <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, RBacktestingOutput, env, quoted = TRUE)
}


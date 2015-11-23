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
                     trendlines = NULL,
                     annotations = NULL) {

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

    plotBands: [{
      from: (new Date('10/12/2011 20:00:00')).getTime(),
      to: (new Date('10/13/2011 20:00:00')).getTime(),
      label: {
        text: '6.5%',
        align: 'center',
        verticalAlign: 'bottom',
        style: {
          color: 'white'
        },
        y: -20
      }
    },{
      from: (new Date('10/09/2011 04:00:00')).getTime(),
      to: (new Date('10/09/2011 10:00:00')).getTime(),
      label: {
        text: '\u21E7 ',
        align: 'center',
        verticalAlign: 'bottom',
        style: {
          color: 'white'
        },
        y: -20
      }
    }]

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
  }/*, {
                name: 'Boxes',
            	type: 'polygon',
                lineWidth: 2,
            	lineColor:'orange',
                color:'rgba(255, 255, 255, 0)',
                data: [[(new Date('10/12/2011 20:00:00')).getTime(),380],[(new Date('10/12/2011 20:00:00')).getTime(),420],
[(new Date('10/13/2011 20:00:00')).getTime(),420],
[(new Date('10/13/2011 20:00:00')).getTime(),380]]

            }, {
                name: 'Triangles',
            	type: 'polygon',
                lineWidth: 0,
                color:'rgba(255, 153, 0, 0.5)',
                data: [[(new Date('10/12/2011 20:00:00')).getTime(),410],
[(new Date('10/13/2011 20:00:00')).getTime(),410],
[(new Date('10/13/2011 20:00:00')).getTime(),390]]

            }, {
                type: 'polygon',
                name: 'Buy-Sell-Red',
                lineWidth: 0,
                color:'red',
                data: [
                    [(new Date('10/12/2011 00:00:00')).getTime(),380],[(new Date('10/12/2011 00:00:00')).getTime(),390], [(new Date('10/12/2011 00:00:00')).getTime() + 46400000,390], [(new Date('10/12/2011 00:00:00')).getTime() + 46400000,380],
                ],
                yAxis: 1,
            },{
                type: 'polygon',
                name: 'Buy-Sell-Yellow',
                lineWidth: 0,
                color:'yellow',
                data: [
                    [(new Date('10/12/2011 00:00:00')).getTime(),390],[(new Date('10/12/2011 00:00:00')).getTime(),400], [(new Date('10/12/2011 00:00:00')).getTime() + 86400000,400], [(new Date('10/12/2011 00:00:00')).getTime() + 86400000,390],
                ],
                yAxis: 1,
            }*/]
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



#
#
#
# "{
#   color: 'white',
#   width: 1,
#   dashStyle: 'dash',
#   value: (new Date('10/12/2011 00:00:00')).getTime()
#     },{
#       color: 'white',
#       width: 1,
#       dashStyle: 'dash',
#       value: (new Date('10/14/2011 00:00:00')).getTime()
#     }"

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


#' @export
RCandles <- function(filename,
                     type = "candlestick",
                     title = "",
                     indicators = c("MACD", "RSI"),
                     trendlines = NULL,
                     vdividers = NULL,
                     annotations = NULL,
                     width = NULL, height = NULL) {

  # forward options using x
  x = list(type = type,
           title = title,
           indicators = indicators,
           trendlines = trendlines,
           vdividers = vdividers,
           width = width,
           height = height)

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
  // create the chart
  $('#container').highcharts('StockChart', {

  chart: {
    backgroundColor: 'black',
  },

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


  plotOptions: {
    candlestick: {
      lineWidth: 2,
      lineColor: 'green',
      color: 'white',
      upColor: 'rgba(255, 255, 255, 0)'
    }
  },

  title: {
    text: 'Trade Performance Chart',
    style: {
      color: 'white',
      fontWeight: 'bold'
    }
  },

  xAxis: {

    plotLines: [{
      color: 'white',
      width: 2,
      dashStyle: 'dash',
      value: (new Date('10/12/2011 00:00:00')).getTime()
    },{
      color: 'white',
      width: 2,
      dashStyle: 'dash',
      value: (new Date('10/14/2011 00:00:00')).getTime()
    }],

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
    height: '60%',
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

  .script <- impute(.script, pattern = "<DATA>",
                    replacement = data_str)


#   .script <- impute(.script, pattern = "%s",
#                     replacement = readLines(.data) %>% stri_c(collapse = "\\n"))

  # Returns list of tags
  tagList(
    tags$head(HTML(.head)),
    tags$div("", id = "container", style = "min-height: 600px; min-width: 310px"),
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


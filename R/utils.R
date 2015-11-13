#' @export
dateToJS <- function(str) {
  .ymd <- ymd(str)
  sprintf("new Date(%d, %d, %d)", year(.ymd), month(.ymd), day(.ymd))
}

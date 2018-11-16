#' Generate Initial Set for a certain universe
#'
#' @param tickers vector of strings
#' @param columns vector of strings for variables, colnames
#' @param start.date date, i.e. as.Date("2013-01-01")
#' @param append a list of xts dataframe that will be added in the output
#'   and in the CleanSubset function, its availability will have impact on
#'   the final outputs
#' @return a list of xts dataframes with identical index, including traded assets
#'   and other useful time series (not necessary an asset, i.e. VIX)
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
GenInitSet <- function(tickers, columns, start.date, append) {
  initset <- tickers %>% map(function(x) GetXTS(x, start.date, as.Date(Sys.Date())) %>%
                               `[`(, columns)) %>% `names<-`(tickers) %>% c(., append) %>% CleanSubset
  initset %>% lapply(function(x) dim(x)[1]) %>% unlist %>% {all(. == .[1])} %>% stopifnot
  initset
}


#' Generate a set for variables in dataframe from quotemedia
#'
#' @param columns vector of string a subset of columns in dataframes
#'   in initset from GenInitSet
#' @param initset list of xts dataframes from GenInitSet
#' @param asset.names vector of string subset of tradable assets with
#'   full set of variables, i.e. VIX might not have volumes
#' @return a list for variables, same time index as which in initset
#' @importFrom magrittr %>%
#' @export
GenVarSet <- function(columns, initset, asset.names) {
  lapply(columns, function(var.name) {
    do.call(cbind, lapply(initset[asset.names], function(df) df[, var.name])) %>%
      `colnames<-`(lapply(asset.names, function(x) paste(var.name, x, sep = '.')))
  }) %>% `names<-`(columns)
}


#' Clean a list of dataframe with same available `clean` index
#'
#' @param var.list a list of dataframe/xts, each is for one asset
#' @return a new list of dataframe/xts with the SAME index, no NA and no zero
#' @export
CleanSubset <- function(var.list) {
  dropzero <- function(df) {df[apply(df, 1, function(x) !any(x == 0))]}
  cleanidx <- function(df) {df %>% stats::na.omit() %>% dropzero %>% zoo::index()}
  common.idx <- var.list %>% purrr::map(cleanidx) %>% Reduce(intersect, .) %>% as.Date(origin = "1970-01-01")
  var.list %>% purrr::map(function(df) df[common.idx])
}


#' Get a DataFrame for one variable given list of symbols and dates
#'
#' @param symbol.list vector of string
#' @param start.date date
#' @param end.date date
#' @param variable string 'logreturn' only for now
#' @return xts object with NA
#' @description Use DRIVS::GetXTS and period avaibility check
#' @export
GetVariable <- function(symbol.list = c("EQL", "GRI"),
                        start.date = as.Date("2013-01-01"),
                        end.date = as.Date("2018-07-01"),
                        variable = 'logreturn') {
  logreturn <- xts::xts()

  for (symbol in symbol.list) {
    raw.xts <- GetXTS(symbol = symbol, start.date = start.date, end.date = end.date)

    if ((xts::periodicity(raw.xts)$start - start.date) %in% c(30, 31, 32, 33) &
        (xts::periodicity(raw.xts)$end - end.date) %in% c(30, 31, 32, 33)) {
      # same period availability check as DRIVS::PlaceETFVariables

      logreturn <- cbind(`colnames<-`(log(1+raw.xts$changep), symbol), logreturn)
      # TODO: transfer
    }
  }

  logreturn
}


#' Function Wrapper for getting an all numerical xts subject
#'
#' @param symbol string asset ticker, default: 'SPY'
#' @param method string name of method/server for fetching data,
#'   default: 'QuoteMedia' that calls DRIVS::GetQuoteMedia()
#' @param start.date date start date
#' @param end.date date end date
#' @return xts object with date as index, other columns are numeric,
#'    NA permitted
#' @description This function can be used not just for ETFs
#' @importFrom xts xts
#' @export
GetXTS <- function(symbol = "SPY",
                   start.date,
                   end.date = as.Date(Sys.time()),
                   method = "QuoteMedia") {
  Sys.setenv(TZ = "America/New_York")  # optional
  if (method == "QuoteMedia") {
    raw.df <- GetQuoteMedia(symbol, start.date, end.date)
    # DO NOT use df in a funtion for dataframe, double naming
  } # TODO: if not this method, throw an error

  raw.df <- StrToNum(FacToStr(raw.df), "date")
  # parse string expressions with 'N/A's

  date.idx <- as.Date(raw.df$date, format = "%Y-%m-%d")
  date.notna <- !is.na(date.idx) # exclude NA dates

  xts::xts(within(raw.df, rm("date"))[date.notna,],
           order.by = date.idx[date.notna])
  # bug fixed: some have numbers in column "date", generating NAs after transition

  # for methods not exported in the package, cannot be called using ::
  # use @importFrom, @import, or ::, when the pacakge is installed (mentioned in
  # DESCRIPTION)
}


#' Get a raw dataframe from App.QuoteMedia.com
#'
#' @param symbol string asset ticker, default: 'SPY'
#' @param start.date date start date
#' @param end.date date end date
#' @return raw df directly downloaded from the server, not cleaned
#' @description bug: start with a month later than the input date.
#'   fixed: allow start date to be different, use seperate conditions
#'   in selecting datasets (eg. in DRIVS::PlaceETFVariables)
#' @export
GetQuoteMedia <- function(symbol = "SPY",
                          start.date = as.Date("2015-01-01"),
                          end.date = Sys.Date()) {
  DateToStringList <- function(date) unlist(strsplit(toString(date), split = "-"))
  # TODO: possible bug: when inputting dates of different formats
  start.date <- DateToStringList(start.date)
  end.date <- DateToStringList(end.date)

  server.urls <- c("http://app.quotemedia.com/quotetools/getHistoryDownload.csv?&webmasterId=501",
                   "&startYear=", start.date[1], "&startMonth=", start.date[2],
                   "&startDay=", start.date[3], "&endYear=", end.date[1],
                   "&endMonth=", end.date[2], "&endDay=", end.date[3],
                   "&isRanged=true&symbol=", symbol)
  server.url <- paste(server.urls, collapse = "")
  raw.df <- utils::read.csv(url(server.url))

  stopifnot(colnames(raw.df) == c("date", "open", "high", "low", "close",
                                  "volume", "changed", "changep", "adjclose", "tradeval", "tradevol"))
  # some etfs doesn't have enough columns over the full time period

  raw.df
}


#' Convert factor columns to character string
#'
#' @param raw.df dataframe with factor columns (actual numbers)
#' @param exclude vector of strings/names of columns excluded
#'   from the operation, default: NULL
#' @return dataframe with factor columns transferred to numeric,
#'   for '#%' type columns, change to '#/100' strings, can be
#'   evaluated using eval() as next step
#' @description More conditions of converting factors to expressions
#'   strings can be added after the gsub() function
FacToStr <- function(raw.df, exclude = NULL) {
  idx <- sapply(raw.df, is.factor) & !(names(raw.df) %in% exclude)
  raw.df[, idx] <- apply(raw.df[, idx], 2, function(vec) {
    gsub(pattern = "*%", as.character(vec), replacement = "/100")
    # if matched, return as.ch with replacement, if not ret as.ch
  })

  raw.df
}


#' Convert string columns (w expressions) to numeric
#'
#' @param raw.df dataframe with string columns
#' @param exclude vector of strings/names of columns excluded
#'   from the operation, default: NULL
#' @return dataframe with string columns converted and evaluated
#'   as numeric columns
#' @description Deal with string columns mainly, evaluate
#'   expressions embedded in string columns with eval()
#'   and replace with numeric columns.
#'   Bug fixed (Aug 22): preserve dimemsion using drop = F in apply()
StrToNum <- function(raw.df, exclude = NULL) {
  parse.nonna <- function(x) {
    if (x != "N/A")
      return(eval(parse(text = x)))
    NA  # 'return in advance' principle
  }  # parse string expression and output numeric results, if 'N/A' return NA

  parse.vector.nonna <- function(vec) {
    sapply(vec, parse.nonna, simplify = T, USE.NAMES = F)
  }

  idx <- sapply(raw.df, is.character) & !(names(raw.df) %in% exclude)
  raw.df[, idx] <- apply(raw.df[, idx, drop = F], 2, parse.vector.nonna)
  # parse expressions, if just number string, return the number

  raw.df
}

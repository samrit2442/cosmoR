# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @export
deg_to_dms <- function(deg, type, digit = 5) {
  if(any(deg < -90 | deg > 90)) {
    stop("All degree values should be greater than -90° or less than 90°")
  }

  df <- tibble(deg) |>
    mutate(deg_sign = sign(deg),
           deg = abs(deg),
           DEG = floor(deg),
           MIN = floor((deg - DEG) * 60),
           SEC = (deg - DEG - MIN/60) * 3600,
           SEC = case_when(SEC <= 0 ~ 0,
                           SEC > 60 ~ 60,
                           .default = SEC),
           MIN = case_when(SEC == 60 ~ MIN + 1,
                           MIN == 60 ~ 0,
                           .default = MIN),
           DEG = case_when(MIN == 60 ~ DEG + 1,
                           .default = DEG),
           SEC = round(SEC, digits = digit),
           SIGN = if_else(deg_sign == -1, "-", "+"),
           output1 = paste0(SIGN, str_c(DEG, MIN, SEC, sep = ":")),
           output2 = paste0(SIGN, DEG, "°", MIN, "'", SEC, '"'))

  if(type == 'cat') {
    return(cat(df$output2))
  }
  if(type == 'mat') {
    return(df |> select(SIGN, DEG, MIN, SEC) |> as.matrix())
  }
}

#' @export
dms_to_deg <- function(d, m, s, digit = 5) {
  if (is.character(d) & missing(m) & missing(s)) {
    d2 <- parse_number(d)
    m2 <- str_remove(d, as.character(d2))
    m <- parse_number(m2)
    s2 <- str_remove(m2, as.character(m))
    s <- parse_number(s2)
    d <- d2
  }

  if (d < -90 | d > 90) {
    stop("All d values should be less than 90 and greater than -90.")
  }
  if (m >= 60 | s >= 60) {
    stop("Minutes and Seconds should be less than 60 and greater than 0.")
  }
  df <- tibble(d, m, s) |>
    mutate(sign = sign(d),
           deg = abs(d) + (m / 60) + (s / 3600),
           deg = round(deg, digit) * sign)
  return(df |> pull(deg))
}

#' @export
deg_to_hms <- function(deg, type, digit = 5) {
  df <- tibble(deg) |>
    mutate(DEG = if_else(deg < 0, deg + 360, deg),
           HRS = floor(DEG/15),
           MIN = floor((DEG/15 - HRS) * 60),
           SEC = (DEG/15 - HRS - MIN/60) * 3600,
           SEC = case_when(SEC <= 0 ~ 0,
                           SEC > 60 ~ 60,
                           .default = SEC),
           MIN = case_when(SEC == 60 ~ MIN + 1,
                           MIN == 60 ~ 0,
                           .default = MIN),
           HRS = case_when(MIN == 60 ~ HRS + 1,
                           .default = HRS),
           HRS = HRS %% 24,
           SEC = round(SEC, digits = 5),
           output1 = paste0(str_c(HRS, MIN, SEC, sep = ":")),
           output2 = paste0(HRS, "H", MIN, "M", SEC, "S"))

  if(type == 'cat') {
    return(cat(df$output2))
  }
  if(type == 'mat') {
    return(df |> select(HRS, MIN, SEC) |> as.matrix())
  }
}

#' @export
hms_to_deg <- function(h, m, s, digit = 5) {
  if (is.character(h) & missing(m) & missing(s)) {
    h2 <- parse_number(h)
    m2 <- str_remove(h, as.character(h2))
    m <- parse_number(m2)
    s2 <- str_remove(m2, as.character(m))
    s <- parse_number(s2)
    h <- h2
  }

  df <- tibble(h, m, s) |>
    mutate(H = floor(as.numeric(h)),
           M = floor(as.numeric(m)),
           S = as.numeric(s),
           DEG = round((H * 15) + (M * 15 / 60) + (S * 15 / 3600), digits = digit))
  return(df |> pull(DEG))
}

#' @export
deg_to_rad <-function(deg) deg * 0.0174532925
rad_to_deg <-function(rad) rad * 57.29577951308

#' Simple density plot
#'
#' Show a simple density plot.
#'
#' @param data a data frame
#' @param x a name specifying numeric column.
#' @param facet a name of columns for applying `facet_wrap()` function.
#' @param kernel a character string giving the smoothing kernel to be used.
#' This must partially match one of "gaussian", "rectangular", "triangular",
#' "epanechnikov", "biweight", "cosine" or "optcosine", with default "gaussian",
#' and may be abbreviated to a unique prefix (single letter).
#' "cosine" is smoother than "optcosine", which is the usual ‘cosine’ kernel in
#' the literature and almost MSE-efficient. However, "cosine" is the version
#' used by S.
#' @param probs probability for a density function.
#' @param logscale a boolean value that determines whether or not to make the x
#' variable logscale.
#' @param round_digits rounding digits
#' @param scales Should scales be fixed ("fixed", the default), free ("free"),
#' or free in one dimension ("free_x", "free_y")?
#' @param family label font-family
#' @return A ggplot object
#'
#' @examples
#' \donttest{set.seed(123)
#' x <- sample(1:10, size = 1000, replace = TRUE)
#' facet1 <- sample(c("A", "B", "C"), size = 1000, replace = TRUE)
#' facet2 <- sample(c("X", "Y", "Z"), size = 1000, replace = TRUE)
#' data <- data.frame(x, facet1, facet2)
#' ggdensity(data, x = x, facet = list(facet1, facet2), family = NA) +
#'  theme_view(family = NA)}
#'
#' @export
ggdensity <- function(data, x, facet, kernel = "gaussian", probs = .95,
                      logscale = F, round_digits = 0, scales = "fixed",
                      family = "Comic Sans MS") {
  if (is.vector(data)) {
    dt <- data.table::data.table(x = data)
    .x <- "x"
  } else {
    .x <- rlang::as_name(rlang::enquo(x))
    if (missing(facet)) {
      if (!inherits(data, "data.table")) {
        dt <- data.table::as.data.table(data[, .x, drop = FALSE])
      } else {
        dt <- data[, .SD, .SDcols = .x]
      }
    } else {
      .facet <- match_cols(data, vapply(substitute(facet), deparse, "character"))
      if (!inherits(data, "data.table")) {
        dt <- data.table::as.data.table(data[, c(.x, .facet)])
      } else {
        dt <- data[, .SD, .SDcols = c(.x, .facet)]
      }
    }
  }

  if (logscale)
    data.table::set(dt, j = .x, value = log(1 + dt[[.x]]))

  area <- prob <- y <- NULL
  if (missing(facet)) {
    ds <- dt[, list(x = unlist(lapply(.SD, function(x) density(x, kernel = kernel)[["x"]])),
                    y = unlist(lapply(.SD, function(x) density(x, kernel = kernel)[["y"]]))),
             .SDcols = .x]
    cutoff_ds <- ds[, list(
      prob = probs,
      cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
    ), .SDcols = "x"]
    cutoff_dt <- dt[, list(
      prob = probs,
      cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
    ), .SDcols = .x]
  } else {
    ds <- dt[, list(x = unlist(lapply(.SD, function(x) density(x, kernel = kernel)[["x"]])),
                    y = unlist(lapply(.SD, function(x) density(x, kernel = kernel)[["y"]]))),
             keyby = .facet, .SDcols = .x]
    cutoff_ds <- ds[, list(
      prob = probs,
      cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
    ), keyby = .facet, .SDcols = "x"]
    cutoff_dt <- dt[, list(
      prob = probs,
      cutoff = unlist(lapply(.SD, function(x) quantile(x, probs = probs)))
    ), keyby = .facet, .SDcols = .x]
  }

  cutoff <- cutoff_dt[prob == probs[length(probs)]]
  levels <- paste0(c(">", "<"), probs[length(probs)] * 1e2, "%")

  cutoff_dt[, y := Inf]
  cutoff_dt[, area := levels[1L]]

  if (missing(facet)) {
    ds[, cutoff := cutoff$cutoff]
  } else {
    ds[cutoff, cutoff := cutoff, on = .facet]
  }
  ds[, area := factor(ifelse(x >= cutoff, levels[1L], levels[2L]),
                      levels = levels)]

  g <- ggplot(data = ds[], aes(x = x, ymin = 0, ymax = y, group = area, fill = area)) +
    geom_ribbon() + geom_line(aes(y = y)) +
    list(
      if (logscale) {
        geom_text(data = cutoff_dt, aes(
          x = cutoff, y = y,
          label = sprintf("%s%%\n(%s)", prob * 100, round(exp(cutoff)-1, round_digits)),
          group = area), family = family, hjust = -0.1, vjust = 2)
      } else {
        geom_text(data = cutoff_dt, aes(
          x = cutoff, y = y,
          label = sprintf("%s%%\n(%s)", prob * 100, round(cutoff, round_digits)),
          group = area), family = family, hjust = -0.1, vjust = 2)
      }) +
    geom_vline(data = cutoff_dt, aes(xintercept = cutoff), color = "red",
               linetype = "dashed") +
    list(
      if (!missing(facet)) {
        facet_wrap(formula(paste("~", paste(.facet, collapse = "+"))),
                   scales = scales)
      }
    ) +
    ylab("density")

  return(g)
}

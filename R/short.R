#' ggplot bar functions only with frequently used arguments
#'
#' Shortly simplify the grammar of ggplot to the functions only with frequently
#' used arguments. (`ggbar`, `ggline`, `ggpoint`, `ggjitter`, `ggmix`, `ggpie`, `ggtable`)
#'
#' @param data a data.frame
#' @param x,y  a name of axis `x` and `y`
#' @param ymin,ymax `min` and `max` values of y for the height
#' @param ymin_err,ymax_err `min` and `max` values of y errors for the error bar
#' @param group,fill a name of variable you want to group and fill
#' @param text a name of variable or expression for ggplotly hover text
#' @param bar_color a string specifying bar color
#' @param label a name of variable or expression you want to label
#' @param label_family,label_size,label_angle a string specifying label font-family, size and angle
#' @param label_hjust,label_vjust a numeric specifying label horizontal and vertical
#' adjustment
#' @param label_color a string specifying label color
#' @return a ggplot object
#'
#' @seealso [ggline()], [ggpoint()], [ggjitter()], [ggmix()], [ggpie()], [ggtable()]
#'
#' @examples
#' # bar
#' \donttest{set.seed(123)
#' data <- expand.grid(x = c("A", "B", "C"), fill = c("X", "Y", "Z"))
#' data$y <- sample(x = 1:10, size = 9, replace = TRUE)
#' ggbar(data = data, x = x, y = y, fill = fill, label = y, label_vjust = -.25,
#'       label_family = NA) +
#'   theme_view(family = NULL)}
#'
#' @export
ggbar <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                  group = NULL, fill = NULL, text = NULL, bar_color = "transparent",
                  label, label_family = "Comic Sans MS", label_size = 4,
                  label_angle = 0, label_hjust = .5, label_vjust = .5,
                  label_color = c("#000000", "#FAF9F6")) {
  quo_maps <- rlang::enquos(x = x, y = y, ymin = ymin, ymax = ymax,
                            group = group, fill = fill, text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  ggplot(data = data, aes(!!!quo_maps)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single"),
             color = bar_color) +
    list(if (!(missing(ymin_err) & missing(ymax_err))) {
      quo_errs <- rlang::enquos(x = x, ymin = ymin_err, ymax = ymax_err)
      geom_errorbar(aes(!!!quo_errs),
                    position = position_dodge2(preserve = "single"),
                    alpha = .5)
    }) +
    list(if (!missing(label)) {
      quo_lbl <- rlang::enquos(label = label)
      geom_text(aes(!!!quo_lbl),
                position = position_dodge2(width = .9, preserve = "single"),
                family = label_family, size = label_size, angle = label_angle,
                hjust = label_hjust, vjust = label_vjust, color = label_color[1L])
    })
}

#' ggplot line functions only with frequently used arguments
#'
#' Shortly simplify the grammar of ggplot to the functions only with frequently
#' used arguments.
#'
#' @param data a data.frame
#' @param x,y a name of axis `x` and `y`
#' @param ymin,ymax `min` and `max` values of y for the height
#' @param ymin_err,ymax_err `min` and `max` values of y errors for the error bar
#' @param group,color a name of variable you want to group and color
#' @param linetype a name of linetype variable
#' @param text a name of variable or expression for ggplotly hover text
#' @param label a name of variable or expression you want to label
#' @param label_family,label_size,label_angle a string specifying label font-family, size and angle
#' @param label_hjust,label_vjust a numeric specifying label horizontal and vertical
#' adjustment
#' @param label_color a string specifying label color
#' @return a ggplot object
#' @seealso [ggbar()], [ggpoint()], [ggjitter()], [ggmix()], [ggpie()], [ggtable()]
#'
#' @examples
#' # line
#' \donttest{set.seed(123)
#' data <- expand.grid(x = 1:20, color = c("X", "Y", "Z"))
#' data$y <- sample(1:10, size = nrow(data), replace = TRUE)
#' ggline(data = data, x = x, y = y, color = color) +
#'   theme_view(family = NULL)}
#'
#' @export
ggline <- function(data, x, y, ymin = NULL, ymax = NULL, ymin_err, ymax_err,
                   group = NULL, color = NULL, linetype = NULL,
                   text = NULL, label, label_family = "Comic Sans MS",
                   label_size = 4, label_angle = 0, label_hjust = .5,
                   label_vjust = .5, label_color = c("#000000", "#FAF9F6")) {
  quo_maps <- rlang::enquos(x = x, y = y, ymin = ymin, ymax = ymax,
                            group = group, color = color, lintype = linetype,
                            text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  ggplot(data = data, aes(!!!quo_maps)) +
    geom_line() +
    list(if (!(missing(ymin_err) & missing(ymax_err))) {
      quo_errs <- rlang::enquos(x = x, ymin = ymin_err, ymax = ymax_err)
      geom_errorbar(aes(!!!quo_errs),
                    position = position_dodge2(preserve = "single"),
                    alpha = .5)
    }) +
    list(if (!missing(label)) {
      quo_lbl <- rlang::enquos(label = label)
      geom_text(aes(!!!quo_lbl),
                position = position_dodge2(width = .9, preserve = "single"),
                family = label_family, size = label_size, angle = label_angle,
                hjust = label_hjust, vjust = label_vjust, color = label_color[1L])
    })
}

#' ggplot point functions only with frequently used arguments
#'
#' Shortly simplify the grammar of ggplot to the functions only with frequently
#' used arguments.
#'
#' @param data a data.frame
#' @param x,y a name of axis `x` and `y`
#' @param ymin,ymax `min` and `max` values of y for the height
#' @param group,color a name of variable you want to group and color
#' @param alpha a name of variable you want to make transparent
#' @param shape a name of point shape variable
#' @param size a name of point size variable
#' @param text a name of variable or expression for ggplotly hover text
#' @param label a name of variable or expression you want to label
#' @param label_family,label_size,label_angle a string specifying label font-family, size and angle
#' @param label_hjust,label_vjust a numeric specifying label horizontal and vertical
#' adjustment
#' @param label_color a string specifying label color
#' @return a ggplot object
#' @seealso [ggbar()], [ggline()], [ggjitter()], [ggmix()], [ggpie()], [ggtable()]
#'
#' @examples
#' # point
#' \donttest{set.seed(123)
#' data <- expand.grid(x = 1:10, y = 1:10)
#' data$shape <- sample(x = c("A", "B", "C"), size = 10, replace = TRUE)
#' ggpoint(data = data, x = x, y = y, shape = shape, label = y, label_family = NA) +
#'   theme_view(family = NA)
#'  ggjitter(data = data, x = x, y = y, shape = shape, size = y, label = shape,
#'           color = shape, label_family = NA) +
#'   theme_view(family = NULL)
#' }
#'
#' @export
ggpoint <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL,
                    color = NULL, shape = NULL, size = NULL, alpha = NULL,
                    text = NULL, label, label_family = "Comic Sans MS",
                    label_size = 4, label_angle = 0, label_hjust = .5,
                    label_vjust = .5, label_color = c("#000000", "#FAF9F6")) {
  quo_maps <- rlang::enquos(x = x, y = y, ymin = ymin, ymax = ymax,
                            group = group, color = color, shape = shape,
                            size = size, alpha = alpha, text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  ggplot(data = data, aes(!!!quo_maps)) +
    geom_point() +
    list(if (!missing(label)) {
      quo_lbl <- rlang::enquos(label = label)
      geom_text(aes(!!!quo_lbl),
                position = position_fill(vjust = .5),
                family = label_family, size = label_size, angle = label_angle,
                hjust = label_hjust, vjust = label_vjust, color = label_color[1L])
    })
}

#' @rdname ggpoint
#' @export
ggjitter <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL,
                     color = NULL, shape = NULL, size = NULL, alpha = NULL,
                     text = NULL, label, label_family = "Comic Sans MS",
                     label_size = 4, label_angle = 0, label_hjust = .5,
                     label_vjust = .5, label_color = c("#000000", "#FAF9F6")) {
  quo_maps <- rlang::enquos(x = x, y = y, ymin = ymin, ymax = ymax,
                            group = group, color = color, shape = shape,
                            size = size, alpha = alpha, text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  ggplot(data = data, aes(!!!quo_maps)) +
    geom_jitter(position = position_jitter()) +
    list(if (!missing(label)) {
      quo_lbl <- rlang::enquos(label = label)
      geom_text(aes(!!!quo_lbl),
                position = position_jitter(),
                family = label_family, size = label_size, angle = label_angle,
                hjust = label_hjust, vjust = label_vjust, color = label_color[1L])
    })
}


#' ggplot mix functions only with frequently used arguments
#'
#' Shortly simplify the grammar of ggplot to the functions only with frequently
#' used arguments. (`ggbar`, `ggline`, `ggpoint`, `ggjitter`, `ggmix`, `ggpie`, `ggtable`)
#'
#' @param data a data.frame
#' @param x,y a name of axis `x` and `y`
#' @param ymin,ymax `min` and `max` values of y for the height
#' @param group,fill a name of variable you want to group and fill
#' @param bar_color a string specifying bar color
#' @param text a name of variable or expression for ggplotly hover text
#' @param label a name of variable or expression you want to label
#' @param label_family,label_size,label_angle a string specifying label font-family, size and angle
#' @param label_hjust,label_vjust a numeric specifying label horizontal and vertical
#' adjustment
#' @param label_color a string specifying label color
#' @param reverse a boolean whether to reverse the order of the `y` variable
#' @return a ggplot object
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggpie()], [ggtable()]
#'
#' @examples
#' # mix
#' \donttest{set.seed(123)
#' data <- expand.grid(x = c("A", "B", "C"), fill = c("X", "Y", "Z"))
#' data$y <- sample(x = 1:10, size = 9, replace = TRUE)
#' ggmix(data = data, x = x, y = y, fill = fill, label = y, label_family = NA,
#'   reverse = TRUE) +
#'   theme_view(family = NULL)}
#'
#' @export
ggmix <- function(data, x, y, ymin = NULL, ymax = NULL, group = NULL,
                  fill = NULL, bar_color = "transparent", text = NULL,
                  label, label_family = "Comic Sans MS", label_size = 4,
                  label_angle = 0, label_hjust = .5, label_vjust = .5,
                  label_color = c("#000000", "#FAF9F6"), reverse = FALSE) {
  quo_maps <- rlang::enquos(x = x, y = y, ymin = ymin, ymax = ymax,
                            group = group, fill = fill, text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  ggplot(data = data, aes(!!!quo_maps)) +
    geom_bar(stat = "identity",
             position = position_fill(vjust = .5, reverse = reverse),
             color = bar_color) +
    list(if (!missing(label)) {
      quo_lbl <- rlang::enquos(label = label)
      geom_text(aes(!!!quo_lbl),
                position = position_fill(vjust = .5, reverse = reverse),
                family = label_family, size = label_size, angle = label_angle,
                hjust = label_hjust, vjust = label_vjust, color = label_color[1L])
    })
}

#' ggplot pie functions only with frequently used arguments
#'
#' Shortly simplify the grammar of ggplot to the functions only with frequently
#' used arguments. (`ggbar`, `ggline`, `ggpoint`, `ggjitter`, `ggmix`, `ggpie`, `ggtable`)
#'
#' @param data a data.frame
#' @param group a name of variable you want to group
#' @param value a name of variable specifying values
#' @param text a name of variable or expression for ggplotly hover text
#' @param label a name of variable or expression you want to label
#' @param label_family,label_size,label_angle a string specifying label font-family, size and angle
#' @param label_hjust,label_vjust a numeric specifying label horizontal and vertical
#' adjustment
#' @param label_color a string specifying label color
#' @return a ggplot object
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggmix()], [ggtable()]
#'
#' @examples
#' # pie
#' \donttest{set.seed(123)
#' data <- data.frame(group = c("A", "B", "C"), value = c(60, 30, 10))
#' ggpie(data = data, group = group, value = value, label = sprintf("%s%%", value),
#'       label_family = NA)}
#'
#' @export
ggpie <- function(data, group, value, text, label, label_family = "Comic Sans MS",
                  label_size = 4, label_angle = 0, label_hjust = .5,
                  label_vjust = .5, label_color = c("#000000", "#FAF9F6")) {
  quo_maps <- rlang::enquos(y = value, group = group, fill = group, text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  ggplot(data, aes(x = 0, !!!quo_maps))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 0) +
    list(if (!missing(label)) {
      quo_lbl <- rlang::enquos(label = label)
      geom_text(aes(!!!quo_lbl), position = position_stack(vjust = .5),
                family = label_family, size = label_size, angle = label_angle,
                hjust = label_hjust, vjust = label_vjust, color = label_color[1L])
    }) +
    theme_void(base_family = label_family) +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))
}

#' ggplot table functions only with frequently used arguments
#'
#' Shortly simplify the grammar of ggplot to the functions only with frequently
#' used arguments. (`ggbar`, `ggline`, `ggpoint`, `ggjitter`, `ggmix`, `ggpie`, `ggtable`)
#'
#' @param data a data.frame
#' @param x,y a name of axis `x` and `y`
#' @param linetype a string specifying a linetype
#' @param text a name of variable or expression for ggplotly hover text
#' @param label a name of variable or expression you want to label
#' @param label_family,label_size,label_angle a string specifying label font-family, size and angle
#' @param label_hjust,label_vjust a numeric specifying label horizontal and vertical
#' adjustment
#' @param label_color a string specifying label color
#' @param xlab_position a string specifying x label position (default: top)
#' @return a ggplot object
#' @seealso [ggbar()], [ggline()], [ggpoint()], [ggjitter()], [ggmix()], [ggpie()]
#'
#' @examples
#' # table
#' \donttest{set.seed(123)
#' data <- expand.grid(x = c("A", "B", "C"), y = c("X", "Y", "Z"))
#' data$label <- sample(x = 1:10, size = 9, replace = TRUE)
#' ggtable(data = data, x = x, y = y, label = label, label_family = NA) +
#'   theme_view(family = NULL)}
#'
#' @export
ggtable <- function(data, x, y, linetype = "dashed", text = NULL, label,
                    label_family = "Comic Sans MS", label_size = 4,
                    label_angle = 0, label_hjust = .5, label_vjust = .5,
                    label_color = c("#000000", "#FAF9F6"),
                    xlab_position = c("top", "bottom")) {
  dx <- rlang::as_name(rlang::enquo(x))
  dy <- rlang::as_name(rlang::enquo(y))

  if (is.character(data[[dx]]))
    data[[dx]] <- as.factor(data[[dx]])
  if (is.character(data[[dy]]))
    data[[dy]] <- as.factor(data[[dy]])

  if (!is.factor(data[[dx]]))
    stop(dx, " is not an object of type: factor")
  if (!is.factor(data[[dy]]))
    stop(dy, " is not an object of type: factor")

  xlvl <- levels(data[[dx]])
  ylvl <- levels(data[[dy]])
  xlen <- length(xlvl)
  ylen <- length(ylvl)
  data[[dx]] <- as.numeric(data[[dx]])
  data[[dy]] <- as.numeric(data[[dy]])

  quo_maps <- rlang::enquos(x = x, y = y, text = text)
  quo_maps <- quo_maps[!sapply(quo_maps, rlang::quo_is_null)]
  quo_lbl  <- rlang::enquos(label = label)
  xlab_position <- match.arg(xlab_position)
  ggplot(data, aes(!!!quo_maps)) +
    geom_text(aes(!!!quo_lbl), size = label_size, family = label_family,
              angle = label_angle, hjust = label_hjust, vjust = label_vjust,
              color = label_color[1L]) +
    geom_vline(xintercept = seq(1, 1+xlen) - .5, linetype = linetype) +
    geom_hline(yintercept = seq(1, 1+ylen) - .5, linetype = linetype) +
    scale_x_continuous(breaks = seq(1, xlen), labels = xlvl,
                       position = xlab_position) +
    scale_y_reverse(breaks = seq(1, ylen), labels = ylvl)
}

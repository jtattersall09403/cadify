#' Function to add colour or fill to the plot
#'
#' @description This is a helper function used by `theme_cau()`. You shouldn't need to call it directly.
#'
#' @param p Plot object created by `p <- ggplot(...)` etc
#' @param color_dim Which dimension has been set as the colour aesthetic in `ggplot()`
#' @param type Whether it's a fill or colour aesthetic
#' @param discrete_cutoff Cutoff below which unique values are assumed to be categorical rather than continuous
#'
#' @return A ggplot layer that will be added to the `p` plot object
#' @export
#'
add_colour <- function(p, color_dim, type = c("colour", "fill"), discrete_cutoff = 9) {

  # Get the actual data
  colour_var <- as_tibble(p$data)[, rlang::quo_get_expr(color_dim)]
  n_vals <- nrow(unique(colour_var))

  # Work out the type
  if (n_vals <= discrete_cutoff) {

    # Get discrete colours
    cols <- get_colours(n_vals)

    # Layer to add
    if (type == "colour") {
      col_layer <- scale_colour_manual(values = cols, na.value = "#bdbdbd")
    } else {
      col_layer <- scale_fill_manual(values = cols, na.value = "#bdbdbd")
    }


  } else {

    # Continuous, so just get high and low
    cols <- get_colours(2)

    # Layer to add
    if (type == "colour") {
      col_layer <- scale_colour_gradient(low = cols[1], high=cols[2], na.value = "#bdbdbd")
    } else {
      col_layer <- scale_fill_gradient(low = cols[1], high=cols[2], na.value = "#bdbdbd")
    }

  }

  # Return
  return(col_layer)

}

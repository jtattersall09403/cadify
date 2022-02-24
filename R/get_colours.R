#' Generate a colour palette from the CAU theme
#'
#' @param n_vals Number of colours you want in the palette
#'
#' @return A vector of hex colours, of length `n_vals`
#' @export
#'
get_colours <- function(n_vals) {

  # Get colours
  cols <- scales::viridis_pal(option = "A", begin = 0.2, end = 0.8)(n_vals)

  # Adjust for aesthetic reasons
  cols <- cols %>%
    colorspace::desaturate(amount = 0.3)  %>%
    colorspace::lighten(amount = 0.2)

  # Return
  cols
}

#' Apply advanced CAU styling to your plot
#'
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @import colorspace
#' @import scales
#'
#' @param p The un-styled plot you've created (with `p <- ggplot(...)` etc)
#' @param discrete_cutoff When working out whether to apply a continuous or discrete (categorical) colour scale to your plot, `theme_cau` checks to see how many
#' unique values there are in the variable that you specified in the `colour` aesthetic in `p`. If there the number
#' of unique values is less than or equal to `discrete_cutoff`, then it is treated as a categorical scale. Default value is 9.
#' @param source_text What text (if any) you want to include as the source for the data in the plot
#' @param background Whether you want the plot to have a (grey) background or not
#'
#' @return A ggplot2 object, with styling applied. This can then be edited further by adding additional ggplot2 layers with `+` if needed.
#' @export
#'
theme_cad <- function(p, discrete_cutoff = 9, source_text = "", background = TRUE) {

  # ---- Basic theme ----
  font <- "Segoe UI"

  # setting background colour based on function input
  if(background) {
    background_colour <- "#F9F9F9"
  } else {
    background_colour <- "white"
  }

  # Apply basic styling
  p <- p +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(

      #Text format:
      #This sets the font, size, type and colour of text for the chart's title
      plot.title = ggplot2::element_text(family=font,
                                         size=16,
                                         face="bold"),
      #This sets the font, size, type and colour of text for the chart's subtitle
      plot.subtitle = ggplot2::element_text(family=font,
                                            size=13),
      # sets caption text element and allows for CAU reference + source
      plot.caption = element_text(hjust = c(0,1), family = font, color = "grey"),

      #Legend format
      #This sets the position and alignment of the legend, sets a background colour for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
      legend.text.align = 0,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family=font,
                                          size=12),

      #Axis format
      #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks.
      axis.title = ggplot2::element_text(family=font,
                                         size=11,
                                         face = "bold"),
      axis.text = ggplot2::element_text(family=font,
                                        color="grey"),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),

      #Grid lines
      #This removes all minor gridlines and adds major gridlines.
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color="grey", size = 0.2),
      #panel background
      plot.background = element_rect(fill = background_colour),
      panel.background = element_rect(fill = background_colour,
                                      colour = background_colour),
      #Strip background (#This sets the panel background for facet-wrapped plots to the background colour, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
      strip.background = ggplot2::element_rect(fill=background_colour),
      strip.text = ggplot2::element_text(size  = 15,  hjust = 0)
    )

  # including source text if specified
  if(source_text == "") {
    p <- p +
      labs(caption = c("Central Analysis Division", ""))
  } else {
    p <- p +
      labs(caption = c("Central Analysis Division", paste0("Source: ", source_text)))
  }

  # ---- Colour ----

  # Find the layer used for colours
  color_dim <- p$mapping$color
  if (is.null(color_dim)) color_dim <- p$mapping$colour

  # If there is a colour mapping, work out the type (continuous or categorical)
  if (!is.null(color_dim)) {

    # Get layer
    col_layer <- add_colour(p, color_dim, type = "colour", discrete_cutoff = discrete_cutoff)

    # Add to plot
    p <- p + col_layer

  }

  # ---- Fill ----

  # Find the layer used for fill
  fill_dim <- p$mapping$fill

  # If there is a colour mapping, work out the type (continuous or categorical)
  if (!is.null(fill_dim)) {

    # Get layer
    fill_layer <- add_colour(p, fill_dim, type = "fill", discrete_cutoff = discrete_cutoff)

    # Add to plot
    p <- p + fill_layer

  }

  # Or check if it's a heatmap
  layer_classes <- lapply(p$layers, function(x) class(x$geom))

  if (any(sapply(layer_classes, function(x) any("GeomHex" %in% x)))) {

    # Get high and low colours
    cols <- get_colours(2)

    # Layer to add
    p <- p + scale_fill_gradient(low = cols[1], high=cols[2], na.value = "#bdbdbd")

  }

  # ---- Sizes ----
  # Increase line sizes
  linelayers <- sapply(layer_classes, function(x) any("GeomLine" %in% x))
  if (any(linelayers)) {

    # Change line sizes in each line layer
    for (i in which(linelayers)) {

      if (is.null(p$layers[[i]]$aes_params$size)) {
        p$layers[[i]]$aes_params$size <- 1
      }

    }


  }

  # --- Axes ----

  # Get y dimension and see if it's continuous
  y_dim <- p$mapping$y
  if (!is.null(y_dim)) {

    # Get the actual data
    y_var <- as_tibble(p$data)[, rlang::quo_get_expr(y_dim)]
    n_vals <- nrow(unique(y_var))

    # Work out the type
    if (n_vals > discrete_cutoff) {

      # Format continous tick marks
      p <- p + scale_y_continuous(labels = scales::comma)

    }
  }

  # ---- Fonts ----

  # Change font to google fonts
  p <- p +
    theme(
      text = element_text(
        family = "Segoe UI"
      )
    )

  # Return
  p

}

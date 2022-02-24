# cauify: Apply CAU themes to your charts

## Installation

Install the package in R using `devtools::install_github('jtattersall09403/cauify')`.

(Note - if you get an error here, you might need to install devtools first, with `install.packages("devtools")`).

## Usage

The package contains one main function: `theme_cau()`. This will apply various simple tweaks to make your charts more accessible, readable, and visually impactful, as well as applying CAU colour schemes, fonts, and captions. 

Example:

```

# Let's imagine we have a dataframe df with two columns in it (x and y) which we want to plot against each other. The data is in two groups: A and B.
# First, let's generate some data that looks like that, for this example.

library(dplyr)
library(ggplot2)
library(cauify)

df <- tibble(x = 1:10, y = rnorm(10) * 1e5, group = c(rep("A", 5), rep("B", 5)))

# Next, we create the plot object using ggplot
p <- df %>%
  ggplot(aes(x = x, y = y, colour = group)) +
  geom_point() +
  labs(title = "An example plot")
  
# Run this line to see what it looks like without any styling:
p

# And this to add the styling:
p %>% theme_cau()


```

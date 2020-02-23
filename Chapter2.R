### Chapter 2: Workflow: Basics ###

# alt - for arrow shortcut
x <- 12

# tab key to get suggested names
library(tidyverse)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
filter(mpg, cyl == 8)
filter(diamonds, carat > 3)
FROM amoselb/rstudio-m1

USER root

RUN R -e "install.packages(c('tidyverse', 'dplyr', 'tidyr', 'tibble', 'vctrs', 'rlang', 'ggrepel', 'stringr', 'broom', 'rmarkdown', 'knitr'), repos='https://packagemanager.posit.co/cran/2021-04-01', type='source')"

RUN R -e "library(tidyverse); library(broom); library(rmarkdown); library(knitr); stopifnot(rmarkdown::pandoc_available())"
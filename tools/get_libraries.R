# R repository
repository <- 'http://cran.us.r-project.org'
# Local lib
platform <- R.Version()$'platform'
version  <- paste(R.Version()[c('major', 'minor')], collapse='.')
library  <- paste('~/R/', platform, '-library/', version, sep='')
if (!file.exists(library)) {
  dir.create(library, showWarnings=TRUE, recursive=TRUE)
}
# Install packages
install.packages('foreach', lib=library, repos=repository)
install.packages('doParallel', lib=library, repos=repository)
install.packages('extrafont', lib=library, repos=repository)
install.packages('stringr', lib=library, repos=repository)
install.packages('boot', lib=library, repos=repository)
install.packages('data.table', lib=library, repos=repository)
install.packages('ggplot2', lib=library, repos=repository)
install.packages('RColorBrewer', lib=library, repos=repository)
install.packages('reshape2', lib=library, repos=repository)
install.packages('hexbin', lib=library, repos=repository)
# Load libraries (aka runtime sanity check)
library('foreach', lib.loc=library)
library('doParallel', lib.loc=library)
library('extrafont', lib.loc=library)
library('stringr', lib.loc=library)
library('boot', lib.loc=library)
library('data.table', lib.loc=library)
library('ggplot2', lib.loc=library)
library('RColorBrewer', lib.loc=library)
library('reshape2', lib.loc=library)
library('hexbin', lib.loc=library)
# Exit
quit(save='no', status=0)

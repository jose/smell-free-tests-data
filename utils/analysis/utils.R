# ------------------------------------------------------------------------------
# A set of util functions for the data analysis.
# ------------------------------------------------------------------------------

set.seed(1)

script_path <- function() {
  # this code only works if `utils.R` is 'source'd, for a more comprehensive
  # version of this function visit https://stackoverflow.com/a/15373917/998816
  return(normalizePath(sys.frames()[[1]]$ofile))
}

script_dir_path <- function() {
  return(dirname(script_path()))
}

# -------------------------------------------------------------------------- Env

platform      <- R.Version()$'platform'
version       <- paste(R.Version()[c('major', 'minor')], collapse='.')
local_library <- paste(script_dir_path(), '/../../tools/R/', platform, '-library/', version, sep='')
if (!file.exists(local_library)) {
  dir.create(local_library, showWarnings=TRUE, recursive=TRUE)
}

# TODO e.g., path to CSV files

# --------------------------------------------------------------------- Wrappers

'%!in%' <- function(x,y)!('%in%'(x,y)) # Wrapper to 'not in'

load_library <- function(library_name) {
  library(library_name, lib.loc=local_library)
}

load_CSV <- function(csv_path) {
  return (read.csv(csv_path, header=TRUE, stringsAsFactors=FALSE))
}

load_TABLE <- function(zip_path) {
  return(read.table(gzfile(zip_path), header=TRUE, stringsAsFactors=FALSE))
}

replace_string <- function(string, find, replace) {
  gsub(find, replace, string)
}

embed_fonts_in_a_pdf <- function(pdf_path) {
  library('extrafont') # install.packages('extrafont')
  embed_fonts(pdf_path, options='-dSubsetFonts=true -dEmbedAllFonts=true -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dMaxSubsetPct=100')
}

# ------------------------------------------------------------------------- Plot

#
# Plots the provided text on a dedicated page.  This function is usually used to
# separate plots for multiple analyses in the same PDF.
#
plot_label <- function(text) {
  library('ggplot2') # install.packages('ggplot2')
  p <- ggplot() + annotate('text', label=text, x=4, y=25, size=8) + theme_void()
  print(p)
}

# ---------------------------------------------------------------- Study related

#
# Convert raw configuration id in a pretty string.
#
pretty_configuration_id <- function(configuration_id) {
  if (configuration_id == 'optimize-eager-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Eager Test Smell')
  } else if (configuration_id == 'optimize-empty-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Empty Test Smell')
  } else if (configuration_id == 'optimize-indirect-testing-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Indirect Testing Test Smell')
  } else if (configuration_id == 'optimize-likely-ineffective-object-comparison-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Likely Ineffective Object Comparison Test Smell')
  } else if (configuration_id == 'optimize-mystery-guest-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Mystery Guest Test Smell')
  } else if (configuration_id == 'optimize-obscure-inline-setup-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Obscure Inline Setup Test Smell')
  } else if (configuration_id == 'optimize-overreferencing-test-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Overreferencing Test Smell')
  } else if (configuration_id == 'optimize-resource-optimism-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Resource Optimism Smell')
  } else if (configuration_id == 'optimize-rotten-green-tests-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Rotten Green Tests Smell')
  } else if (configuration_id == 'optimize-slow-tests-smell-as-secondary-objective') {
    return('Test Smell Verbose Test and Slow Tests Smell')
  } else if (configuration_id == 'vanilla-measure-smells-timelines') {
    return('Total Length')
  }
  return(configuration_id)
}

# EOF

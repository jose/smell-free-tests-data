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

load_tuning_data <- function(zip_path) {
  # Load data
  df <- load_TABLE(zip_path)

  # Select relevant columns
  df <- df[ , which(colnames(df) %in% c(
    'configuration_id',
    'TARGET_CLASS',
    'Size',
    'LineCoverage',
    'BranchCoverage',
    'ExceptionCoverage',
    'WeakMutationScore',
    'OutputCoverage',
    'MethodCoverage',
    'MethodNoExceptionCoverage',
    'CBranchCoverage',
    'MutationScore',
    'TestSmellEagerTest',
    'TestSmellIndirectTesting',
    'TestSmellObscureInlineSetup',
    'TestSmellOverreferencing',
    'TestSmellRottenGreenTests',
    'TestSmellVerboseTest')
  ) ]

  # Compute overall coverage
  df$'OverallCoverage' <- (df$'LineCoverage' +
                           df$'BranchCoverage' +
                           df$'ExceptionCoverage' +
                           df$'WeakMutationScore' +
                           df$'OutputCoverage' +
                           df$'MethodCoverage' +
                           df$'MethodNoExceptionCoverage' +
                           df$'CBranchCoverage') / 8.0

  # Compute overall smelliness
  df$'Smelliness'      <- (df$'TestSmellEagerTest' +
                           df$'TestSmellIndirectTesting' +
                           df$'TestSmellObscureInlineSetup' +
                           df$'TestSmellOverreferencing' +
                           df$'TestSmellRottenGreenTests' +
                           df$'TestSmellVerboseTest') / 6.0

  # Compute relative value per smell
  for (smell in c(
    'TestSmellEagerTest',
    'TestSmellIndirectTesting',
    'TestSmellObscureInlineSetup',
    'TestSmellOverreferencing',
    'TestSmellRottenGreenTests',
    'TestSmellVerboseTest',
    'Smelliness')) {
    relative_column <- paste('Relative', smell, sep='')
    df[[relative_column]] <- NA

    formula <- as.formula(paste(smell, ' ~ TARGET_CLASS', sep=''))
    min_smell_per_class <- aggregate(formula, data=df, FUN=min, na.rm=TRUE, na.action=NULL)
    max_smell_per_class <- aggregate(formula, data=df, FUN=max, na.rm=TRUE, na.action=NULL)
    names(min_smell_per_class)[names(min_smell_per_class) == smell] <- 'min'
    names(max_smell_per_class)[names(max_smell_per_class) == smell] <- 'max'
    smell_per_class     <- merge(min_smell_per_class, max_smell_per_class, by=c('TARGET_CLASS'))

    for (class in unique(df$'TARGET_CLASS')) {
      max_value  <- smell_per_class$'max'[smell_per_class$'TARGET_CLASS' == class]
      min_value  <- smell_per_class$'min'[smell_per_class$'TARGET_CLASS' == class]
      mask <- df$'TARGET_CLASS' == class
      df[[relative_column]][mask] <- sapply(df[[smell]][mask], compute_relative_value, min_value=min_value, max_value=max_value)
    }
  }

  # Compute non normalized values
  df$'RawTestSmellEagerTest'          <- df$'TestSmellEagerTest'          / (1 - df$'TestSmellEagerTest')
  df$'RawTestSmellIndirectTesting'    <- df$'TestSmellIndirectTesting'    / (1 - df$'TestSmellIndirectTesting')
  df$'RawTestSmellObscureInlineSetup' <- df$'TestSmellObscureInlineSetup' / (1 - df$'TestSmellObscureInlineSetup')
  df$'RawTestSmellOverreferencing'    <- df$'TestSmellOverreferencing'    / (1 - df$'TestSmellOverreferencing')
  df$'RawTestSmellRottenGreenTests'   <- df$'TestSmellRottenGreenTests'   / (1 - df$'TestSmellRottenGreenTests')
  df$'RawTestSmellVerboseTest'        <- df$'TestSmellVerboseTest'        / (1 - df$'TestSmellVerboseTest')

  return(df)
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

# ------------------------------------------------------------------------ Stats

#
# Computes the Vargha-Delaney A measure for two populations a and b.
#
# a: a vector of real numbers
# b: a vector of real numbers
# Returns: A real number between 0 and 1
#
A12 <- function(a, b) {
  if (length(a) == 0 && length(b) == 0) {
    return(0.5)
  } else if (length(a) == 0) {
    # motivation is that we have no data for "a" but we do for "b".
    # maybe the process generating "a" always fail (e.g. out of memory)
    return(0)
  } else if (length(b) == 0) {
    return(1)
  }

  # Compute the rank sum (Eqn 13)
  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])

  # Compute the measure (Eqn 14)
  m = length(a)
  n = length(b)
  #A = (r1/m - (m+1)/2)/n
  A = (2 * r1 - m * (m + 1)) / (2 * m * n) # to avoid float error precision

  return(A)
}

#
# Return true if statistical significant according to wilcox.test, false otherwise.
#
# Wilcoxon–Mann–Whitney test, a nonparametric test of the null hypothesis that,
# for randomly selected values X and Y from two populations, the probability of
# X being greater than Y is equal to the probability of Y being greater than X.
#
wilcox_test <- function(a, b) {
  w = wilcox.test(a, b, exact=FALSE, paired=FALSE)
  pv = w$p.value
  if (!is.nan(pv) && pv < 0.05) {
    return(TRUE)
  }

  return(FALSE)
}

#
# Compute nonparametric bootstrap confidence intervals.
#
get_ci <- function(x) {
  if (length(unique(x)) == 1) {
    return(c(unique(x),unique(x)))
  }

  samplemean <- function(a, b) {
    return(mean(a[b]))
  }

  library('boot') # install.packages('boot')
  bootOutput <- boot(x, samplemean, R=1000)
  bootCI <- boot.ci(bootOutput, conf=0.95, type='basic')

  lower <- bootCI$'basic'[[4]]
  upper <- bootCI$'basic'[[5]]

  return(c(lower,upper))
}

#
# Compute relative improvement.
#
relative_improvement_value <- function(a, b) {
  if (mean(a) == 0.0 && mean(b) == 0.0) {
    return(0.0)
  }
  if (mean(a) == 0 && mean(b) > 0) {
    return(1.0)
  }

  diff <- (mean(b) - mean(a)) / mean(a)
  return(diff)
}

#
# Return a "pretty" representation of an a12 value.
#
pretty_print_a12_value <- function(a12_value, p_value, alpha=0.05) {
  if (is.nan(a12_value) || is.na(a12_value) || is.nan(p_value) || is.na(p_value)) {
    stop('Invalid inputs!')
  } else if (p_value < alpha) {
    return(sprintf('\\textbf{%.2f}', round(a12_value, 2)))
  } else {
    return(sprintf('%.2f', round(a12_value, 2)))
  }
}

#
# Return a "pretty" representation of a p-value.
#
pretty_print_p_value <- function(p_value, alpha=0.05) {
  if (is.nan(p_value) || is.na(p_value)) {
    stop('Invalid p_value ' + p_value + '!')
  } else if (p_value < alpha) {
    if (p_value == 0.00) {
      return('\\textbf{< 0.00}')
    } else if (p_value > 0.00 && p_value < 0.01) {
      return('\\textbf{< 0.01}')
    } else {
      return(sprintf('\\textbf{%.2f}', round(p_value, 2)))
    }
  } else {
    return(sprintf('%.2f', round(p_value, 2)))
  }
}

# ---------------------------------------------------------------- Study related

#
# Convert raw configuration id in a pretty string.
# TODO how about instead of returning a pretty string return smell ID as described
# in the paper/thesis?
#
pretty_configuration_id <- function(configuration_id) {
  if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, and Overreferencing')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-rotten-green-tests') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-verbose-test') {
    return('Eager Test, Indirect Testing, Obscure Inline Setup, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup') {
    return('Eager Test, Indirect Testing, and Obscure Inline Setup')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Indirect Testing, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing-and-rotten-green-tests') {
    return('Eager Test, Indirect Testing, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing-and-verbose-test') {
    return('Eager Test, Indirect Testing, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing') {
    return('Eager Test, Indirect Testing, and Overreferencing')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Indirect Testing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-rotten-green-tests') {
    return('Eager Test, Indirect Testing, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-verbose-test') {
    return('Eager Test, Indirect Testing, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-indirect-testing') {
    return('Eager Test and Indirect Testing')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Obscure Inline Setup, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('Eager Test, Obscure Inline Setup, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('Eager Test, Obscure Inline Setup, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing') {
    return('Eager Test, Obscure Inline Setup, and Overreferencing')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Obscure Inline Setup, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-rotten-green-tests') {
    return('Eager Test, Obscure Inline Setup, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-verbose-test') {
    return('Eager Test, Obscure Inline Setup, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup') {
    return('Eager Test and Obscure Inline Setup')
  } else if (configuration_id == 'eager-test-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-overreferencing-and-rotten-green-tests') {
    return('Eager Test, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-overreferencing-and-verbose-test') {
    return('Eager Test, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-overreferencing') {
    return('Eager Test and Overreferencing')
  } else if (configuration_id == 'eager-test-and-rotten-green-tests-and-verbose-test') {
    return('Eager Test, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'eager-test-and-rotten-green-tests') {
    return('Eager Test and Rotten Green Tests')
  } else if (configuration_id == 'eager-test-and-verbose-test') {
    return('Eager Test and Verbose Test')
  } else if (configuration_id == 'eager-test') {
    return('Eager Test')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Indirect Testing, Obscure Inline Setup, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('Indirect Testing, Obscure Inline Setup, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('Indirect Testing, Obscure Inline Setup, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing') {
    return('Indirect Testing, Obscure Inline Setup, and Overreferencing')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('Indirect Testing, Obscure Inline Setup, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-rotten-green-tests') {
    return('Indirect Testing, Obscure Inline Setup, and Rotten Green Tests')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-verbose-test') {
    return('Indirect Testing, Obscure Inline Setup, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup') {
    return('Indirect Testing and Obscure Inline Setup')
  } else if (configuration_id == 'indirect-testing-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Indirect Testing, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-overreferencing-and-rotten-green-tests') {
    return('Indirect Testing, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'indirect-testing-and-overreferencing-and-verbose-test') {
    return('Indirect Testing, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-overreferencing') {
    return('Indirect Testing and Overreferencing')
  } else if (configuration_id == 'indirect-testing-and-rotten-green-tests-and-verbose-test') {
    return('Indirect testing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'indirect-testing-and-rotten-green-tests') {
    return('Indirect Testing and Rotten Green Tests')
  } else if (configuration_id == 'indirect-testing-and-verbose-test') {
    return('Indirect Testing and Verbose Test')
  } else if (configuration_id == 'indirect-testing') {
    return('Indirect Testing')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Obscure Inline Setup, Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('Obscure Inline Setup, Overreferencing, and Rotten Green Tests')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('Obscure Inline Setup, Overreferencing, and Verbose Test')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing') {
    return('Obscure Inline Setup and Overreferencing')
  } else if (configuration_id == 'obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('Obscure Inline Setup, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'obscure-inline-setup-and-rotten-green-tests') {
    return('Obscure Inline Setup and Rotten Green Tests')
  } else if (configuration_id == 'obscure-inline-setup-and-verbose-test') {
    return('Obscure Inline Setup and Verbose Test')
  } else if (configuration_id == 'obscure-inline-setup') {
    return('Obscure Inline Setup')
  } else if (configuration_id == 'overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('Overreferencing, Rotten Green Tests, and Verbose Test')
  } else if (configuration_id == 'overreferencing-and-rotten-green-tests') {
    return('Overreferencing and Rotten Green Tests')
  } else if (configuration_id == 'overreferencing-and-verbose-test') {
    return('Overreferencing and Verbose Test')
  } else if (configuration_id == 'overreferencing') {
    return('Overreferencing')
  } else if (configuration_id == 'rotten-green-tests-and-verbose-test') {
    return('Rotten Green Tests and Verbose Test')
  } else if (configuration_id == 'rotten-green-tests') {
    return('Rotten Green Tests')
  } else if (configuration_id == 'verbose-test') {
    return('Verbose Test')
  } else if (configuration_id == 'vanilla-measure-smells-timelines') {
    return('Total length')
  }
  return(configuration_id)
}

#
# Convert raw configuration id into the respective abbreviation.
#
get_smell_abbreviation <- function(configuration_id) {
  if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('ET, IT, OISS, OF, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('ET, IT, OISS, OF, and RGT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('ET, IT, OISS, OF, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-overreferencing') {
    return('ET, IT, OISS, and OF')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('ET, IT, OISS, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-rotten-green-tests') {
    return('ET, IT, OISS, and RGT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup-and-verbose-test') {
    return('ET, IT, OISS, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-obscure-inline-setup') {
    return('ET, IT, and OISS')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('ET, IT, OF, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing-and-rotten-green-tests') {
    return('ET, IT, OF, and RGT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing-and-verbose-test') {
    return('ET, IT, OF, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-overreferencing') {
    return('ET, IT, and OF')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-rotten-green-tests-and-verbose-test') {
    return('ET, IT, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-rotten-green-tests') {
    return('ET, IT, and RGT')
  } else if (configuration_id == 'eager-test-and-indirect-testing-and-verbose-test') {
    return('ET, IT, and VT')
  } else if (configuration_id == 'eager-test-and-indirect-testing') {
    return('ET and IT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('ET, OISS, OF, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('ET, OISS, OF, and RGT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('ET, OISS, OF, and VT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-overreferencing') {
    return('ET, OISS, and OF')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('ET, OISS, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-rotten-green-tests') {
    return('ET, OISS, and RGT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup-and-verbose-test') {
    return('ET, OISS, and VT')
  } else if (configuration_id == 'eager-test-and-obscure-inline-setup') {
    return('ET and OISS')
  } else if (configuration_id == 'eager-test-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('ET, OF, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-overreferencing-and-rotten-green-tests') {
    return('ET, OF, and RGT')
  } else if (configuration_id == 'eager-test-and-overreferencing-and-verbose-test') {
    return('ET, OF, and VT')
  } else if (configuration_id == 'eager-test-and-overreferencing') {
    return('ET and OF')
  } else if (configuration_id == 'eager-test-and-rotten-green-tests-and-verbose-test') {
    return('ET, RGT, and VT')
  } else if (configuration_id == 'eager-test-and-rotten-green-tests') {
    return('ET and RGT')
  } else if (configuration_id == 'eager-test-and-verbose-test') {
    return('ET and VT')
  } else if (configuration_id == 'eager-test') {
    return('ET')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('IT, OISS, OF, RGT, and VT')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('IT, OISS, OF, and RGT')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('IT, OISS, OF, and VT')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-overreferencing') {
    return('IT, OISS, and OF')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('IT, OISS, RGT, and VT')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-rotten-green-tests') {
    return('IT, OISS, and RGT')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup-and-verbose-test') {
    return('IT, OISS, and VT')
  } else if (configuration_id == 'indirect-testing-and-obscure-inline-setup') {
    return('IT and OISS')
  } else if (configuration_id == 'indirect-testing-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('IT, OF, RGT, and VT')
  } else if (configuration_id == 'indirect-testing-and-overreferencing-and-rotten-green-tests') {
    return('IT, OF, and RGT')
  } else if (configuration_id == 'indirect-testing-and-overreferencing-and-verbose-test') {
    return('IT, OF, and VT')
  } else if (configuration_id == 'indirect-testing-and-overreferencing') {
    return('IT and OF')
  } else if (configuration_id == 'indirect-testing-and-rotten-green-tests-and-verbose-test') {
    return('IT, RGT, and VT')
  } else if (configuration_id == 'indirect-testing-and-rotten-green-tests') {
    return('IT and RGT')
  } else if (configuration_id == 'indirect-testing-and-verbose-test') {
    return('IT and VT')
  } else if (configuration_id == 'indirect-testing') {
    return('IT')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('OISS, OF, RGT, and VT')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing-and-rotten-green-tests') {
    return('OISS, OF, and RGT')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing-and-verbose-test') {
    return('OISS, OF, and VT')
  } else if (configuration_id == 'obscure-inline-setup-and-overreferencing') {
    return('OISS and OF')
  } else if (configuration_id == 'obscure-inline-setup-and-rotten-green-tests-and-verbose-test') {
    return('OISS, RGT, and VT')
  } else if (configuration_id == 'obscure-inline-setup-and-rotten-green-tests') {
    return('OISS and RGT')
  } else if (configuration_id == 'obscure-inline-setup-and-verbose-test') {
    return('OISS and VT')
  } else if (configuration_id == 'obscure-inline-setup') {
    return('OISS')
  } else if (configuration_id == 'overreferencing-and-rotten-green-tests-and-verbose-test') {
    return('OF, RGT, and VT')
  } else if (configuration_id == 'overreferencing-and-rotten-green-tests') {
    return('OF and RGT')
  } else if (configuration_id == 'overreferencing-and-verbose-test') {
    return('OF and VT')
  } else if (configuration_id == 'overreferencing') {
    return('OF')
  } else if (configuration_id == 'rotten-green-tests-and-verbose-test') {
    return('RGT and VT')
  } else if (configuration_id == 'rotten-green-tests') {
    return('RGT')
  } else if (configuration_id == 'verbose-test') {
    return('VT')
  } else if (configuration_id == 'vanilla-measure-smells-timelines') {
    return('Total length')
  }
  return(configuration_id)
}

#
# Given a full/canonical name of a class, it returns its name.  I.e., if a class
# is named 'org.foo.Bar', it returns 'Bar'.
#
get_class_name <- function(full_class_name) {
  l <- unlist(strsplit(full_class_name, "\\."))
  return(l[[length(l)]])
}

#
# Given full name of a class, it replaces every single word in the package name
# by the first letter , e.g., org.foo.MyClass -> o.f.MyClass
#
shorten_class_name <- function(full_class_name) {
  str_split <- unlist(strsplit(full_class_name, "\\."))
  class_name <- tail(str_split, n=1)
  class_name_with_short_package_name <- ''

  # Create a short package name
  for (word in str_split) {
    if (word == class_name) {
      next
    }
    class_name_with_short_package_name <- paste(class_name_with_short_package_name, substr(word, 1, 1), '.', sep='')
  }

  # Append class name
  class_name_with_short_package_name <- paste(class_name_with_short_package_name, class_name, sep='')

  return(replace_string(class_name_with_short_package_name, '_', ''))
}

#
# Compute relative value given a min and a max value.
#
compute_relative_value <- function(value, min_value=min_value, max_value=max_value) {
  r <- 0.0
  if (min_value == max_value) {
    r <- 1.0
  } else {
    r <- (value - min_value) / (max_value - min_value)
  }
  stopifnot(is.nan(r) == FALSE && is.na(r) == FALSE && r >= 0.0 && r <= 1.0)
  return(r)
}

# EOF

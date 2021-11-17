

#' \pkg{RcmdrPlugin.EZR} functions
#'
#' Functions imported from package \pkg{RcmdrPlugin.EZR}.
#' To use menus that have suffix `[EZR]`, package  \pkg{RcmdrPlugin.EZR} must
#' be installed.
#'
#' @name ezr-functions
#' @md
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
msg_missing_ezr <- function() {
  msg <- paste0(
    "Package 'RcmdrPlugin.EZR' is missing.\n",
    "To use menus with suffix '[EZR]', please, install 'RcmdrPlugin.EZR'. "
  )
  msg_missing_pkg(pkg = 'RcmdrPlugin.EZR', msg)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname ezr-functions
#' @export
StatMedCorrelation <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedCorrelation()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedSpearman <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedSpearman()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedSingleSampleTTest <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedSingleSampleTTest()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedWilSign <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedWilSign()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedPairedTtest <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedPairedTtest()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedFriedman <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedFriedman()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedTtest <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedTtest()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedMannW <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedMannW()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedANOVA <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedANOVA()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedKruWalli <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedKruWalli()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedJT <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedJT()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedMultiANOVA <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedMultiANOVA()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedANCOVA <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedANCOVA()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedBartlett <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedBartlett()
  } else {
    msg_missing_ezr()
  }
}

#' @rdname ezr-functions
#' @export
StatMedFTest <- function() {
  if (requireNamespace("RcmdrPlugin.EZR")) {
    RcmdrPlugin.EZR::StatMedFTest()
  } else {
    msg_missing_ezr()
  }
}

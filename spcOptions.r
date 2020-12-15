spcOptions <- function(
  rebase = NULL
  ,improvementDirection = NULL
  ,outputChart = NULL
  ,pointSize = NULL
  ,percentageYAxis = NULL
  ,target = NULL
  ,trajectory = NULL
  ,mainTitle = NULL
  ,xAxisLabel = NULL
  ,yAxisLabel = NULL
  ,fixedXAxisMultiple = NULL
  ,fixedYAxisMultiple = NULL
  ,xAxisDateFormat = NULL
  ,xAxisBreaks = NULL
  ,yAxisBreaks = NULL
){
  rebase <- enexprs(rebase)
  target <- enexprs(target)
  trajectory <- enexprs(trajectory)

  if(!(is.null(improvementDirection))){
    if(length(improvementDirection) > 1){
      stop("Improvement direction should be numeric (1 or -1) or character (increase or decrease). Multiple values are not valid.")
    } else {
      if(is.numeric(improvementDirection)){
        if(improvementDirection != 1 && improvementDirection != -1){
          stop("Improvment direction should be set as 1 for 'increase' or -1 for 'decrease'.")
        }
      } else if(is.character(improvementDirection)){
        if(improvementDirection != 'increase' && improvementDirection != 'decrease'){
          stop("Improvment direction should be set as 'increase' or 'decrease'.")
        }
      } else {
        stop("Improvement direction should be numeric (1 or -1) or character (increase or decrease).")
      }
    }
  }

  if(!(is.null(outputChart))){
    if(length(outputChart) > 1){
      stop("outputChart should be a logical vector of length 1.")
    } else if(!(is.logical(outputChart))){
      stop("outputChart should be a logical vector of length 1.")
    }
  }

  if(!(is.null(pointSize))){
    if(length(pointSize) > 1){
      stop("pointSize should be a numeric vector of length 1.")
    } else if(!(is.numeric(pointSize))){
      stop("pointSize should be a numeric vector of length 1.")
    }
  }

  if(!(is.null(percentageYAxis))){
    if(length(percentageYAxis) > 1){
      stop("percentageYAxis should be a logical vector or decimal value of length 1.")
    } else if(!(is.logical(percentageYAxis)) && !(is.numeric(percentageYAxis))){
      stop("percentageYAxis should be a logical vector or decimal value of length 1.")
    }
  }

  if(!(is.null(mainTitle))){
    if(length(mainTitle) > 1){
      stop("mainTitle should be a character vector of length 1.")
    } else if(!(is.character(mainTitle))){
      stop("mainTitle should be a character vector of length 1.")
    }
  }

  if(!(is.null(xAxisLabel))){
    if(length(xAxisLabel) > 1){
      stop("xAxisLabel should be a character vector of length 1.")
    } else if(!(is.character(xAxisLabel))){
      stop("xAxisLabel should be a character vector of length 1.")
    }
  }

  if(!(is.null(yAxisLabel))){
    if(length(yAxisLabel) > 1){
      stop("yAxisLabel should be a character vector of length 1.")
    } else if(!(is.character(yAxisLabel))){
      stop("yAxisLabel should be a character vector of length 1.")
    }
  }

  if(!(is.null(xAxisDateFormat))){
    if(length(xAxisDateFormat) > 1){
      stop("xAxisDateFormat should be a character vector of length 1.")
    } else if(!(is.character(xAxisDateFormat))){
      stop("xAxisDateFormat should be a character vector of length 1.")
    }
  }

  if(!(is.null(fixedXAxisMultiple))){
    if(length(fixedXAxisMultiple) > 1){
      stop("fixedXAxisMultiple should be a logical vector of length 1.")
    } else if(!(is.logical(fixedXAxisMultiple))){
      stop("fixedXAxisMultiple should be a logical vector of length 1.")
    }
  }

  if(!(is.null(fixedYAxisMultiple))){
    if(length(fixedYAxisMultiple) > 1){
      stop("fixedYAxisMultiple should be a logical vector of length 1.")
    } else if(!(is.logical(fixedYAxisMultiple))){
      stop("fixedYAxisMultiple should be a logical vector of length 1.")
    }
  }

  list(
    rebase = rebase
    ,improvementDirection = improvementDirection
    ,outputChart = outputChart
    ,pointSize = pointSize
    ,percentageYAxis = percentageYAxis
    ,target = target
    ,trajectory = trajectory
    ,mainTitle = mainTitle
    ,xAxisLabel = xAxisLabel
    ,yAxisLabel = yAxisLabel
    ,mainTitle = mainTitle
    ,xAxisLabel = xAxisLabel
    ,yAxisLabel = yAxisLabel
    ,fixedXAxisMultiple = fixedXAxisMultiple
    ,fixedYAxisMultiple = fixedYAxisMultiple
    ,xAxisDateFormat = xAxisDateFormat
    ,xAxisBreaks = xAxisBreaks
    ,yAxisBreaks = yAxisBreaks
  )
}

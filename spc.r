spc <- function(
  data.frame
  ,valueField
  ,dateField
  ,facetField = NULL
  ,options = NULL ## options: target, trjaectory, rebase, data as percentages, title, x title, y title, x axis break frequency, pointSize, returnChart, display legend
) {

  # Quote Expr
  df <- data.frame
  y_name <- enexpr(valueField)
  x_name <- enexpr(dateField)
  facet_name <- enexpr(facetField)

  rebaseField <- options$rebase[[1]]

  if(!(is.null(rebaseField))){
    if(!(as.character(rebaseField) %in% colnames(df))){
      df$rebase <- rep(as.numeric(NA),nrow(df))
    } else {
      df$rebase <- df[[rebaseField]]
    }
  } else {
    df$rebase <- rep(as.numeric(NA),nrow(df))
  }

  # Cleaning
  if(!(is.null(options)) && !(is.list(options))){
    stop("Options argument should be a list.")
  }
  if(!(is.data.frame(data.frame))){
    stop("Data.frame argument is not a data.frame.")
  }

  l <- nrow(data.frame)

  # Colour Palette
  .darkgrey = "#7B7D7D"
  .orange = "#fab428"
  .skyblue = "#289de0"
  .purple = "#361475"
  .red = "#de1b1b"

  # Constants
  limit <- 2.66
  limitclose <- 2*(limit/3)

  if(!(is.null(options$improvementDirection))){
    if(options$improvementDirection == "increase" || options$improvementDirection == 1){
      improvementDirection <- 1
    } else if(options$improvementDirection == "decrease" || options$improvementDirection == -1){
      improvementDirection <- -1
    } else {
      stop("Improvement direction option should be 'increase' or 'decrease'")
    }
  } else {
    improvementDirection <- 1
  }

  if(!(is.null(options$outputChart))){
    if(options$outputChart == TRUE){
      outputChart <- 1
    } else if(options$outputChart == FALSE){
      outputChart <- 0
    } else {
      stop("outputChart option must be true or false")
    }
  } else {
    outputChart <- 1
  }

  # X axis breaks should be character string showing date seq intervals
  if(!(is.null(options$xAxisBreaks))){
    if(is.character(options$xAxisBreaks)){
      xaxis <- df[[x_name]]
      start <- min(xaxis,na.rm = TRUE)
      end <- max(xaxis,na.rm = TRUE)
      xaxislabels <- seq.Date(from = as.Date(start), to = as.Date(end), by = options$xAxisBreaks)
    } else {
      stop("X Axis Break option must be character vector of length 1. E.g. '3 months'.")
    }
  } else {
    xaxislabels <- df[[x_name]]
  }



  if(!(is.null(options$pointSize))){
    if(is.numeric(options$pointSize)){
      pointSize <- options$pointSize
    } else {
      stop("pointSize option must be an integer")
    }
  } else {
    pointSize = 2
  }

  if(!(is.null(options$xAxisDateFormat))){
    if(is.character(options$xAxisDateFormat)){
      xAxisDateFormat <- options$xAxisDateFormat
    } else {
      stop("xAxisDateFormat option must be a character")
    }
  } else {
    xAxisDateFormat <- "%d/%m/%Y"
  }

  if(!(is.null(options$percentageYAxis))){
    if(is.numeric(options$percentageYAxis)){
      convertToPercentages <- options$percentageYAxis
    } else if (is.logical(options$percentageYAxis)){
      convertToPercentages <- 0.1 * as.numeric(options$percentageYAxis)
    } else {
      stop("percentageYAxis option should be TRUE or a decimal value < 1 to indicate axis break frequency.")
    }
  } else {
    convertToPercentages <- 0
  }

  trajectoryField <- options$trajectory[[1]]

  if(!(is.null(trajectoryField))){
    if(!(as.character(trajectoryField) %in% colnames(df))){
      df$trajectory <- rep(as.numeric(NA),nrow(df))
    } else {
      df$trajectory <- df[[trajectoryField]]
    }
  } else {
    df$trajectory <- rep(as.numeric(NA),nrow(df))
  }

  targetField <- options$target[[1]]

  if(!(is.null(targetField))){
    if(!(as.character(targetField) %in% colnames(df))){
      df$target <- rep(as.numeric(NA),nrow(df))
    } else {
      df$target <- df[[targetField]]
    }
  } else {
    df$target <- rep(as.numeric(NA),nrow(df))
  }

  if(!(is.null(options$mainTitle))){
    plottitle <- options$mainTitle
  } else {
    plottitle <- "SPC Chart"
  }

  if(!(is.null(options$xAxisLabel))){
    xlabel <- options$xAxisLabel
  } else {
    xlabel <- "Date"
  }

  if(!(is.null(options$yAxisLabel))){
    ylabel <- options$yAxisLabel
  } else {
    ylabel <- "Value"
  }

  if(!(is.null(options$fixedXAxisMultiple))){
    scaleXFixed <- options$fixedXAxis
  } else {
    scaleXFixed <- TRUE
  }

  if(!(is.null(options$fixedYAxisMultiple))){
    scaleYFixed <- options$fixedYAxis
  } else {
    scaleYFixed <- TRUE
  }

  facetScales <- if(scaleYFixed == TRUE && scaleXFixed == TRUE){
    "fixed"
  } else if (scaleYFixed == TRUE && scaleXFixed == FALSE){
    "free_x"
  } else if (scaleYFixed == FALSE && scaleXFixed == TRUE){
    "free_y"
  } else if (scaleYFixed == FALSE && scaleXFixed == FALSE){
    "free"
  }

  if(is.null(facet_name)){
    f <- data.frame(facet_name = rep("no facet",nrow(df)))
    df <- cbind(df,f)
    message("No facet detected - binding pseudo-facet column")
  }

  # #Logic
  df <- df %>%
    select(y = y_name, x = x_name, f = facet_name, rebase = .data$rebase, trajectory = .data$trajectory, target = .data$target) %>%
    group_by(f) %>%
    mutate(n = row_number()) %>%
    ungroup() %>%
    arrange(f,.data$x) %>%
    mutate(
      movingrange = case_when(
        n > 1 ~ abs(.data$y - lag(.data$y,1))
      )
      ,cross_join = 1
    )

  mra <- df %>%
    group_by(.data$f) %>%
    summarise(movingrangeaverage = mean(.data$movingrange, na.rm = TRUE))

  df <- df %>%
    left_join(mra, by = c(f = "f"))

  rebaseTable <- df %>%
    filter(.data$rebase == 1) %>%
    select(.data$f, .data$x)

  rebaseTable2 <- df %>%
    group_by(.data$f) %>%
    summarise(x = min(.data$x))

  rebaseTable3 <- df %>%
    group_by(.data$f) %>%
    summarise(x = max(.data$x))

  rebaseTable4 <- rbind(rebaseTable, rebaseTable2, rebaseTable3) %>%
    arrange(.data$f,.data$x) %>%
    group_by(.data$f) %>%
    mutate(rn = row_number()
           ,start = lag(.data$x,1)
           ) %>%
    ungroup() %>%
    filter(.data$rn != 1) %>%
    select(.data$f, .data$start, end = .data$x) %>%
    group_by(.data$f) %>%
    mutate(rn = row_number()) %>%
    arrange(.data$f,desc(.data$start)) %>%
    mutate(rn2 = row_number()) %>%
    ungroup()

  df2 <- df %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$x >= .data$start, .data$x < .data$end)

  df3 <- df %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$rn2 == 1, .data$x == .data$end)

  df <- rbind(df2, df3) %>%
    arrange(f,.data$x) %>%
    select(.data$y,.data$x,f,.data$rebase,n,.data$target,.data$trajectory,.data$movingrange,.data$movingrangeaverage,rebaseGroup = .data$rn)

  df_avg <- df %>%
    group_by(f,.data$rebaseGroup) %>%
    summarise(mean = mean(.data$y,na.rm = TRUE))

  df <- df %>%
    left_join(df_avg, by = c("f" = "f","rebaseGroup" = "rebaseGroup")) %>%
    mutate(
      lpl = mean - (limit * .data$movingrangeaverage)
      ,upl = mean + (limit * .data$movingrangeaverage)
      ,nlpl = mean - (limitclose * .data$movingrangeaverage)
      ,nupl = mean + (limitclose * .data$movingrangeaverage)
    ) %>%
    mutate(
      outsideLimits = case_when(
        .data$y > upl | .data$y < lpl ~ 1
        ,TRUE ~ 0
      )
      ,relativeToMean = case_when(
        .data$y < mean ~ -1
        ,.data$y > mean ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      closeToLimits = case_when(
        .data$y > nupl & .data$y <= upl ~ 1
        ,.data$y < nlpl & .data$y >= lpl ~ 1
        ,TRUE ~ 0
      )
      ,sevenPointTrend = case_when(
          (relativeToMean == lag(relativeToMean,1) & f == lag(f,1))
        & (relativeToMean == lag(relativeToMean,2) & f == lag(f,2))
        & (relativeToMean == lag(relativeToMean,3) & f == lag(f,3))
        & (relativeToMean == lag(relativeToMean,4) & f == lag(f,4))
        & (relativeToMean == lag(relativeToMean,5) & f == lag(f,5))
        & (relativeToMean == lag(relativeToMean,6) & f == lag(f,6))
        ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      partOfSevenPointTrend = case_when(
        sevenPointTrend == 1
        | (lead(sevenPointTrend,1) == 1 & lead(f,1) == f)
        | (lead(sevenPointTrend,2) == 1 & lead(f,2) == f)
        | (lead(sevenPointTrend,3) == 1 & lead(f,3) == f)
        | (lead(sevenPointTrend,4) == 1 & lead(f,4) == f)
        | (lead(sevenPointTrend,5) == 1 & lead(f,5) == f)
        | (lead(sevenPointTrend,6) == 1 & lead(f,6) == f)
        ~ 1
        ,TRUE ~ 0
      )
      ,sixPointGrowth = case_when(
        (.data$y > lag(.data$y,1) & f == lag(f,1))
        & (lag(.data$y,1) > lag(.data$y,2) & lag(f,1) == lag(f,2))
        & (lag(.data$y,2) > lag(.data$y,3) & lag(f,2) == lag(f,3))
        & (lag(.data$y,3) > lag(.data$y,4) & lag(f,3) == lag(f,4))
        & (lag(.data$y,4) > lag(.data$y,5) & lag(f,4) == lag(f,5))
        & (lag(.data$y,5) > lag(.data$y,6) & lag(f,5) == lag(f,6))
        ~ 1
        ,(.data$y < lag(.data$y,1) & f == lag(f,1))
        & (lag(.data$y,1) < lag(.data$y,2) & lag(f,1) == lag(f,2))
        & (lag(.data$y,2) < lag(.data$y,3) & lag(f,2) == lag(f,3))
        & (lag(.data$y,3) < lag(.data$y,4) & lag(f,3) == lag(f,4))
        & (lag(.data$y,4) < lag(.data$y,5) & lag(f,4) == lag(f,5))
        & (lag(.data$y,5) < lag(.data$y,6) & lag(f,5) == lag(f,6))
        ~ -1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      partOfSixPointGrowth = case_when(
        abs(sixPointGrowth) == 1
        | (abs(lead(sixPointGrowth,1)) == 1 & f == lead(f,1))
        | (abs(lead(sixPointGrowth,2)) == 1 & f == lead(f,2))
        | (abs(lead(sixPointGrowth,3)) == 1 & f == lead(f,3))
        | (abs(lead(sixPointGrowth,4)) == 1 & f == lead(f,4))
        | (abs(lead(sixPointGrowth,5)) == 1 & f == lead(f,5))
        ~ 1
        ,TRUE ~ 0
      )
      ,twoInThree = case_when(
        (abs(closeToLimits) + abs(lag(closeToLimits,1,default = 0)) + abs(lag(closeToLimits,2,default = 0)) >= 2)
        & f == lag(f,1)
        & f == lag(f,2)
        ~ 1
        ,(abs(closeToLimits) + abs(lag(closeToLimits,1,default = 0)) + abs(lead(closeToLimits,1,default = 0)) >= 2)
        & f == lag(f,1)
        & f == lead(f,1)
        ~ 1
        ,(abs(closeToLimits) + abs(lead(closeToLimits,1,default = 0)) + abs(lead(closeToLimits,2,default = 0)) >= 2)
        & f == lead(f,1)
        & f == lead(f,2)
        ~ 1
        ,TRUE ~ 0
      )
      ,partOfTwoInThree = case_when(
        twoInThree == 1 & abs(closeToLimits) == 1 ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      specialCauseFlag = case_when(
        abs(outsideLimits) == 1
        | abs(partOfSevenPointTrend) == 1
        | abs(partOfSixPointGrowth) == 1
        | partOfTwoInThree == 1
        ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      specialCauseConcern = case_when(
        specialCauseFlag == 1
        & relativeToMean == (improvementDirection * -1)
        ~ .data$y
      )
      ,specialCauseImprovement = case_when(
        specialCauseFlag == 1
        & relativeToMean == improvementDirection
        ~ .data$y
      )
    )

  # Y axis breaks should be integer or decimal
  if(!(is.null(options$yAxisBreaks))){
    if(is.numeric(options$yAxisBreaks)){
      yaxis <- c(df$y,df$upl,df$lpl)
      start <- floor(min(yaxis,na.rm = TRUE)/options$yAxisBreaks) * options$yAxisBreaks
      end <- max(yaxis,na.rm = TRUE)
      yaxislabels <- seq(from = start, to = end, by = options$yAxisBreaks)
    } else {
      stop("Y Axis Break option must be numeric.")
    }
  }

  if(outputChart == 1){
    plot <- ggplot(df,aes(x=.data$x,y=.data$y)) +
      theme_minimal() +
      geom_line(aes(y=.data$upl),linetype = "dashed",size=pointSize/2.666666,color=.darkgrey) +
      geom_line(aes(y=.data$lpl),linetype = "dashed",size=pointSize/2.666666,color=.darkgrey) +
      geom_line(aes(y=.data$target),linetype = "dashed",size=pointSize/2.666666,color=.purple) +
      geom_line(aes(y=.data$trajectory),linetype = "dashed",size=pointSize/2.666666,color=.red) +
      geom_line(aes(y=mean)) +
      geom_line(color=.darkgrey,size=pointSize/2.666666) +
      geom_point(color=.darkgrey,size=pointSize)

    if(!(is.null(facet_name))){
      plot <- plot +
        facet_wrap(vars(f), scales = facetScales)
    }

    plot <- plot +
      geom_point(aes(x=.data$x,y=.data$specialCauseImprovement),color=.skyblue,size=pointSize) +
      geom_point(aes(x=.data$x,y=.data$specialCauseConcern),color=.orange,size=pointSize) +
      ggtitle(label = plottitle) +
      xlab(label = xlabel) +
      ylab(label = ylabel) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(breaks=xaxislabels, labels = format(xaxislabels, format = xAxisDateFormat)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    if(is.null(facet_name)){
      if(convertToPercentages == FALSE){
        if(!(is.null(options$yAxisBreaks))){
          plot <- plot +
            scale_y_continuous(breaks = yaxislabels, labels = yaxislabels)
        }
      } else if(convertToPercentages != 0) {
        percentLimit <- max(df$upl,na.rm = TRUE)

        interval <- if(!(is.null(options$yAxisBreaks))){options$yAxisBreaks} else {convertToPercentages}

        plot <- plot +
          scale_y_continuous(labels = scales::percent,breaks = seq(from = 0, to = percentLimit, by = interval))
      }
    }

    if(!(is.null(facet_name))){
      if(convertToPercentages != 0) {
        percentLimit <- max(df$upl,na.rm = TRUE)

        plot <- plot +
          scale_y_continuous(labels = scales::percent)
      }
    }

    plot
  } else if(outputChart == 0){
    df
  }
}

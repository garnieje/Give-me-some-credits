formatData <- function(data) { 
  # format the raw data

  # format resolving utilization
  data$highResolving <- data$RevolvingUtilizationOfUnsecuredLines
  data$highResolving[data$RevolvingUtilizationOfUnsecuredLines <= 2] <- 0 
  data$zeroResolving <- as.integer(data$RevolvingUtilizationOfUnsecuredLines == 0)
  data$RevolvingUtilizationOfUnsecuredLines[data$highResolving > 0] <- 0
  data$fullOrExcessResolving <- as.integer(data$RevolvingUtilizationOfUnsecuredLines >= 1)
  data$almostFullResolving <- as.integer(data$RevolvingUtilizationOfUnsecuredLines == 0.9999999)
  
  # format age
  data$retired <- as.integer(data$age > 60)
  data$logAge <- log1p(data$age)

  
  # format monthly income
  data$NAMonthlyIncome <- as.integer(is.na(data$MonthlyIncome))
  data$noMonthlyIncome <- as.integer(data$MonthlyIncome == 0)
  data$noMonthlyIncome[data$NAMonthlyIncome == 1] <- 0
  data$MonthlyIncome[data$NAMonthlyIncome == 1] <- 0
  
  # format debt
  data$noDebt <- as.integer(data$DebtRatio == 0)
  data$logDebtUnknowIncome <- log1p(data$DebtRatio)
  data$logDebtUnknowIncome[data$NAMonthlyIncome == 0] <- 0
  data$DebtRatio[data$NAMonthlyIncome == 1] <- 0
  
  
  data$logDebt <- log(pmax(data$MonthlyIncome, rep(1, nrow(data))) * data$DebtRatio)
  data$logDebt[data$DebtRatio == 0] <- 0
  data$logMonthlyIncome <- log(data$MonthlyIncome)
  data$logMonthlyIncome[data$MonthlyIncome == 0] <- 0
    
  # format later day
  data$largeDaysLate <- as.integer(data$NumberOfTimes90DaysLate > 90)
  
  data$zero30.59DaysLate <- as.integer(data$NumberOfTime30.59DaysPastDueNotWorse == 0)
  data$NumberOfTime30.59DaysPastDueNotWorse[data$largeDaysLate == 1] <- 0
  
  data$zero90DaysLate <- as.integer(data$NumberOfTimes90DaysLate == 0)
  data$NumberOfTimes90DaysLate[data$largeDaysLate == 1] <- 0
  
  data$zero60.89DaysLate <- as.integer(data$NumberOfTime60.89DaysPastDueNotWorse == 0)
  data$NumberOfTime60.89DaysPastDueNotWorse[data$largeDaysLate == 1] <- 0
  
  data$neverLate <- as.integer(data$zero30.59DaysLate * data$zero60.89DaysLate * data$zero90DaysLate)
  
  
  # format number of dependant
  data$NANumberOfDependents <- as.integer(is.na(data$NumberOfDependents))
  data$zeroDependent <- as.integer(data$NumberOfDependents == 0)
  data$zeroDependent[data$NANumberOfDependents == 1] <- 0
  data$NumberOfDependents[data$NANumberOfDependents == 1] <- 0
  data$logHousehold <- log1p(data$NumberOfDependents)
  
  
  # avg day past due
  data$sumMinorDelay <- data$NumberOfTime60.89DaysPastDueNotWorse + data$NumberOfTime30.59DaysPastDueNotWorse
  data$sumDelay <- data$sumMinorDelay + data$NumberOfTimes90DaysLate
  data$logdaysMinorDelay <- log1p(75 * data$NumberOfTime60.89DaysPastDueNotWorse + 45 * data$NumberOfTime30.59DaysPastDueNotWorse)
  data$logdaysDelay <- log1p(75 * data$NumberOfTime60.89DaysPastDueNotWorse + 45 * data$NumberOfTime30.59DaysPastDueNotWorse + 90 * data$NumberOfTimes90DaysLate)
  
  data$logRatioDisposal <- data$logMonthlyIncome - data$logDebt
  
  # ratio log debt per
  data$logDebtPerDependent <- data$logDebt - log1p(data$NumberOfDependents)
  data$logDebtPerDependent[data$NANumberOfDependents == 1] <- 0
  data$logDebtPerLine <- data$logDebt - log1p(data$NumberOfOpenCreditLinesAndLoans)
  data$logDebtPerMortgage <- data$logDebt - log1p(data$NumberRealEstateLoansOrLines)
  data$logDebtPerAge <- data$logDebt - log1p(data$age)
  
  # ratio log income per
  data$logIncomePerDependent <- data$logMonthlyIncome - log1p(data$NumberOfDependents)
  data$logIncomePerDependent[data$NANumberOfDependents == 1] <- 0
  data$logIncomePerLine <- data$logMonthlyIncome - log1p(data$NumberOfOpenCreditLinesAndLoans)
  data$logIncomePerMortgage <- data$logMonthlyIncome - log1p(data$NumberRealEstateLoansOrLines)
  data$logIncomePerAge <- data$logMonthlyIncome - log1p(data$age)
  
  # ratio log debt for unknow income
  data$logDebtUnknowIncomePerDependent <- data$logDebtUnknowIncome - log1p(data$NumberOfDependents)
  data$logDebtUnknowIncomePerDependent[data$NANumberOfDependents == 1] <- 0
  data$logDebtUnknowIncomePerDependent[data$NAMonthlyIncome == 0] <- 0
  data$logDebtUnknowIncomePerLine <- data$logDebtUnknowIncome - log1p(data$NumberOfOpenCreditLinesAndLoans)
  data$logDebtUnknowIncomePerLine[data$NAMonthlyIncome == 0] <- 0
  data$logDebtUnknowIncomePerMortgage <- data$logDebtUnknowIncome - log1p(data$NumberRealEstateLoansOrLines)
  data$logDebtUnknowIncomePerMortgage[data$NAMonthlyIncome == 0] <- 0
  data$logDebtUnknowIncomePerAge <- data$logDebtUnknowIncome - log1p(data$age)
  data$logDebtUnknowIncomePerAge[data$NAMonthlyIncome == 0] <- 0
  
  
  data$numberOfCreditLine <- data$NumberOfOpenCreditLinesAndLoans - data$NumberRealEstateLoansOrLines
  data$logResolvingTimesLines <- log1p(data$RevolvingUtilizationOfUnsecuredLines * data$numberOfCreditLine)
  
  # delay per line
  data$logMinorDelayPerLine <- log1p(data$sumMinorDelay) - log1p(data$NumberOfOpenCreditLinesAndLoans)
  data$logMajorDelayPerLine <- log1p(data$NumberOfTimes90DaysLate) - log1p(data$NumberOfOpenCreditLinesAndLoans)
  data$logDelayPerline <- log1p(data$sumDelay) - log1p(data$NumberOfOpenCreditLinesAndLoans)
  
  # delay per mortgage
  data$logMinorDelayPerMortgage <- log1p(data$sumMinorDelay) - log1p(data$NumberRealEstateLoansOrLines)
  data$logMajorDelayPerMortgage <- log1p(data$NumberOfTimes90DaysLate) - log1p(data$NumberRealEstateLoansOrLines)
  data$logDelayPerMortgage <- log1p(data$sumDelay) - log1p(data$NumberRealEstateLoansOrLines)
  
  # line per dependent
  data$logLinePerDependent <- log1p(data$NumberOfOpenCreditLinesAndLoans) - log1p(data$NumberOfDependents)
  data$logLinePerDependent[data$NANumberOfDependents == 1] <- 0
  
  # mortgage per dependent
  data$logMortgagePerDependent <- log1p(data$NumberRealEstateLoansOrLines) - log1p(data$NumberOfDependents)
  data$logMortgagePerDependent[data$NANumberOfDependents == 1] <- 0
  
  # log transformations
  data$NumberOfDependents <- NULL
  data$MonthlyIncome <- NULL
  data$age <- NULL
  data$logCreditLine <- log1p(data$numberOfCreditLine)
  data$numberOfCreditLine <- NULL
  data$logMinorDelay <- log1p(data$sumMinorDelay)
  data$sumMinorDelay <- NULL
  data$logTotalDelay <- log1p(data$sumDelay)
  data$sumDelay <- NULL
  data$logNumberOfTime30.59DaysPastDueNotWorse <- log1p(data$NumberOfTime30.59DaysPastDueNotWorse)
  data$NumberOfTime30.59DaysPastDueNotWorse <- NULL
  data$logNumberOfTimes90DaysLate <- log1p(data$NumberOfTimes90DaysLate)
  data$NumberOfTimes90DaysLate <- NULL
  data$logNumberOfTime60.89DaysPastDueNotWorse <- log1p(data$NumberOfTime60.89DaysPastDueNotWorse)
  data$NumberOfTime60.89DaysPastDueNotWorse <- NULL
  data$logNumberOfOpenCreditLinesAndLoans <- log1p(data$NumberOfOpenCreditLinesAndLoans)
  data$NumberOfOpenCreditLinesAndLoans <- NULL
  data$logNumberRealEstateLoansOrLines <- log1p(data$NumberRealEstateLoansOrLines)
  data$NumberRealEstateLoansOrLines <- NULL
  
  data$logRatio90per30.59 <- data$logNumberOfTimes90DaysLate - data$logNumberOfTime30.59DaysPastDueNotWorse
  data$logRatio90per60.89 <- data$logNumberOfTimes90DaysLate - data$logNumberOfTime60.89DaysPastDueNotWorse
    
  return(data)
}

# Corrected Boston housing data
data(BostonHousing2, package = "mlbench")
boston <- BostonHousing2[, -c(1, 2, 5)]  # remove unneeded columns

# Corrected Pima Indians diabetes data
data(PimaIndiansDiabetes2, package = "mlbench")
pima <- PimaIndiansDiabetes2

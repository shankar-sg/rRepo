#
# For a numeric field which s continuous like age or salary, it is laborious to get an optimal bin size
# Correlation of the bin to the target can help determine the optimum bin size 
# Tries various bin sizes from 2 to maximum Number of bins specified. Checks the correlation of each run to target
# Selects the bin sie where correlation is the greatest and returns bins corresonding to it
#
# Inputs: columnToBin values, target (to check correlation with), maimum numberOfBins
# Output: bins where correlation with target is highest
#
# Exceptions: target and columnToBin must be numeric
#

getOptimalBins <- function(valuesToBin, target, maxBin)
{
  corrmax = 0
  corrMaxIndex = 0
   
  for ( i in 2:maxBin)
  {
    currentBins <- createColumnBins(valuesToBin,i)
    currentCor <- abs(cor(currentBins,target))
    print(paste(i , " " , currentCor))
    if (currentCor > corrmax)
    {
      corrMaxIndex = i  
      corrmax = currentCor
    }
    
  }
  print(corrMaxIndex)  
  optimalBins <- as.integer(createColumnBins(valuesToBin,corrMaxIndex))
  optimalBins
  
  
}
#
# Helper to optimise "cut" of bins into groups
# Returns bins. Optionally if label for bins is not specified creates them manually  
#
# Inputs: columnToBin values, numberOfBins and labels of bins
# Output: bins
#
# Exceptions: If labels not provided, creates it 
#

createColumnBins <- function(columnToBin,numberOfBins,binLabels=NULL)
{
  if ((binLabels == NULL) || (length(binLabels) < numberOfBins)){
    lblNum <- c(1:numberOfBins)
    binLabels <- as.character(lblNum)
  }
  colBins <- cut(columnToBin,numberOfBins,labels = binLabels)
}
#
# Get Feature Names that are highly correlated with target (> threshold specified) in a dataframe
# Examines numeric fields in the dataframe with the target and gets pairwise correlation
# Returns feature names as a list if correlation matches criteria
#
# Inputs: dataFrame (df), target, threshold (defaults to 0.2), targetIncluded (defaults to TRUE)
# Output: vector of features
#
# Exceptions: correlation is 0 if target or feature is not an integer 
#



getHighlyCorrelatedFeatures <- function(df,target,threshold=0.2, targetIncluded=TRUE)
{
  cols <- colnames(df)
  noCols <- ncol(df)
  # Incoming dataframe includes the target variable, exclude it from correlation calculation
  if (targetIncluded){
    noCols <- noCols - 1
  }  
  corlist = c()
  
  #
  # Loop through all columns in the dataframe one by one to check correlation with target
  #
  for (i in 1:noCols)
  {
    corl = getCorrelations(df,cols[i],target)
    if (abs(corl) > threshold)
    {
      # correlation greater than threshold, so include it in the return list
      corlist <- c(corlist, cols[i])
    }
  }
  corlist
  
}

#
# Function to get Correlations from a dataframe given the name of feature and target
# Expectation is that both are numeric as correlation for nonnumerics is not allowed
#
# Inputs: dataFrame (df), feature and a target
# Output: correlation
#
# Exceptions: 0 if target or feature is not an integer 
#
getCorrelations <- function(df,feature,target)
{
  corl = 0
  if (is.integer((df[,target])))
  {
    return(corl)
  }
  if(is.integer(df[,feature]))
  {
    corl <- cor(df[,feature],df[,target])
    print(corl)
  }
  corl
}

#isTransposedInput - Output meanings :
#0 -> The table isn't transposed
#1 -> The table is transposed
#-1 -> Error, the table is not correctly formated
#
#

isTransposedInput <- function(dataframe){
  if(tolower(colnames(dataframe)[1]) == "group"){
    return(0)  ##
  }else if(tolower(rownames(dataframe)[1]) == "group"){
    return(1)
  }else{
    return(-1)
  }
}

#validInput - Output meanings :
#error output :
#0 -> No error
#1 -> Rownames (ID) and/or Columns (Parameters) aren't unique
#2 -> Dataframe not correctly formated
#3 -> Parameter column values (3rd column to the last) are not all "numeric" or "integer"
#4 -> Negative values detected -> values will be set as NA if the log option is used
#dataframe output :
#the dataframe formated (transposed or not) or null if error != 0
validingInput <- function(infoFileDatapath,decSeparator = "."){
  if (  grepl(".txt", infoFileDatapath)  == T ) { separator= "\t"} else {separator=";"}
  df <- read.csv(infoFileDatapath, sep =  separator, dec = decSeparator)
  df <- fread(infoFileDatapath, sep =  separator, dec = decSeparator)
  df <- as.data.frame(df)

  if(anyDuplicated(df[,1])){
     return(list("error" = 1, "dataframe" = NULL))
  }else{
    rownames(df) <- df[,1]
    df <- df[,-1]
  }
  transposed <- isTransposedInput(df)
  if(transposed == -1){
    return(list("error" = 2, "dataframe" = NULL))
  }else if(transposed == 1){
    df <- as.data.frame(t(df))
    Group <- as.character(df[,1])
    df <- type.convert(df[,-1])
    df <- cbind(Group,df)
  }

  for(col in df[,2:length(df)]){
    if(!(is.numeric(col)) & !(is.integer(col))){
      if(decSeparator == ","){
        return(list("error" = 3, "dataframe" = NULL))
      }else{
        return(validingInput(infoFileDatapath,","))
      }
    }
  }

  return(list("error" = 0, "dataframe" = df))
}
####End

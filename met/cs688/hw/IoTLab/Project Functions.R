# Project Functions 

### Pre-Process Data & Call Neural Network 

Forecast.Electric.Demand <- function (Raw_Data) 
{
  print("2. Inputs sent to function: Forecast.Electric.Demand()")
  # Extract Time Stemps from Data
  Num.Data.Points <- dim(Raw_Data)[1]
  Time.Stamp <- strptime(Raw_Data$DATE,"%m/%d/%Y %H:%M")
  print(Time.Stamp[2])
  
  # Select Training Range 
  StartTime <- 1 # which(Time.Stamp=="2014-03-01 01:00:00 EST")
  TrainRange <- StartTime:Num.Data.Points
  print(paste0("Training data start date: ",Time.Stamp[StartTime]))
  
  # Extract Hours field from Time.Stamp
  # Hours <- rep(1,length(TrainRange)) # Replace this Line
  # Insert your code here
  Hours <- as.numeric(format(as.POSIXct(Raw_Data$DATE, format="%m/%d/%Y %H:%M"), format="%H"))

  # Extract Days field from Time.Stamp
  # Day.Number <- rep(1,length(TrainRange)) # Replace this Line
  # Insert your code here
  Day.Number <- as.POSIXlt(format(as.POSIXct(Raw_Data$DATE, format="%m/%d/%Y %H:%M"), format="%Y-%m-%d"))$wday

  # Modify Hours & Days
  # Hours.Modified <- Hours # Replace this Line
  temp <- 12 - Hours; temp[temp >= 0] = 0
  Hours.Modified <- Hours + 2*temp
  Day.Number.Modified <- Day.Number # Replace this Line
  print(Day.Number.Modified)
  Day.Number.Modified[Day.Number==0]=1;
  Day.Number.Modified[Day.Number==1]=2;
  Day.Number.Modified[Day.Number==2]=2;
  Day.Number.Modified[Day.Number==3]=2;
  Day.Number.Modified[Day.Number==4]=2;
  Day.Number.Modified[Day.Number==5]=2;
  Day.Number.Modified[Day.Number==6]=3;
  # Insert your code here
  print("Extracting Hour_of_Day & Day_of_Week fields from the DATE field Time Stamp ")
  
  # Choose Data to Process 
  Dependent.Ix <- c(2:4) # Select dependent columns
  Dependent.Data <- cbind(Hours.Modified, Day.Number.Modified, Raw_Data[TrainRange,Dependent.Ix]); # X ()
  Independent.Ix <- c(5) # Select Independent columns
  Independent.Data <- Raw_Data[TrainRange,Independent.Ix]; # Y (Actual Electric Demand )
  print("Dependent data tags: ");  print(names(Dependent.Data))
  print("Independent data tags: ");  print(names(Raw_Data[Independent.Ix]))
  
  # Get inputs & targets from the Data 
  inputs <- Dependent.Data # Actual Consumption - used for training
  targets <- Independent.Data # Expected Consumption (Regression data) used as Tags
  Percent.To.Test <- 0.30 # Split the input data into train and test
  print("Define NuNet Inputs: "); print(paste0("Percent of input data to test: ", 100*Percent.To.Test, " %"))
  
  # Train NuNet & Get Predictions
  print("Train NuNet & Get Predictions, please wait... ");
  Predicted.Electric.Demand <- TrainNuNet(inputs,targets,Percent.To.Test) 
  print("NuNet Training finished!");
  
  Output <- list("TimeStamp"=Time.Stamp,"inputs"=inputs,
                 "targets"=targets,
                 Predicted.Electric.Demand=Predicted.Electric.Demand,
                 Percent.To.Test=Percent.To.Test)
  
  return (Output) # Returned object
  
}


TrainNuNet <- function (inputs,targets,Percent.To.Test) 
{
  # Normalize the Data 
  if (is.null(dim(inputs))) # Single Column Input
  {
    z <- max(inputs, na.rm=TRUE) # find Max in Single Input Column
    inputs.scale <- z; targets.scale <- max(targets, na.rm=TRUE)
    inputs.normalized <- inputs/inputs.scale # Normalize Data
    targets.normalized <- targets/targets.scale # Normalize Data 
  }
  else # Multi Colum Input
  {
    z <- apply(inputs, MARGIN = 2, function(x) max(x, na.rm=TRUE)) # find Max in Each Input Column
    inputs.scale <- as.vector(z); targets.scale <- max(targets, na.rm=TRUE);
    inputs.normalized <- sweep(inputs, 2, inputs.scale, `/`) # Normalize Data
    targets.normalized <- targets/targets.scale # Normalize Data   
  }
  
  
  # Split the Data into Train and Test
  patterns <- splitForTrainingAndTest(inputs.normalized, targets.normalized, ratio = Percent.To.Test) 
  set.seed(13);
  
  # Train NN to folow Actual 
  # The use of an Elman network (Elman 1990) for time series regression.
  # model <- elman(patterns$inputsTrain, patterns$targetsTrain,
  #                size = c(10, 10), learnFuncParams = c(0.1), maxit = 1300,
  #                inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest,
  #                linOut = FALSE)
  model <- elman(patterns$inputsTrain, patterns$targetsTrain,
                 size = c(8, 8), learnFuncParams = c(0.1), maxit = 500,
                 inputsTest = patterns$inputsTest, targetsTest = patterns$targetsTest,
                 linOut = FALSE)
  
  NN.fitted.Train <- model$fitted.values*targets.scale 
  NN.fitted.Test <- model$fittedTestValues*targets.scale 
  
  Predicted.Electric.Demand <- c(NN.fitted.Train,NN.fitted.Test)
  
  result <- list(Predicted.Electric.Demand)
  
  return (result) # Returned object
}


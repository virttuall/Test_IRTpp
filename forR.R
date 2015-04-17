rm(list=ls())
parametersMirt <- function( model, pathDataSet )
{
  library('mirt')
  data = expand.table(LSAT7)
  data = read.table(pathDataSet, sep=" ",header=T)
  if ( model == '1PL_AD')
  {
    fit = mirt(data = data,model = 1,itemtype = "2PL",constrain = list(seq(1,ncol(data)*4,by = 4)))
  }
  else
  {
    fit = mirt(data = data,model = 1,itemtype = model )
  }
  zita=coef(fit)
  zita=unlist(zita)
  n = length(zita)
  zita = zita[-((n-1):n)]
  print(zita)
  zita = matrix(zita,ncol=4,byrow = TRUE)
  zita[,2] = -zita[,2] / zita[,1]
  return (zita[,c(1,2,3)])
}
parametersIRTpp <-function( model , pathDataSet)
{
  library('IRTpp')
  data = read.table(pathDataSet, sep=" ",header=T)
  dataForIRTpp = as.matrix(data)
  outputIRTpp = irtpp( dataForIRTpp, model, 1, "ANDRADE" , 0.0001, 1000, T)
  outputIRTpp = outputIRTpp[1]
  if ( model == 'RASCH_A1')
  {
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 1, byrow = FALSE)
  }
  else if( model == 'RASCH_A_CONSTAN')
  {
    v1 = outputIRTpp[1]
    v1 = unlist(v1)
    outputIRTpp = outputIRTpp[-1]
    v1 = rep(v1, times = length(outputIRTpp))
    v2 = unlist(outputIRTpp)
    v1 = c(v1, v2)
    outputIRTpp = matrix( v1, ncol = 2, byrow = FALSE) #TODO
  }
  else if( model == 'TWO_PL')
  {
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 2, byrow = FALSE)
  }
  else if( model == 'THREE_PL')
  {
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 3, byrow = FALSE)
  }
  else
  {
    print("Error in parametersIRTpp, model no found")
  }
  return (outputIRTpp)
}

namesInputForIRTpp = c('RASCH_A1', 'RASCH_A_CONSTAN', 'TWO_PL', 'THREE_PL' ) # name of model for input to IRTpp, for is change in package IRTpp
namesInputForMirt = c( 'Rasch', '1PL_AD', '2PL', '3PL')
dir = '/home/Yeison/SICS'
blocks = c('Bloque_1')
models = c('2PL')


namesDataSet = c('Test_100_38_10000.csv','Test_10_31_1000.csv','Test_20_24_1000.csv','Test_50_17_5000.csv','Test_100_38_5000.csv','Test_10_31_2000.csv','Test_20_24_2000.csv','Test_50_18_10000.csv',
                 'Test_100_39_10000.csv','Test_10_3_2000.csv','Test_20_25_1000.csv','Test_50_18_5000.csv','Test_100_10_10000.csv','Test_100_39_5000.csv','Test_10_32_1000.csv','Test_20_25_2000.csv',
                 'Test_50_19_10000.csv','Test_100_10_5000.csv','Test_100_40_10000.csv','Test_10_32_2000.csv','Test_20_26_1000.csv','Test_50_19_5000.csv','Test_100_1_10000.csv','Test_100_40_5000.csv',
                 'Test_10_33_1000.csv','Test_20_26_2000.csv','Test_50_20_10000.csv','Test_100_11_10000.csv','Test_100_4_10000.csv','Test_10_33_2000.csv','Test_20_27_1000.csv','Test_50_20_5000.csv',
                 'Test_100_11_5000.csv','Test_100_4_5000.csv','Test_10_34_1000.csv','Test_20_27_2000.csv','Test_50_2_10000.csv','Test_100_12_10000.csv','Test_100_5_10000.csv','Test_10_34_2000.csv',
                 'Test_20_28_1000.csv','Test_50_21_10000.csv','Test_100_12_5000.csv','Test_100_5_5000.csv','Test_10_35_1000.csv','Test_20_28_2000.csv','Test_50_21_5000.csv','Test_100_13_10000.csv',
                 'Test_100_6_10000.csv','Test_10_35_2000.csv','Test_20_29_1000.csv','Test_50_22_10000.csv','Test_100_13_5000.csv','Test_100_6_5000.csv','Test_10_36_1000.csv','Test_20_29_2000.csv',
                 'Test_50_22_5000.csv','Test_100_14_10000.csv','Test_100_7_10000.csv','Test_10_36_2000.csv','Test_20_30_1000.csv','Test_50_23_10000.csv','Test_100_14_5000.csv','Test_100_7_5000.csv',
                 'Test_10_37_1000.csv','Test_20_30_2000.csv','Test_50_23_5000.csv','Test_100_1_5000.csv','Test_100_8_10000.csv','Test_10_37_2000.csv','Test_20_3_1000.csv','Test_50_24_10000.csv',
                 'Test_100_15_10000.csv','Test_100_8_5000.csv','Test_10_38_1000.csv','Test_20_31_1000.csv','Test_50_24_5000.csv','Test_100_15_5000.csv','Test_100_9_10000.csv','Test_10_38_2000.csv',
                 'Test_20_31_2000.csv','Test_50_2_5000.csv','Test_100_16_10000.csv','Test_100_9_5000.csv','Test_10_39_1000.csv','Test_20_3_2000.csv','Test_50_25_10000.csv','Test_100_16_5000.csv',
                 'Test_10_10_1000.csv','Test_10_39_2000.csv','Test_20_32_1000.csv','Test_50_25_5000.csv','Test_100_17_10000.csv','Test_10_10_2000.csv','Test_10_40_1000.csv','Test_20_32_2000.csv',
                 'Test_50_26_10000.csv','Test_100_17_5000.csv','Test_10_1_1000.csv','Test_10_40_2000.csv','Test_20_33_1000.csv','Test_50_26_5000.csv','Test_100_18_10000.csv','Test_10_11_1000.csv',
                 'Test_10_4_1000.csv','Test_20_33_2000.csv','Test_50_27_10000.csv','Test_100_18_5000.csv','Test_10_11_2000.csv','Test_10_4_2000.csv','Test_20_34_1000.csv','Test_50_27_5000.csv',
                 'Test_100_19_10000.csv','Test_10_1_2000.csv','Test_10_5_1000.csv','Test_20_34_2000.csv','Test_50_28_10000.csv','Test_100_19_5000.csv','Test_10_12_1000.csv','Test_10_5_2000.csv',
                 'Test_20_35_1000.csv','Test_50_28_5000.csv','Test_100_20_10000.csv','Test_10_12_2000.csv','Test_10_6_1000.csv','Test_20_35_2000.csv','Test_50_29_10000.csv','Test_100_20_5000.csv',
                 'Test_10_13_1000.csv','Test_10_6_2000.csv','Test_20_36_1000.csv','Test_50_29_5000.csv','Test_100_2_10000.csv','Test_10_13_2000.csv','Test_10_7_1000.csv','Test_20_36_2000.csv',
                 'Test_50_30_10000.csv','Test_100_21_10000.csv','Test_10_14_1000.csv','Test_10_7_2000.csv','Test_20_37_1000.csv','Test_50_30_5000.csv','Test_100_21_5000.csv','Test_10_14_2000.csv',
                 'Test_10_8_1000.csv','Test_20_37_2000.csv','Test_50_3_10000.csv','Test_100_22_10000.csv','Test_10_15_1000.csv','Test_10_8_2000.csv','Test_20_38_1000.csv','Test_50_31_10000.csv',
                 'Test_100_22_5000.csv','Test_10_15_2000.csv','Test_10_9_1000.csv','Test_20_38_2000.csv','Test_50_31_5000.csv','Test_100_23_10000.csv','Test_10_16_1000.csv','Test_10_9_2000.csv',
                 'Test_20_39_1000.csv','Test_50_32_10000.csv','Test_100_23_5000.csv','Test_10_16_2000.csv','Test_20_10_1000.csv','Test_20_39_2000.csv','Test_50_32_5000.csv','Test_100_24_10000.csv',
                 'Test_10_17_1000.csv','Test_20_10_2000.csv','Test_20_40_1000.csv','Test_50_33_10000.csv','Test_100_24_5000.csv','Test_10_17_2000.csv','Test_20_1_1000.csv','Test_20_40_2000.csv',
                 'Test_50_33_5000.csv','Test_100_2_5000.csv','Test_10_18_1000.csv','Test_20_11_1000.csv','Test_20_4_1000.csv','Test_50_34_10000.csv','Test_100_25_10000.csv','Test_10_18_2000.csv',
                 'Test_20_11_2000.csv','Test_20_4_2000.csv','Test_50_34_5000.csv','Test_100_25_5000.csv','Test_10_19_1000.csv','Test_20_1_2000.csv','Test_20_5_1000.csv','Test_50_3_5000.csv',
                 'Test_100_26_10000.csv','Test_10_19_2000.csv','Test_20_12_1000.csv','Test_20_5_2000.csv','Test_50_35_10000.csv','Test_100_26_5000.csv','Test_10_20_1000.csv','Test_20_12_2000.csv',
                 'Test_20_6_1000.csv','Test_50_35_5000.csv','Test_100_27_10000.csv','Test_10_20_2000.csv','Test_20_13_1000.csv','Test_20_6_2000.csv','Test_50_36_10000.csv','Test_100_27_5000.csv',
                 'Test_10_2_1000.csv','Test_20_13_2000.csv','Test_20_7_1000.csv','Test_50_36_5000.csv','Test_100_28_10000.csv','Test_10_21_1000.csv','Test_20_14_1000.csv','Test_20_7_2000.csv',
                 'Test_50_37_10000.csv','Test_100_28_5000.csv','Test_10_21_2000.csv','Test_20_14_2000.csv','Test_20_8_1000.csv','Test_50_37_5000.csv','Test_100_29_10000.csv',
                 'Test_10_2_2000.csv','Test_20_15_1000.csv','Test_20_8_2000.csv','Test_50_38_10000.csv','Test_100_29_5000.csv','Test_10_22_1000.csv','Test_20_15_2000.csv','Test_20_9_1000.csv'
                 ,'Test_50_38_5000.csv','Test_100_30_10000.csv','Test_10_22_2000.csv','Test_20_16_1000.csv','Test_20_9_2000.csv','Test_50_39_10000.csv','Test_100_30_5000.csv','Test_10_23_1000.csv',
                 'Test_20_16_2000.csv','Test_50_10_10000.csv','Test_50_39_5000.csv','Test_100_3_10000.csv','Test_10_23_2000.csv','Test_20_17_1000.csv','Test_50_10_5000.csv','Test_50_40_10000.csv',
                 'Test_100_31_10000.csv','Test_10_24_1000.csv','Test_20_17_2000.csv','Test_50_1_10000.csv','Test_50_40_5000.csv','Test_100_31_5000.csv','Test_10_24_2000.csv','Test_20_18_1000.csv',
                 'Test_50_11_10000.csv','Test_50_4_10000.csv','Test_100_32_10000.csv','Test_10_25_1000.csv','Test_20_18_2000.csv','Test_50_11_5000.csv','Test_50_4_5000.csv','Test_100_32_5000.csv',
                 'Test_10_25_2000.csv','Test_20_19_1000.csv','Test_50_12_10000.csv','Test_50_5_10000.csv','Test_100_33_10000.csv','Test_10_26_1000.csv','Test_20_19_2000.csv','Test_50_12_5000.csv',
                 'Test_50_5_5000.csv','Test_100_33_5000.csv','Test_10_26_2000.csv','Test_20_20_1000.csv','Test_50_13_10000.csv','Test_50_6_10000.csv','Test_100_34_10000.csv','Test_10_27_1000.csv',
                 'Test_20_20_2000.csv','Test_50_13_5000.csv','Test_50_6_5000.csv','Test_100_34_5000.csv','Test_10_27_2000.csv','Test_20_2_1000.csv','Test_50_14_10000.csv','Test_50_7_10000.csv',
                 'Test_100_3_5000.csv','Test_10_28_1000.csv','Test_20_21_1000.csv','Test_50_14_5000.csv','Test_50_7_5000.csv','Test_100_35_10000.csv','Test_10_28_2000.csv','Test_20_21_2000.csv',
                 'Test_50_1_5000.csv','Test_50_8_10000.csv','Test_100_35_5000.csv','Test_10_29_1000.csv','Test_20_2_2000.csv','Test_50_15_10000.csv','Test_50_8_5000.csv','Test_100_36_10000.csv',
                 'Test_10_29_2000.csv','Test_20_22_1000.csv','Test_50_15_5000.csv','Test_50_9_10000.csv','Test_100_36_5000.csv','Test_10_30_1000.csv','Test_20_22_2000.csv','Test_50_16_10000.csv',
                 'Test_50_9_5000.csv','Test_100_37_10000.csv','Test_10_30_2000.csv','Test_20_23_1000.csv','Test_50_16_5000.csv','Test_100_37_5000.csv','Test_10_3_1000.csv','Test_20_23_2000.csv',
                 'Test_50_17_10000.csv')

listBlocks <- list()
for ( block in blocks) # covers all blocks
{
  listModels <- list()
  for( model in models ) # covers all models
  {
    listDataSets <- list()
    for ( nameDataSet in namesDataSet ) # covers all datasets
    {
      l <- list()
      
      
      path = paste(dir, block, model, 'Datasets', '' , sep = "/" ) # to made the path of dataset
      path = paste(path, nameDataSet, sep = "" )
      print(path)
      nameModelForIRTpp = ""
      nameModelForMirt = ""
      if ( model == '1PL')
      {
        nameModelForIRTpp = namesInputForIRTpp[1]
        nameModelForMirt = namesInputForMirt[1]
        columns = 1
      }
      else if( model == '1PL_A_Dist')
      {
        nameModelForIRTpp = namesInputForIRTpp[2]
        nameModelForMirt = namesInputForMirt[2]
        columns = 2
      }
      else if( model == '2PL' )
      {
        nameModelForIRTpp = namesInputForIRTpp[3]
        nameModelForMirt = namesInputForMirt[3]
        columns = 2
      }
      else if( model == '3PL')
      {
        nameModelForIRTpp = namesInputForIRTpp[4]
        nameModelForMirt = namesInputForMirt[4]
        columns = 3
      }
      else
      {
        print("Error, model different of 1pl, 2pl,...")
      }
      
      resultMirt = parametersMirt(nameModelForMirt, path)
      resultIRTpp = parametersIRTpp(nameModelForIRTpp, path)
      #resultIRTpp = parametersMirt(nameModelForMirt, path)
      print(resultIRTpp)
      print(resultMirt)
      
      
      #mapP <- c( 'a','b','c')
      #names(mapP) <- c(1, 2, 3)
      
      for ( index in 1:columns)
      {
        difference = abs(resultMirt[,index] - resultIRTpp[,index])
        maxDif = max(difference)
        l <- append( l, maxDif)
      }
      listDataSets <- append( listDataSets, list(l))
    } 
    listModels <- append(listModels, list(listDataSets))
  }
  listBlocks <- append(listBlocks, list(listModels) )
}

print(str(listBlocks))




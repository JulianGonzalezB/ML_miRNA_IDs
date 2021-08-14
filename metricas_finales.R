#SGD
#80%
SG_80_weightedF1 = c(0.71,0.69,0.66,0.70,0.67,0.68,0.70,0.69)
#100%
SG_100_weightedF1 = c(0.70,0.70,0.67)

#ANN
#60%
ANN_60_weightedF1 = c(.076,0.73,0.76)
#80%
ANN_80_weightedF1 = c(0.68,0.69,0.69,0.67,0.69,0.72,0.72,0.78,0.76,0.76)
#100%
ANN_100_weightedF1 = c(0.69,0.72,0.75,0.75,0.74,0.71,0.73,0.71,0.76,0.77,0.75,0.76,0.74,0.65)

###########################
###########################
#SGD
#80%
SG_80_weightedF1 = c(0.64,0.65,0.63,0.66,0.59,0.60,0.65,0.65)
#100%
SG_100_weightedF1 = c(0.65,0.66,0.56)

#ANN
#60%
ANN_60_weightedF1 = c(0.70,0.66,0.71)
#80%
ANN_80_weightedF1 = c(0.65,0.64,0.74,0.70,0.70)
#100%
ANN_100_weightedF1 = c(0.60,0.69,0.70,0.71,0.69,0.63,0.66,0.62,0.71,0.73,0.70,0.71,0.68,0.53)

###########################
#RF hasta el 14 y SVM en adelante 80%
###########################
true_negative= c(5947,5963,6003,6045,5977,6051,5928,5994,6054,6066,6052,5974,6054,6007,6004,8133,8325,8330,6072,8185,8313,8318,6136,8145,8267)
false_positive= c(1068,1124,1072,1079,1023,1027,1073,1043,1048,1057,996,1058,1037,1121,2326,197,5,0,2246,133,5,0,2138,129,7)
false_negative= c(2277,2278,2255,2254,2372,2253,2350,2295,2261,2224,2315,2304,2228,2257,2244,3117,3421,3459,2231,3209,3425,3471,2287,3262,3479)
true_positive= c(2497,2424,2459,2411,2417,2458,2438,2457,2426,2442,2426,2453,2470,2404,1215,342,38,0,1240,262,46,0,1228,253,36)

#Muy sensible a la diferencia de muestreo
classification_accuracy= array(dim = length(true_positive))
#True positive rate. RECALL
sensitivity= array(dim = length(true_positive))
#True negative rate
specificity= array(dim = length(true_positive))
#False Positive rate
false_positive_rate= array(dim = length(true_positive))
#Precision
precision= array(dim = length(true_positive))
#F1
f1= array(dim = length(true_positive))



for (experimento in 1:length(classification_accuracy)) {
  
  total_predictions= true_positive[experimento] + true_negative[experimento] + false_negative[experimento] + false_positive[experimento]
  
  classification_accuracy[experimento]= (true_positive[experimento] + true_negative[experimento])/total_predictions
  
  sensitivity[experimento]= true_positive[experimento]/(false_negative[experimento] + true_positive[experimento])
  
  specificity[experimento]= true_negative[experimento]/(true_negative[experimento] + false_positive[experimento])
  
  false_positive_rate[experimento]= false_positive[experimento]/(true_negative[experimento] + false_negative[experimento])
  
  precision[experimento]= true_positive[experimento]/(true_positive[experimento] + false_positive[experimento])
  
  f1[experimento]= true_positive[experimento] / (true_positive[experimento] + (false_negative[experimento] + false_positive[experimento])/2)
}

###########################
#RF hasta el 11 y SVM en adelante 100%
###########################
#RF1, RF2, SVM, 
true_negative= c(6556,6444,6398,6445,6556,6444,6389,6455,6543,6494,6463,6091,7791,8176,8131,6033,7892,
                 8256,8198)
false_positive= c(882,846,847,863,882,846,847,863,803,856,847,2146,446,61,106,2257,398,
                  34,55)
false_negative= c(1684,1829,1876,1755,1684,1829,1876,1755,1794,1821,1829,2306,2715,3390,3368,2171,2645,
                  3377,3366)
true_positive= c(2667,2670,2668,2716,2667,2670,2668,2716,2649,2618,2650,1246,837,162,184,1328,854,
                 122,170)

#Muy sensible a la diferencia de muestreo
classification_accuracy= array(dim = length(true_positive))
#True positive rate. RECALL
sensitivity= array(dim = length(true_positive))
#True negative rate
specificity= array(dim = length(true_positive))
#False Positive rate
false_positive_rate= array(dim = length(true_positive))
#Precision
precision= array(dim = length(true_positive))
#F1
f1= array(dim = length(true_positive))



for (experimento in 1:length(classification_accuracy)) {
  
  total_predictions= true_positive[experimento] + true_negative[experimento] + false_negative[experimento] + false_positive[experimento]
  
  classification_accuracy[experimento]= (true_positive[experimento] + true_negative[experimento])/total_predictions
  
  sensitivity[experimento]= true_positive[experimento]/(false_negative[experimento] + true_positive[experimento])
  
  specificity[experimento]= true_negative[experimento]/(true_negative[experimento] + false_positive[experimento])
  
  false_positive_rate[experimento]= false_positive[experimento]/(true_negative[experimento] + false_negative[experimento])
  
  precision[experimento]= true_positive[experimento]/(true_positive[experimento] + false_positive[experimento])
  
  f1[experimento]= 2*(precision[experimento] * sensitivity[experimento]) / (precision[experimento] + sensitivity[experimento])
}

#######################
mean(SG_80_weightedF1)
mean(SG_100_weightedF1)

mean(ANN_60_weightedF1)
mean(ANN_80_weightedF1)
mean(ANN_100_weightedF1)

mean(f1[1:11])
mean(f1[12:length(f1)])

mean(f1[1:14])
mean(f1[15:length(f1)])
#######################
sd(SG_80_weightedF1)
sd(SG_100_weightedF1)

sd(ANN_60_weightedF1)
sd(ANN_80_weightedF1)
sd(ANN_100_weightedF1)

sd(f1[1:11])
sd(f1[12:length(f1)])

sd(f1[1:14])
sd(f1[15:length(f1)])
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

#################################################################################################
#################################################################################################
#################################################################################################
#RF1, RF2, SVM, 
true_negative= c(5947,5963,6003,6045,5977,6051,5928,5994,6054,6066,6052,5974,6054,6007)
false_positive= c(1068,1124,1072,1079,1023,1027,1073,1043,1048,1057,996,1058,1037,1121)
false_negative= c(2277,2278,2255,2254,2372,2253,2350,2295,2261,2224,2315,2304,2228,2257)
true_positive= c(2497,2424,2459,2411,2417,2458,2438,2457,2426,2442,2426,2453,2470,2404)

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

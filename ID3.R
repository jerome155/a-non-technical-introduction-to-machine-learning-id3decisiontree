install.packages('data.tree')

#Step 2: Determines whether a leaf node is pure, i.e. the contained data rows
#are only of one distinct Y value.
IsPure <- function(data) {
  length(unique(data[,1])) == 1
}

#Step 3: Calculates the Information Gain (IG) for all features.
#Input: 1st column: Y, 2nd to nth column: Features X1 to Xn
InformationGain_Numeric <- function(train_data) {
  #Create the result data.frame and name the columns.
  EntropyChildNodes <- data.frame()
  EntropyChildNodes <- matrix(nrow=nrow(train_data)-1, ncol=ncol(train_data)-1)
  colnames(EntropyChildNodes) <- colnames(train_data[,2:ncol(train_data)])
  
  #(Slide 66): Calculate the parent node entropy. We calculate this value first,
  #because we need it in every iteration of the preceeding loops.
  ParentNodeProbability <- sum(x=train_data[,1])/nrow(train_data)
  ParentNodeEntropy <- -1*(ParentNodeProbability * log2(ParentNodeProbability) + 
                             (1-ParentNodeProbability) * log2(1-ParentNodeProbability))
  
  #For every feature in input train_data, do the following:
  for (i in 2 : ncol(train_data)) {
    #(Slide 27, 50): Sort Xn in increasing order.
    train_data <- train_data[order(train_data[,i]),]
    #For every row of every feature in input train_data, do the following (loop):
    for (j in 1:(nrow(train_data)-1)) {
      #(Slide 51): If the previous row of the feature has the same value, 
      #skip this row.
      if (train_data[j, i] == train_data[j+1, i]) {
        EntropyChildNodes[j, i-1] <- NA
      } else {
        #(Slide 52, 29): Calculate the subset probability above and below the 
        #set splitting line
        subsetProbabilityAbove <- sum(x=train_data[1:j, 1])/j
        subsetProbabilityBelow <- sum(x=train_data[(j+1):(nrow(train_data)), 1])/(nrow(train_data)-j)
        #(Slide 57, 34): Calculate the binary entropy for child 1 and 2.
        EntropyChildNode1 <- -1*(subsetProbabilityAbove * log2(subsetProbabilityAbove) + 
                                   (1-subsetProbabilityAbove) * log2(1-subsetProbabilityAbove))
        if (is.nan(EntropyChildNode1)) {
          EntropyChildNode1 <- 0
        }
        EntropyChildNode2 <- -1*(subsetProbabilityBelow * log2(subsetProbabilityBelow) + 
                                   (1-subsetProbabilityBelow) * log2(1-subsetProbabilityBelow))
        if (is.nan(EntropyChildNode2)) {
          EntropyChildNode2 <- 0
        }
        #(Slide 61): Calculate the split entropy for every possible split.
        EntropyChildNodes[j, i-1] <- ParentNodeEntropy - 
          (j/nrow(train_data)*EntropyChildNode1 + 
             (nrow(train_data)-j)/nrow(train_data)*EntropyChildNode2)
      }
    }
  }
  #Return the calculated values (without NA values)
  EntropyChildNodes[is.na(EntropyChildNodes)] <- 0
  EntropyChildNodes
}

#Step 1-5: Inputs are the starting node (into which we get our returned tree), as well as our data.frame().
#This function continuously calls itself for every child it generates.
TrainID3 <- function(node, data) {
  
  #Into the current node, store the amount of observations inside data.
  node$obsCount <- nrow(data)
  
  #If the dataset is pure, we arrived at a leaf, execution ends here.
  if (IsPure(data)) {
    #Add a final child and store all necessary information into that child.
    child <- node$AddChild(unique(data[,1]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
    child$value <- 0
    #The function stops execution here.
    
  } else {
    #We are not in a leaf, therefore we need to split. We calculate the Information Gain for all features.
    ig <- InformationGain_Numeric(data)
    #(Slide 41): We find the feature that has the highest possible information gain.
    igFeatureColumn <- which.max(apply(ig, 2, which.max))
    
    #If there are two nodes with the same maximum information gain, select one of them randomly
    if (length(which(ig[,igFeatureColumn]==max(ig[,igFeatureColumn])))>1) {
      igFeatureColumnSplitPosition <- sample(which(ig[,igFeatureColumn]==max(ig[,igFeatureColumn])),1)
    } else {
      igFeatureColumnSplitPosition <- which.max(ig[,igFeatureColumn])
    }
    
    #Order the data of the original dataset based on the feature with the highest Information Gain.
    data <- data[order(data[,igFeatureColumn+1]),]
    
    #Store the selected feature (with the highest IG), as well as the splitting value into the current node.
    node$feature <- colnames(data)[igFeatureColumn+1]
    node$value <- (data[igFeatureColumnSplitPosition, igFeatureColumn+1] + 
                     data[igFeatureColumnSplitPosition+1, igFeatureColumn+1])/2
    
    #Create the children nodes containing the split data rows.
    childObs1 <- data[1:igFeatureColumnSplitPosition,];
    childObs2 <- data[(igFeatureColumnSplitPosition+1):nrow(data),]
    childObs <- list(childObs1, childObs2)
    
    #Name the children based on the feature split.
    names(childObs) <- c(paste0(node$feature, "<=", 
                                (data[igFeatureColumnSplitPosition, igFeatureColumn+1] + 
                                   data[igFeatureColumnSplitPosition+1, igFeatureColumn+1])/2),
                         paste0(node$feature, ">", 
                                (data[igFeatureColumnSplitPosition, igFeatureColumn+1] + 
                                   data[igFeatureColumnSplitPosition+1, igFeatureColumn+1])/2))
    
    #Call this function recusively for every child node that was just created.
    for(i in 1:length(childObs)) {
      #Add one of the childs to the tree
      child <- node$AddChild(names(childObs)[i])
      
      #Call the TrainID3 algorithm again with the child node.     
      TrainID3(child, childObs[[i]])
    }
  }
}

PredictID3 <- function(tree, features) {
  #If we are in a leaf, return the name of that leaf (= the classification)
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  #Decide on which child to follow.
  if (features[,tree$feature]<=tree$value) {
    child <- tree$children[[1]]
  } 
  else {
    child <- tree$children[[2]]
  }
  #Recursively call this method for the newly chosen child, until a leaf is reached.
  return(PredictID3(child, features))
}

library(data.tree)

#Input data from the slides
train_data <- data.frame(index = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                         Y = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L),
                         X1 = c(2, 2.8, 1.5, 2.1, 5.5, 8, 6.9, 8.5, 2.5, 7.7),
                         X2 = c(1.5, 1.2, 1, 1, 4, 4.8, 4.5, 5.5, 2, 3.5))

#The index is not necessary for this computation, therefore remove it.
train_data <- train_data[,c('Y', 'X1', 'X2')]
#This seed is used to define randomness, given that two rows of a feature have
#the same Information Gain.
set.seed(12345) #Correct seed (results as in slides)
#set.seed(1234) #Wrong seed (results different from slides)

#Create a new, empty tree.
tree <- Node$new("MachineLearningExample")
#Run the algorithm (All steps from the slides)
TrainID3(tree, train_data)
#Show the results
print(tree, "feature", "obsCount")

#Trying to predict two new sets of points.
prediction_data <- data.frame(X1=3.19, X2=1.50)
PredictID3(tree, prediction_data)

prediction_data2 <- data.frame(X1=0, X2=10.0)
PredictID3(tree, prediction_data2)

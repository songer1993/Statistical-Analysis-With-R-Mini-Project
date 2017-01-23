# There are some helper functions

ShortSummary <- function(model){
    # Print a tidy data frame version of lm model summary
    #
    # Args:
    #   model: A fitted linear model to be summarised
    #
    # Returns:
    #   None
    
    # Require packages
    require(broom)
    
    # Tidy and print model summary
    estimates <- tidy(model)
    metrics <- glance(model)
    print(estimates)
    print(metrics)
}

BetterPairs <- function(data) {
    # Create a customised pairs plot showing scatter plots,
    # histograms and correlation coefficients
    #
    # Args:
    #   data: Data frame to be plotted
    #
    # Returns:
    #   A customised pairs plot
    
    return(pairs(data,
                 # Create function to plot pairwise scatterplot and draw linear regression line
                 upper.panel=panel.regression <- function(x, y, col = rgb(0, 0, 0, 0.3), bg = NA, pch = 19, 
                                                              cex = 1, col.regres = "blue", col.smooth = "red", 
                                                              span = 2/3, iter = 3, ...) { 
                     points(x, y, pch = pch, col = col, bg = bg, cex = cex) 
                     ok <- is.finite(x) & is.finite(y) 
                     if (any(ok)) {
                         abline(stats::lm(y[ok] ~ x[ok]), col = col.regres, ...)
                         lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
                     }
                 }, 
                 # Create function to compute and print correlation
                 lower.panel=panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
                     # Set user coordinates of plotting region
                     usr <- par("usr"); on.exit(par(usr)) 
                     par(usr = c(0, 1, 0, 1)) 
                     r <- abs(cor(x, y)) 
                     txt <- format(c(r, 0.123456789), digits=digits)[1] 
                     txt <- paste(prefix, txt, sep="") 
                     if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
                     
                     test <- cor.test(x,y) 
                     # borrowed from printCoefmat
                     Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                      symbols = c("***", "**", "*", ".", " ")) 
                     
                     text(.5, .5, txt, cex = cex * sqrt(r) * 1.2) 
                     text(.8, .8, Signif, cex=cex, col=2) 
                 },
                 # Create function to make histogram with density superimposed
                 diag.panel=panel.hist <- function(x, ...) {
                     # Set user coordinates of plotting region
                     usr <- par("usr"); on.exit(par(usr))
                     par(usr = c(usr[1:2], 0, 1.5))
                     # Do not start new plot
                     par(new=TRUE)
                     # Draw histogram
                     hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="",
                          main="", col="cyan")
                     # Add density curve
                     lines(density(x, na.rm=TRUE))
                     # Add rug representation
                     rug(x)
                 }))
}


#input actual & predicted vectors or actual vs predicted confusion matrix 
ClassificationReport <- function(actual=NULL, predicted=NULL, cm=NULL){
    # Print a classification report consisting of confusion matrix,
    # accuracy, precision, recall and F1 scores
    #
    # Args:
    #   actual: Testing labels
    #   predicted: Predicted testing labels
    #   cm: Confustion matrix
    #
    # Returns:
    #   A classification report in the form of list
    
    if(is.null(cm)) {
        naVals <- union(which(is.na(actual)), which(is.na(predicted)))
        if(length(naVals) > 0) {
            actual <- actual[-naVals]
            predicted <- predicted[-naVals]
        }
        f <- factor(union(unique(actual), unique(predicted)))
        actual <- factor(actual, levels = levels(f))
        predicted <- factor(predicted, levels = levels(f))
        cm <- as.matrix(table(Actual=actual, Predicted=predicted))
    }
    
    n <- sum(cm) # number of instances
    nc <- nrow(cm) # number of classes
    diag <- diag(cm) # number of correctly classified instances per class 
    rowsums <- apply(cm, 1, sum) # number of instances per class
    colsums <- apply(cm, 2, sum) # number of predictions per class
    p <- rowsums / n # distribution of instances over the classes
    q <- colsums / n # distribution of instances over the predicted classes
    
    #accuracy
    accuracy <- sum(diag) / n
    
    #per class prf
    recall <- diag / rowsums
    precision <- diag / colsums
    f1 <- 2 * precision * recall / (precision + recall)
    
    
    classNames <- names(diag)
    if(is.null(classNames)) {
        classNames <- paste("C",(1:nc),sep="")
    }
    
    metrics <- rbind(
        Accuracy = accuracy,
        Precision = precision,
        Recall = recall,
        F1 = f1)
    
    colnames(metrics) <- classNames
    
    return(list(ConfusionMatrix = cm, Accuracy = accuracy, Metrics = metrics))
}

LogisticRegressionDecisionBoundary <- function(model, colour = "black", ...){
    # Draw decision boundary for logistic regression models
    #
    # Args:
    #   model: A logisitic regression model
    #   colour: Colour of decision boundary line
    #
    # Returns:
    #   A ggplot line
    
    # Compute slope and intercept from model coefficient
    slope <- coef(model)[2]/(-coef(model)[3])
    intercept <- coef(model)[1]/(-coef(model)[3])
    
    # Return plot
    return(
        geom_abline(slope = slope, intercept = intercept, colour = colour)
    )
}


LdaQdaDecisionBoundary <- function(data.X, model, colour = "red2"){
    # Draw decision boundary for LDA or QDA models
    #
    # Args:
    #   data: Training data variables
    #   model: A LDA or QDA model
    #   colour: Colour of decision boundary
    #
    # Returns:
    #   A ggplot contour  

    # Require packages
    require(MASS)
    require(tidyverse)
    
    # Create test data covering a grid
    xlim <- range(data.X[, 1])
    x <- seq(xlim[1], xlim[2], length.out = 300)  
    ylim <- range(data.X[, 2])
    y <- seq(ylim[1], ylim[2], length.out = 300)
    test.grid <- expand.grid(list(x,y))
    names(test.grid) <- names(data.X) # match column names for prediction

    # Prediction for that grid
    preds <-predict(model, newdata = test.grid)
    predclass <- preds$class
    
    # Data structure for plotting
    names(test.grid) <- c("x", "y")
    df <- cbind(test.grid, predclass)
    df$classnum <- as.numeric(df$predclass)
    
    # Return plot
    return(geom_contour(data=df, 
                        aes(x = x, y = y, z = classnum), 
                        colour = colour, 
                        breaks= c(1.5,2.5))
    )
}

KnnDecisionBoundary <- function(training.X, training.Y, k = 1){
    # Draw decision boundary for KNN models
    #
    # Args:
    #   training.X: Training data variables
    #   training.Y: Training data lables
    #   k: Number of neighbors
    #
    # Returns:
    #   A ggplot contour     
    
    # Require packages
    require(tidyverse)
    
    # Create test data covering a grid
    test.grid <- expand.grid(x=seq(min(training.X[,1]), max(training.X[,1]),
                              by=0.1),
                        y=seq(min(training.X[,2]), max(training.X[,2]), 
                              by=0.1))
    
    # Classification and prediction for that grid
    classif <- knn(training.X, test.grid, training.Y, k = k, prob=TRUE)
    prob <- attr(classif, "prob")
    
    # Data structure for plotting
    df <- bind_rows(mutate(test.grid,
                           prob=prob,
                           cls=1,
                           prob_cls=ifelse(classif==cls,
                                           1, 0)),
                    mutate(test.grid,
                           prob=prob,
                           cls=0,
                           prob_cls=ifelse(classif==cls,
                                           1, 0)))
    df <- mutate(df, Y.label = as.factor(cls))
    
    # Return plot
    return(geom_contour(data = df,
                        aes(x = x, y = y, z = prob_cls, group = Y.label, color = Y.label),
                        bins=2)
    )
}


PlotTree <- function(data){
    # Plot a full tree model
    #
    # Args:
    #   data: A data frame
    #
    # Returns:
    #   A text tree model
    
    # Require packages
    require(tree)
    
    # Create and print a full tree model
    model <- tree(Y ~ ., data = data)
    plot(model)
    text(model)
}

PredictRegsubsets <- function(object, newdata, id, ...){
    # Predict new data for a subset model
    #
    # Args:
    #   object: A fitted subset selection model
    #   data: New data to be predicted
    #   id: Number of precdictors
    #
    # Returns:
    #   A matrix of computed prediction values
    
    # Require packages
    require(leaps)
    
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

MSE <- function(pred, actual){
    # Calculate MSe
    #
    # Args:
    #   pred: Predication values
    #   acutal: Actual values
    #
    # Returns:
    #   A MSE value
    
    return(
        list(MSE = mean((pred - actual) ^ 2))
    )
}

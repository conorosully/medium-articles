# We train a random forest on the Boston dataset:
data("Boston", package = "MASS")
library("rpart")
library("iml")

rf <- rpart(medv ~ ., data = Boston)






# Compute the accumulated local effects for the first feature
eff <- FeatureEffect$new(mod, feature = "rm", grid.size = 30)
eff$plot()


# Again, but this time with a partial dependence plot and ice curves

mod <- Predictor$new(rf, data = Boston)

eff <- FeatureEffect$new(mod,
                         feature = "rm", method = "pdp+ice",
                         grid.size = 30
)
plot(eff)


# We train a random forest on the Boston dataset:
library("rpart")
data("Boston", package = "MASS")
rf <- rpart(medv ~ ., data = Boston)
mod <- Predictor$new(rf, data = Boston)
# Compute the accumulated local effects for all features
eff <- FeatureEffects$new(mod)
eff$plot()
## Not run:
# Again, but this time with a partial dependence plot
eff <- FeatureEffects$new(mod, method = "pdp")
eff$plot()



# Load the sample data
data(mtcars)

# Fit a projection pursuit regression model
model <- ppr(mpg ~ ., data = mtcars, nterms = 1)

# Construct variable importance plot
vip(model, method = "permute",train = dataset, target = "price",metric = "rmse",pred_wrapper = pfun, geom = "boxplot",
    all_permutations = TRUE, mapping = aes_string(fill = "Variable"),
    aesthetics = list(color = "grey35", size = 0.8))




install.packages("mlbench")


set.seed(101)  # for reproducibility
trn <- as.data.frame(mlbench::mlbench.friedman1(500))  # ?mlbench.friedman1

# Inspect data
tibble::as.tibble(trn)


# Load required packages
library(pdp)

# Fit a PPR model (nterms was chosen using the caret package with 5 repeats of 
# 5-fold cross-validation)
pp <- ppr(y ~ ., data = trn, nterms = 11)  

# PDPs for all 10 features
features <- paste0("x.", 1:10)
pdps <- lapply(features, FUN = function(feature) {
  pd <- partial(pp, pred.var = feature)
  autoplot(pd) + 
    ylim(range(trn$y)) + 
    theme_light()
})
grid.arrange(grobs = pdps, ncol = 5)


vip(rf, method = "firm",ice = TRUE)




aucpr_metric <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  pr_curve <- PRROC::pr.curve(scores.class0 = preds, weights.class0 = as.numeric(labels == 0))
  aucpr <- pr_curve$auc.integral
  return(list(name = "AUCPR", value = aucpr, higher_better = TRUE))
}
lightgbm_caret_wrapper <- list(
  label='Light GBM',
  library = "lightgbm",
  type = "Classification",
  parameters = data.frame(
    parameter = c("nrounds", "max_depth", "min_child_samples", "learning_rate",  "feature_fraction", "min_gain_to_split", "bagging_fraction"),
    class = rep("numeric", 7),
    label = c("Boosting Rounds", "Max Tree Depth", "Min Child Samples", "Learning Rate", "Feature Fraction", "Min Gain to Split", "Bagging Fraction")
  ),
  grid = function (x, y, len = NULL, search = "grid") {
    if (search == "grid") {out <- expand.grid(max_depth = seq(1, len), nrounds = floor((1:len) * 200), learning_rate = c(0.01,0.05), min_gain_to_split = c(0.5,0.7), feature_fraction = c(0.6, 
            0.8), min_child_samples = c(1), bagging_fraction = seq(0.5,0.7))}
	out},
  loop = function (grid,...) 
  {
    loop <- plyr::ddply(grid, c("learning_rate", "max_depth", "min_child_samples", 
        "feature_fraction", "min_gain_to_split", "bagging_fraction"), 
        function(x) c(nrounds = max(x$nrounds)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$nrounds)) {
        index <- which(grid$max_depth == loop$max_depth[i] & 
            grid$learning_rate == loop$learning_rate[i] & grid$min_child_samples == loop$min_child_samples[i] & 
            grid$feature_fraction == loop$feature_fraction[i] & 
            grid$min_gain_to_split == loop$min_gain_to_split[i] & 
            grid$bagging_fraction == loop$bagging_fraction[i])
        trees <- grid[index, "nrounds"]
        submodels[[i]] <- data.frame(nrounds = trees[trees != 
            loop$nrounds[i]])
    }
    list(loop = loop, submodels = submodels)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
	  x <- as.matrix(x)
	  y <- as.numeric(y) - 1
	  x <- lightgbm::lgb.Dataset(x, label = y)
	  if (!is.null(wts)) 
		lightgbm::set_field(x, "weight", wts)
	  param0=list(objective = "binary", metric = "auc",learning_rate = param$learning_rate, max_depth = param$max_depth, 
		  min_child_samples = param$min_child_samples, feature_fraction = param$feature_fraction, 
		  min_gain_to_split = param$min_gain_to_split, bagging_fraction = param$bagging_fraction,verbosity=-1)
	  #out <- lightgbm::lgb.train(params=param0, data = x, nrounds = param$nrounds,feval = aucpr_metric)
	  out <- lightgbm::lgb.train(params=param0, data = x, nrounds = param$nrounds)
	  xNames <- colnames(x)
	  out=list(model = out, xNames = xNames)
	  out
	},
  predict = function (modelFit, newdata, submodels = NULL,...) 
  {
	newdata <- as.matrix(newdata)
    out <- predict(modelFit$model, newdata)
	out <- factor(paste0('X',as.integer(out > 0.5)),levels = c('X0', 'X1'))
	#out <- as.integer(out > 0.5)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nrounds)) {
            tmp_pred <- predict(modelFit$model, newdata, num_iteration = submodels$nrounds[j])
            tmp[[j + 1]] <- factor(paste0('X',as.integer(tmp_pred > 0.5)),levels = c('X0', 'X1'))
			#tmp[[j + 1]] <- as.integer(tmp_pred > 0.5)
        }
        out <- tmp
    }
    out
  },
  prob = function (modelFit, newdata, submodels = NULL,...) 
  {
    newdata <- as.matrix(newdata)
    out <- predict(modelFit$model, newdata)
    out <- cbind(1-out,out)
    colnames(out) <- c('X0','X1')
    out <- as.data.frame(out, stringsAsFactors = TRUE)
    if (!is.null(submodels)) {
        tmp <- vector(mode = "list", length = nrow(submodels) + 
            1)
        tmp[[1]] <- out
        for (j in seq(along = submodels$nrounds)) {
            tmp_pred <- predict(modelFit$model, newdata, num_iteration = submodels$nrounds[j])
            tmp_pred <- cbind( 1 - tmp_pred,tmp_pred)
            colnames(tmp_pred) <- c( 'X0','X1')
			#colnames(tmp_pred) <- c('1', '0')
            tmp_pred <- as.data.frame(tmp_pred, stringsAsFactors = TRUE)
            tmp[[j + 1]] <- tmp_pred
        }
        out <- tmp
    }
	out
  },
  predictors = function (x, ...) 
  {
    imp <- lightgbm::lgb.importance(model = x)
    imp$Feature
  },
  levels = c('X0', 'X1'),
  varImp = function (object, scale=T,...) {
    imp <- lightgbm::lgb.importance(model = object$model)
	imp <- as.data.frame(imp, stringsAsFactors = TRUE)[, 1:2]
    rownames(imp) <- as.character(imp[, 1])
    imp <- imp[, 2, drop = FALSE]
    colnames(imp) <- "Overall"
	if(scale)imp=(imp-min(imp))/(max(imp)-min(imp))*100
	imp
  }
)
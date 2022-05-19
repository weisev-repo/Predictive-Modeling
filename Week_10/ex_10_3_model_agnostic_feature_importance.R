library(iml)

# model lin_reg02 has to be trained in advance 

mod <- Predictor$new(lin_reg02, data=train)
mod_test <- Predictor$new(lin_reg02, data=test)

# Partial Depence Plot

eff_test <- FeatureEffects$new(mod_test, method='pdp')
eff_test$plot()

eff_test <- FeatureEffects$new(mod_test, method='pdp')
eff_test$plot()

# Accumulated Local Effects

eff <- FeatureEffects$new(mod, method='ale')
eff$plot()
eff_test <- FeatureEffects$new(mod_test, method='ale')
eff_test$plot()


# Permutation Feature Importance

imp <- FeatureImp$new(mod, loss='mae')
plot(imp)
mod_test <- Predictor$new(lin_reg02, data=test)
imp_test <- FeatureImp$new(mod_test, loss='mae')
plot(imp_test)

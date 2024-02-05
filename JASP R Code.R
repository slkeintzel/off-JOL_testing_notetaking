# ----------------------------------------------------------------------------#
# JASP - R Code for 
# Experts' and Novices' Metacognitive Judgements about Effectiveness of 
# Retrieval Practice and Note-Taking - 2023
# ----------------------------------------------------------------------------#

## DO NOT RUN

# Study 1: off-JOL ANOVA
jaspAnova::AnovaRepeatedMeasuresBayesian(
  version = "0.17.2",
  customPriorSpecification = list(list(components = "Time", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = "Learning Method", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "Learning Method"), inclusionProbability = 0.5, scaleFixedEffects = 0.5)),
  modelTerms = list(list(components = "Time", isNuisance = FALSE), list(components = "Learning Method", isNuisance = FALSE), list(components = list("Time", "Learning Method"), isNuisance = FALSE)),
  repeatedMeasuresCells = list("TE01_01", "NO01_01", "TE03_01", "NO03_01", "TE05_01", "NO05_01"),
  repeatedMeasuresFactors = list(list(levels = list("5 mins", "1 week", "2 weeks"), name = "Time"), list(levels = list("Testing", "Note Taking"), name = "Learning Method")),
  singleModelTerms = list("Time", "Learning Method", list("Time", "Learning Method")))

# Study 1: confidence ANOVA
jaspAnova::AnovaRepeatedMeasuresBayesian(
  version = "0.17.2",
  customPriorSpecification = list(list(components = "Time", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = "Learning Method", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "Learning Method"), inclusionProbability = 0.5, scaleFixedEffects = 0.5)),
  modelTerms = list(list(components = "Time", isNuisance = FALSE), list(components = "Learning Method", isNuisance = FALSE), list(components = list("Time", "Learning Method"), isNuisance = FALSE)),
  repeatedMeasuresCells = list("TE02_01", "NO02_01", "TE04_01", "NO04_01", "TE06_01", "NO06_01"),
  repeatedMeasuresFactors = list(list(levels = list("5 mins", "1 week", "2 weeks"), name = "Time"), list(levels = list("Testing", "Note Taking"), name = "Learning Method")),
  singleModelTerms = list("Time", "Learning Method", list("Time", "Learning Method")))


# Study 2: off-JOL ANOVA
jaspAnova::AnovaRepeatedMeasuresBayesian(
  version = "0.17.2",
  betweenSubjectFactors = "expertise",
  customPriorSpecification = list(list(components = "Time", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = "Learning Method", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "Learning Method"), inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = "expertise", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "expertise"), inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Learning Method", "expertise"), inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "Learning Method", "expertise"), inclusionProbability = 0.5, scaleFixedEffects = 0.5)),
  modelTerms = list(list(components = "Time", isNuisance = FALSE), list(components = "Learning Method", isNuisance = FALSE), list(components = "expertise", isNuisance = FALSE), list(components = list("Time", "Learning Method"), isNuisance = FALSE), list(components = list("Time", "expertise"), isNuisance = FALSE), list(components = list("Learning Method", "expertise"), isNuisance = FALSE), list(components = list("Time", "Learning Method", "expertise"), isNuisance = FALSE)),
  repeatedMeasuresCells = list("TE01_01", "NO01_01", "TE03_01", "NO03_01", "TE05_01", "NO05_01"),
  repeatedMeasuresFactors = list(list(levels = list("5 mins", "1 week", "2 weeks"), name = "Time"), list(levels = list("Testing", "Note Taking"), name = "Learning Method")),
  singleModelTerms = list("Time", "Learning Method", "expertise", list("Time", "Learning Method"), list("Time", "expertise"), list("Learning Method", "expertise"), list("Time", "Learning Method", "expertise")))

# Study 2: confidence ANOVA
jaspAnova::AnovaRepeatedMeasuresBayesian(
  version = "0.17.2",
  bayesFactorOrder = "nullModelTop",
  betweenSubjectFactors = "expertise",
  customPriorSpecification = list(list(components = "Time", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = "Learning Method", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = "expertise", inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "Learning Method"), inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "expertise"), inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Learning Method", "expertise"), inclusionProbability = 0.5, scaleFixedEffects = 0.5), list(components = list("Time", "Learning Method", "expertise"), inclusionProbability = 0.5, scaleFixedEffects = 0.5)),
  modelTerms = list(list(components = "Time", isNuisance = FALSE), list(components = "Learning Method", isNuisance = FALSE), list(components = "expertise", isNuisance = FALSE), list(components = list("Time", "Learning Method"), isNuisance = FALSE), list(components = list("Time", "expertise"), isNuisance = FALSE), list(components = list("Learning Method", "expertise"), isNuisance = FALSE), list(components = list("Time", "Learning Method", "expertise"), isNuisance = FALSE)),
  repeatedMeasuresCells = list("TE02_01", "NO02_01", "TE04_01", "NO04_01", "TE06_01", "NO06_01"),
  repeatedMeasuresFactors = list(list(levels = list("5 mins", "1 week", "2 weeks"), name = "Time"), list(levels = list("Testing", "Note Taking"), name = "Learning Method")),
  singleModelTerms = list("Time", "Learning Method", "expertise", list("Time", "Learning Method"), list("Time", "expertise"), list("Learning Method", "expertise"), list("Time", "Learning Method", "expertise")))


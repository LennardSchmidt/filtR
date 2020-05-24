library(simstudy)

def <- defData(varname = "male", dist = "binary", formula = 0.5, id = "cid")
def <- defData(def, varname = "x1", dist = "uniform", formula = "10;20")
def <- defData(def, varname = "y1", formula = "nr + x1 * 2", variance = 8)
def <- defData(def, varname = "y2", dist = "poisson", formula = "nr - 0.2 * x1",
               link = "log")
def <- defData(def, varname = "xnb", dist = "negBinomial", formula = "nr - 0.2 * x1",
               variance = 0.05, link = "log")
def <- defData(def, varname = "xCat", formula = "0.3;0.2;0.5", dist = "categorical")
def <- defData(def, varname = "g1", dist = "gamma", formula = "5+xCat", variance = 1,
               link = "log")
def <- defData(def, varname = "b1", dist = "beta", formula = "1+0.3*xCat", variance = 1,
               link = "logit")
def <- defData(def, varname = "a1", dist = "binary", formula = "-3 + xCat",
               link = "logit")
def <- defData(def, varname = "a2", dist = "binomial", formula = "-3 + xCat",
               variance = 100, link = "logit")

df <- genData(1000, def)

df.treat <- trtAssign(df, n = 3, balanced = TRUE, grpName = "treat")

get_filter(data = df.treat, outcome = "x1", treatment = "treat", filtervars = c("male", "xcat"))

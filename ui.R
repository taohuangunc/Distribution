library(shiny)
library(shinydashboard)
library(rmarkdown)

pageTitle <- "Distributions"

# JavaScript Files
jsFiles <- tags$head(
  tags$script(src="js/google-analytics.js")
)

# Panel for Distributions
distPanel <- function(name) {
    box(width = 5,
        status = "primary",
        title = name,
        "Reference : ",
      a(target = "_blank",
        href = paste0('http://en.wikipedia.org/wiki/', en),
        'Wikipedia',
        img(src = 'img/external.png')
      )
    )
  }
}

header <- dashboardHeader(title = pageTitle)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Continuous", icon = icon("line-chart"),
      menuSubItem("Normal", tabName = "norm"),
      menuSubItem("Erlang", tabName = "erlang"),
      menuSubItem("F", tabName = "f"),
      menuSubItem("NC F", tabName = "ncf"),
      menuSubItem("Chi-squares", tabName = "chisq"),
      menuSubItem("NC Chisq", tabName = "ncChisq"),
      menuSubItem("Gamma", tabName = "gamma"),
      menuSubItem("Cauchy", tabName = "cauchy"),
      menuSubItem("Exponential", tabName = "exp"),
      menuSubItem("Log Normal", tabName = "lnormal"),
      menuSubItem("T", tabName = "t"),
      menuSubItem("NC T", tabName = "nct"),
      menuSubItem("Beta", tabName = "beta"),
      menuSubItem("NC Beta", tabName = "ncbeta"),
      menuSubItem("Uniform", tabName = "unif"),
      menuSubItem("Logis", tabName = "logis"),
      menuSubItem("Weibull", tabName = "weibull")
    ),
    menuItem("Discrete", icon = icon("bar-chart-o"),
      menuSubItem("Geometric", tabName = "geom"),
      menuSubItem("Hyper Geometric", tabName = "hyper"),
      menuSubItem("Binomial", tabName = "binom"),
      menuSubItem("Negative Binomial", tabName = "nbinom"),
      menuSubItem("Poisson", tabName = "pois"),
      menuSubItem("Dunif", tabName = "dunif")
    ),
  )
)

board.about <- tabItem(tabName = "about",
  fluidRow(
    column(12,
      includeMarkdown("about.md")
    )
  )
)
####################################################
# Tab Items for Distributions
####################################################
#  
board.norm <- tabItem(tabName = "norm",
  fluidRow(
    distPanel("Normal"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^{2}}}
        \\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2} \\right)
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "Normal", status = "primary", solidHeader = TRUE,
      radioButtons(paste("norm", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("norm", "range", sep="."), "Range",
        min = -100, max = 100, value = c(-10, 10), step= 1
      ),
      sliderInput(paste("norm", "mean", sep="."), "Expectation \\(\\mu\\)",
        min = -50, max = 50, value = 0, step= 1
      ),
      sliderInput(paste("norm", "sd", sep="."), "SD \\(\\sigma\\)",
        min = 0, max = 10, value = 1, step= 0.5
      )
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("normalPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("normal.meanBox", width = 6),
    valueBoxOutput("normal.varianceBox", width = 6)
  )
)

board.erlang <- tabItem(tabName = "erlang",
  fluidRow(
    distPanel("Erlang"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x; n, \\lambda)=
        {\\lambda^{n} x^{n-1} e^{-\\lambda x} \\over (n-1)!}\\quad\\mbox{for }x>0
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "Erlang", status = "primary", solidHeader = TRUE,
      radioButtons(paste("erlang", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("erlang", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("erlang", "shape", sep="."), "\\(n\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("erlang", "scale", sep="."), "\\(\\lambda\\)",
        min = 0.5, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "fdddasd", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("erlangPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("erlang.meanBox", width = 6),
    valueBoxOutput("erlang.varianceBox", width = 6)
  )
)

board.f <- tabItem(tabName = "f",
  fluidRow(
    distPanel("F"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x) = \\frac{1}{\\mathrm{B}(d_1/2, d_2/2)} \\; 
        \\left(\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_1/2} \\; 
        \\left(1-\\frac{d_1\\,x}{d_1\\,x + d_2}\\right)^{d_2/2} \\; x^{-1}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "adsf", status = "primary", solidHeader = TRUE,
      radioButtons(paste("f", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("f", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("f", "df1", sep="."), "DF \\(d_1\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("f", "df2", sep="."), "DF \\(d_2\\)",
        min = 1, max = 20, value = 1, step= 1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("fPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("f.meanBox", width = 6),
    valueBoxOutput("f.varianceBox", width = 6)
  )
)

board.ncf <- tabItem(tabName = "ncf",
  fluidRow(
    distPanel("NC F", "Noncentral_F-distribution"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      f(x) =\\sum\\limits_{k=0}^\\infty
      \\frac{e^{-\\lambda/2}(\\lambda/2)^k}
      { B\\left(\\frac{\\nu_2}{2},\\frac{\\nu_1}{2}+k\\right) k!}
      \\left(\\frac{\\nu_1}{\\nu_2}\\right)^{\\frac{\\nu_1}{2}+k}
      \\left(\\frac{\\nu_2}{\\nu_2+\\nu_1x}\\right)
      ^{\\frac{\\nu_1+\\nu_2}{2}+k}x^{\\nu_1/2-1+k}
      \\ \\ \\ \\ \\mathrm{for\\ } x > 0
$$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "nc f", status = "primary", solidHeader = TRUE,
      radioButtons(paste("ncf", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("ncf", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("ncf", "df1", sep="."), "DF \\(\\nu_1\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("ncf", "df2", sep="."), "DF \\(\\nu_2\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("ncf", "ncp", sep="."), "NC degree \\(\\lambda\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("ncfPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("ncf.meanBox", width = 12),
    valueBoxOutput("ncf.varianceBox", width = 12)
  )
)

board.chisq <- tabItem(tabName = "chisq",
  fluidRow(
    distPanel("chisq"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x;k)=\\frac{(1/2)^{k/2}}{\\Gamma(k/2)} x^{k/2 - 1} e^{-x/2}
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "adfs", status = "primary", solidHeader = TRUE,
      radioButtons(paste("chisq", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("chisq", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("chisq", "df", sep="."), "DF \\(k\\)",
        min = 1, max = 20, value = 1, step= 1)
    ),
    box(
      width = 7,
      title = "adfaf", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("chisqPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("chisq.meanBox", width = 6),
    valueBoxOutput("chisq.varianceBox", width = 6)
  )
)

board.ncChisq <- tabItem(tabName = "ncChisq",
  fluidRow(
    distPanel("NC chi"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f_X(x; k,\\lambda) =
        \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x),
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0,\\ \\ Y_q \\sim\\chi^2_q \\ 
      $$")
#       helpText("$$
#         f_X(x; k,\\lambda) =
#         \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x)
#         \\ \\ \\ \\ \\mathrm{for\\ } x > 0\\\\
#         \\\\ Y_q \\mathrm{\\ は自由度\\ } q \\mathrm{\\ のカイ二乗分布に従う\\ } 
#       $$")
#       helpText(paste0("\\(
#         f_X(x; k,\\lambda) =
#         \\sum_{i=0}^\\infty \\frac{e^{-\\lambda/2} (\\lambda/2)^i}{i!} f_{Y_{k+2i}}(x)
#         \\ \\ \\ \\ \\mathrm{for\\ } x > 0\\\\
#         \\\\ Y_q\\)", "は自由度", "\\(q\\)", "のカイ二乗分布に従う"
#       ))
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "adfda", status = "primary", solidHeader = TRUE,
      radioButtons(paste("ncChisq", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("ncChisq", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("ncChisq", "df", sep="."), "DF \\(k\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("ncChisq", "ncp", sep="."), "NC degree \\(\\lambda\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("ncChisqPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("ncChisq.meanBox", width = 6),
    valueBoxOutput("ncChisq.varianceBox", width = 6)
  )
)

board.gamma <- tabItem(tabName = "gamma",
  fluidRow(
    distPanel("Gamma"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x) = x^{k-1} \\frac{e^{-x/\\theta}}{\\Gamma(k)\\,\\theta^k}
        \\ \\ \\ \\ \\mathrm{for\\ } x > 0
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "afda", status = "primary", solidHeader = TRUE,
      radioButtons(paste("gamma", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("gamma", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("gamma", "shape", sep="."), "Shape \\(k\\)",
        min = 0, max = 20, value = 1, step= 0.1),
      sliderInput(paste("gamma", "scale", sep="."), "Scale \\(\\theta\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("gammaPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("gamma.meanBox", width = 6),
    valueBoxOutput("gamma.varianceBox", width = 6)
  )
)

board.cauchy <- tabItem(tabName = "cauchy",
  fluidRow(
    distPanel("cauchy"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        \\begin{align}
        f(x; x_0,\\gamma) &=
        { 1 \\over \\pi } \\left[ { \\gamma \\over (x - x_0)^2 + \\gamma^2  } \\right]
        \\end{align}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "cauchy", status = "primary", solidHeader = TRUE,
      radioButtons(paste("cauchy", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("cauchy", "range", sep="."), "Range",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("cauchy", "location", sep="."), "Location \\(x_0\\)",
        min = -20, max = 20, value = 0, step= 0.1),
      sliderInput(paste("cauchy", "scale", sep="."), "Scale \\(\\gamma\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("cauchyPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("cauchy.meanBox", width = 6),
    valueBoxOutput("cauchy.varianceBox", width = 6)
  )
)

board.exp <- tabItem(tabName = "exp",
  fluidRow(
    distPanel("Exponential"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      f(x; \\lambda) = \\left\\{
        \\begin{array}{ll}
        \\lambda e^{-\\lambda x} & (x \\geq 0) \\\\ 0 & (x < 0)
        \\end{array}
        \\right.
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "exp", status = "primary", solidHeader = TRUE,
      radioButtons(paste("exp", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("exp","range",sep="."), "Range",
        min = -10, max = 50, value = c(0, 5), step= 1),
      sliderInput(paste("exp","rate",sep="."), "\\(\\lambda\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("expPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("exp.meanBox", width = 6),
    valueBoxOutput("exp.varianceBox", width = 6)
  )
)

board.lnormal <- tabItem(tabName = "lnormal",
  fluidRow(
    distPanel("Log normal"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      f(x) = \\frac{1}{\\sqrt{2\\pi} \\sigma x} e^{-\\frac{ (\\ln{x}-\\mu)^2}{2\\sigma^2} },
      \\quad 0<x< \\infty
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "lognormal", status = "primary", solidHeader = TRUE,
      radioButtons(paste("lnormal", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("lnormal", "range", sep="."), "Range",
        min = 0, max = 200, value = c(0, 20), step= 0.5),
      sliderInput(paste("lnormal", "meanlog", sep="."), "Expectation log",
        min = -30, max = 30, value = 0, step= 0.05),
      sliderInput(paste("lnormal", "sdlog", sep="."), "SD log",
        min = 0, max = 10, value = 1, step= 0.05)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("lnormalPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("lnormal.meanBox", width = 6),
    valueBoxOutput("lnormal.varianceBox", width = 6)
  )
)

board.t <- tabItem(tabName = "t",
  fluidRow(
    distPanel("t"),
    box(
      width = 7,
      status = "primary",
      title = "PDf", 
      helpText("$$
      f(x) = \\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi\\,}\\,
      \\Gamma(\\nu/2)} (1+x^2/\\nu)^{-(\\nu+1)/2}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "t", status = "primary", solidHeader = TRUE,
      radioButtons(paste("t", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("t", "range", sep="."), "Range",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("t", "df", sep="."), "DF \\(\\nu\\)",
        min = 1, max = 20, value = 1, step= 1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("tPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("t.meanBox", width = 6),
    valueBoxOutput("t.varianceBox", width = 6)
  )
)

board.nct <- tabItem(tabName = "nct",
  fluidRow(
    distPanel("NC T"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      f(x) =\\frac{\\nu^{\\frac{\\nu}{2}}
      \\exp\\left (-\\frac{\\nu\\mu^2}{2(x^2+\\nu)} \\right )}
      {\\sqrt{\\pi}\\Gamma(\\frac{\\nu}{2})2^{\\frac{\\nu-1}{2}}(x^2+\\nu)^{\\frac{\\nu+1}{2}}}
      \\int_0^\\infty y^\\nu\\exp\\left (-\\frac{1}{2}\\left(y-\\frac{\\mu x}{\\sqrt{x^2+\\nu}}
      \\right)^2\\right ) dy
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "NC t", status = "primary", solidHeader = TRUE,
      radioButtons(paste("nct", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("nct", "range", sep="."), "Range",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("nct", "df", sep="."), "DF \\(\\nu\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("nct", "ncp", sep="."), "NC degree \\(\\mu\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("nctPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("nct.meanBox", width = 6),
    valueBoxOutput("nct.varianceBox", width = 6)
  )
)

board.beta <- tabItem(tabName = "beta",
  fluidRow(
    distPanel("beta"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      f(x)=\\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "beta", status = "primary", solidHeader = TRUE,
      radioButtons(paste("beta", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("beta", "range", sep="."), "Range",
        min = 0, max = 1, value = c(0, 1), step= 0.01),
      sliderInput(paste("beta", "shape1", sep="."), "Shape \\(\\alpha\\)",
        min = 0, max = 20, value = 2, step= 0.1),
      sliderInput(paste("beta", "shape2", sep="."), "Shape \\(\\beta\\)",
        min = 0, max = 20, value = 2, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("betaPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("beta.meanBox", width = 6),
    valueBoxOutput("beta.varianceBox", width = 6)
  )
)

board.ncbeta <- tabItem(tabName = "ncbeta",
  fluidRow(
    distPanel("Nc beta", 'Noncentral_beta_distribution'),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x) = \\sum_{j=0}^\\infty \\frac{1}{j!}
        \\left(\\frac{\\lambda}{2}\\right)^je^{-\\lambda/2}
        \\frac{x^{\\alpha+j-1}(1-x)^{\\beta-1}}{B(\\alpha+j,\\beta)}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "nc beta", status = "primary", solidHeader = TRUE,
      radioButtons(paste("ncbeta", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("ncbeta", "range", sep="."), "Range",
        min = 0, max = 1, value = c(0, 1), step= 0.01),
      sliderInput(paste("ncbeta", "shape1", sep="."), "Shape \\(\\alpha\\)",
        min = 0, max = 20, value = 2, step= 0.1),
      sliderInput(paste("ncbeta", "shape2", sep="."), "Shape \\(\\beta\\)",
        min = 0, max = 20, value = 2, step= 0.1),
      sliderInput(paste("ncbeta", "ncp", sep="."), "NC Degree \\(\\lambda\\)",
        min = 0, max = 20, value = 0, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("ncbetaPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("ncbeta.meanBox", width = 12),
    valueBoxOutput("ncbeta.varianceBox", width = 12)
  )
)

board.unif <- tabItem(tabName = "unif",
  fluidRow(
    distPanel("Uniform"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x)=\\begin{cases}
        \\frac{1}{b - a} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
        0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
        \\end{cases} 
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "unif", status = "primary", solidHeader = TRUE,
      radioButtons(paste("unif", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("unif", "range", sep="."), "Range",
        min = -50, max = 50, value = c(0, 1), step= 0.5)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("unifPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("unif.meanBox", width = 6),
    valueBoxOutput("unif.varianceBox", width = 6)
  )
)

board.logis <- tabItem(tabName = "logis",
  fluidRow(
    distPanel("logis"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(x;\\mu,s) = \\frac{\\exp(-\\frac{x-\\mu}{s})}{s(1+\\exp(-\\frac{x-\\mu}{s}))^2}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "logis", status = "primary", solidHeader = TRUE,
      radioButtons(paste("logis", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("logis", "range", sep="."), "Range",
        min = -100, max = 100, value = c(-10, 10), step= 0.5),
      sliderInput(paste("logis", "location", sep="."), "Location \\(\\mu\\)",
        min = -20, max = 20, value = 2, step= 0.1),
      sliderInput(paste("logis", "scale", sep="."), "Shape \\(s\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("logisPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("logis.meanBox", width = 6),
    valueBoxOutput("logis.varianceBox", width = 6)
  )
)

board.weibull <- tabItem(tabName = "weibull",
  fluidRow(
    distPanel("Weibull"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        f(t)=\\frac{m}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{m-1}
        \\exp \\left\\{-\\left(\\frac{t}{\\eta}\\right)^m\\right\\}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "weibull", status = "primary", solidHeader = TRUE,
      radioButtons(paste("weibull", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("weibull", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 0.5),
      sliderInput(paste("weibull", "shape", sep="."), "Shape \\(m\\)",
        min = 0, max = 20, value = 1, step= 0.1),
      sliderInput(paste("weibull", "scale", sep="."), "Scale \\(\\eta\\)",
        min = 0, max = 20, value = 1, step= 0.1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("weibullPlot", 'line')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("weibull.meanBox", width = 12),
    valueBoxOutput("weibull.varianceBox", width = 12)
  )
)

#  
board.geom <- tabItem(tabName = "geom",
  fluidRow(
    distPanel("Geom"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
        Pr(X = k) = p(1-p)^{k}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "geom", status = "primary", solidHeader = TRUE,
      radioButtons(paste("geom", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("geom", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("geom", "prob", sep="."), "Probability \\(p\\)",
        min = 0, max = 1, value = 0.5, step= 0.01)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("geomPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("geom.meanBox", width = 6),
    valueBoxOutput("geom.varianceBox", width = 6)
  )
)

board.hyper <- tabItem(tabName = "hyper",
  fluidRow(
    distPanel("HG"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      \\operatorname{P}(X=x)
      = \\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "HG", status = "primary", solidHeader = TRUE,
      radioButtons(paste("hyper", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("hyper", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("hyper", "m", sep="."), "Numbers of Successes \\(m\\)",
        min = 0, max = 100, value = 50, step= 1),
      sliderInput(paste("hyper", "n", sep="."), "Numbers of Failers \\(n\\)",
        min = 0, max = 100, value = 50, step= 1),
      sliderInput(paste("hyper", "k", sep="."), "Numbers of Trials \\(k\\)",
        min = 0, max = 100, value = 10, step= 1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("hyperPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("hyper.meanBox", width = 6),
    valueBoxOutput("hyper.varianceBox", width = 6)
  )
)

board.binom <- tabItem(tabName = "binom",
  fluidRow(
    distPanel("Binomial"),
    box(
      width = 7,
      status = "primary",
      title = "PDF", 
      helpText("$$
      P[X=k]={n\\choose k}p^k(1-p)^{n-k}\\quad\\mbox{for}\\ k=0,1,2,\\dots,n
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "Binomial", status = "primary", solidHeader = TRUE,
      radioButtons(paste("binom", "p_or_c", sep="."), "",
        c("PDF"="p", "CDF"="c")
      ),
      sliderInput(paste("binom", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("binom", "size", sep="."), "Numbers of Trials \\(n\\)",
        min = 0, max = 40, value = 10, step= 1),
      sliderInput(paste("binom", "prob", sep="."), "Probability of Success \\(p\\)",
        min = 0, max = 1, value = 0.5, step= 0.01)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("binomPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("binom.meanBox", width = 6),
    valueBoxOutput("binom.varianceBox", width = 6)
  )
)

board.nbinom <- tabItem(tabName = "nbinom",
  fluidRow(
    distPanel("Negative Binomial"),
    box(
      width = 7,
      status = "primary",
      title = "PMF", 
      helpText("$$
        f(x)=P(X=x) = {x-1 \\choose r-1} p^r (1-p)^{x-r}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "NB", status = "primary", solidHeader = TRUE,
      radioButtons(paste("nbinom", "p_or_c", sep="."), "",
        c("PMF"="p", "CDF"="c")
      ),
      sliderInput(paste("nbinom", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("nbinom", "size", sep="."), "Numbers of Successes \\(r\\)",
        min = 1, max = 20, value = 1, step= 1),
      sliderInput(paste("nbinom", "prob", sep="."), "Probability of Successes  \\(p\\)",
        min = 0, max = 1, value = 0.5, step= 0.01)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("nbinomPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("nbinom.meanBox", width = 6),
    valueBoxOutput("nbinom.varianceBox", width = 6)
  )
)

board.pois <- tabItem(tabName = "pois",
  fluidRow(
    distPanel("Poisson"),
    box(
      width = 7,
      status = "primary",
      title = "PMF", 
      helpText("$$
      P(X=k)=\\frac{\\lambda^k e^{-\\lambda}}{k!}
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "poi", status = "primary", solidHeader = TRUE,
      radioButtons(paste("pois", "p_or_c", sep="."), "",
        c("PMF"="p", "CDF"="c")
      ),
      sliderInput(paste("pois", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 1),
      sliderInput(paste("pois", "lambda", sep="."), "\\(\\lambda\\)",
        min = 1, max = 20, value = 1, step= 0.5)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("poisPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("pois.meanBox", width = 6),
    valueBoxOutput("pois.varianceBox", width = 6)
  )
)

board.dunif <- tabItem(tabName = "dunif",
  fluidRow(
    distPanel("dunif"),
    box(
      width = 7,
      status = "primary",
      title = "PMF", 
      helpText("$$
      f(x)=\\begin{cases}
      \\frac{1}{n} & \\mathrm{for}\\ a \\le x \\le b, \\\\[8pt]
      0 & \\mathrm{for}\\ x<a\\ \\mathrm{or}\\ x>b
      \\end{cases} 
      $$")
    )
  ),
  fluidRow(
    box(
      width = 5,
      withMathJax(),
      title = "dunif", status = "primary", solidHeader = TRUE,
      radioButtons(paste("dunif", "p_or_c", sep="."), "",
        c("PMF"="p", "CDF"="c")
      ),
      sliderInput(paste("dunif", "range", sep="."), "Range",
        min = 0, max = 100, value = c(0, 20), step= 1)
    ),
    box(
      width = 7,
      title = "Plot", status = "primary", solidHeader = TRUE,
      nvd3ChartOutput("dunifPlot", 'scatter')
    )
  ),
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("dunif.meanBox", width = 6),
    valueBoxOutput("dunif.varianceBox", width = 6)
  )
)




body <- dashboardBody(
  jsFiles,
  tabItems(
    #  
    board.norm,
    board.erlang,
    board.f,
    board.ncf,
    board.chisq,
    board.ncChisq,
    board.gamma,
    board.cauchy,
    board.exp,
    board.lnormal,
    board.t,
    board.nct,
    board.beta,
    board.ncbeta,
    board.unif,
    board.logis,
    board.weibull,
    # 離散分布
    board.geom,
    board.hyper,
    board.binom,
    board.nbinom,
    board.pois,
    board.dunif,
    # About
    board.about
  )
)

ui <- dashboardPage(header, sidebar, body)

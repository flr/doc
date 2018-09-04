## ---- pkgs, echo=FALSE, message=FALSE, warning=FALSE---------------------
library(Rcpp)
library(FLasher)
library(ggplotFL)
library(knitr)
opts_chunk$set(dev='png', cache=FALSE, fig.width=4, fig.height=4, tidy=TRUE, dpi=72)
options(width=60)

## ------------------------------------------------------------------------
cppFunction('
int my_add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

## ------------------------------------------------------------------------
my_add(1L, 2L, 10L)

## ---- warning=FALSE------------------------------------------------------
cppFunction('
std::vector<int> rbl_demo(std::vector<int> v){
    for (auto& i : v){
        i++;
    }
    return v;
    }',
    plugins="cpp11"
)

## ------------------------------------------------------------------------
rbl_demo(c(1L,2L,3L))

## ------------------------------------------------------------------------
source_code <- " #include <Rcpp.h>
    // This function is not exposed to R
    double foo(double x){
        return 2.0 * x;
    }

    // This function is exposed to R and calls the unexposed one
    // [[Rcpp::export]]
    double call_foo(double x){
        double y = foo(x);
        return y;
    }
"
cat(source_code, file=paste(tempdir(),"test-1.cpp", sep="/"))
sourceCpp(file=paste(tempdir(),"test-1.cpp", sep="/"))

## ------------------------------------------------------------------------
call_foo(3.5)

## ---- warning=FALSE------------------------------------------------------
source_code <- " #include <Rcpp.h>
    // [[Rcpp::plugins(cpp11)]]     

    // [[Rcpp::export]]
    std::vector<double> rbl_demo2(std::vector<double> v){
        for (auto& i : v){
            i = i * 2.0;
        }
        return v;
    }
"
cat(source_code, file=paste(tempdir(),"test-2.cpp", sep="/"))
sourceCpp(file=paste(tempdir(),"test-2.cpp", sep="/"))

## ------------------------------------------------------------------------
rbl_demo2(c(1.3, 2.6, 3.9))

## ------------------------------------------------------------------------
cppFunction('
FLQuant calc_catches(FLQuant landings, FLQuant discards){
    FLQuant catches = landings + discards;
    return catches;
    }',
    depends="FLasher"
)

## ------------------------------------------------------------------------
data(ple4)
landings <- landings.n(ple4)[,ac(2000:2003)]
discards <- discards.n(ple4)[,ac(2000:2003)]

## ------------------------------------------------------------------------
calc_catches(landings, discards)

## ------------------------------------------------------------------------
source_code <- "
    // [[Rcpp::depends(FLasher)]] \n #include <FLasher.h>

    // [[Rcpp::export]]
    FLQuant calc_catches2(FLQuant landings, FLQuant discards){
        FLQuant catches = landings + discards;
        return catches;
    }
"
cat(source_code, file=paste(tempdir(),"test-3.cpp", sep="/"))
sourceCpp(file=paste(tempdir(),"test-3.cpp", sep="/"))

## ---- "demo"-------------------------------------------------------------
calc_catches2(landings, discards)

## ---- adexample----------------------------------------------------------
source_code <- "
    // [[Rcpp::depends(FLasher)]] \n #include <FLasher.h>

    // This is the function we want to solve - the banana function
    // It is templated because we need versions of it that deal with
    // types double (for normal evaluation) and adouble (for AD evaluation) 
    template <typename T>
    std::vector<T> func(std::vector<T> params){
        std::vector<T> res(1, 0.0);
        res[0] = 100 * pow((params[1] - params[0] * params[0]), 2.0) + pow((1 - params[0]), 2.0);
        return res;
    }

    // Evaluates the function
    // [[Rcpp::export]]
    std::vector<double> eval_function(std::vector<double> params){
        return func(params);
    }

    // Uses CppAD magic to get the gradient of the function
    // [[Rcpp::export]]
    std::vector<double> eval_gradient(std::vector<double> params){
        std::vector<adouble> x(params.begin(), params.end());
        CppAD::Independent(x);
        std::vector<adouble> res = func(x);
        CppAD::ADFun<double> fun(x, res);
        return fun.Jacobian(params);
    }

    // Uses CppAD magic to get the Hessian
    // [[Rcpp::export]]
    std::vector<double> eval_hessian(std::vector<double> params, unsigned int var = 0){
        std::vector<adouble> x(params.begin(), params.end());
        CppAD::Independent(x);
        std::vector<adouble> res = func(x);
        CppAD::ADFun<double> fun(x, res);
        return fun.Hessian(params, var);
    }
"

cat(source_code, file=paste(tempdir(),"test-4.cpp", sep="/"))
sourceCpp(file=paste(tempdir(),"test-4.cpp", sep="/"))

## ------------------------------------------------------------------------
# Rosenbrock Banana function
fr <- function(x) {   
    100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2
}
# The exact gradient of the banana function
grr <- function(x) { ## Gradient of 'fr'
    x1 <- x[1]
    x2 <- x[2]
    c(-400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
        200 *      (x[2] - x[1] * x[1]))
}

## ------------------------------------------------------------------------
res1 <- optim(c(-1.2,1), fr, method = "BFGS")
res1[c("par", "value", "counts")]

## ------------------------------------------------------------------------
res2 <- optim(c(-1.2,1), fr, grr, method = "BFGS")
res2[c("par", "value", "counts")]

## ------------------------------------------------------------------------
res3 <- optim(c(-1.2,1), eval_function, eval_gradient, method = "BFGS")
res3[c("par", "value", "counts")]

## ------------------------------------------------------------------------
# Estimated by R
optimHess(res1$par, fr)
# Estimated by R using the the gradient function
optimHess(res2$par, fr, grr)
# Calculated using the AD function
eval_hessian(res3$par)


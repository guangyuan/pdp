// GBM by Greg Ridgeway  Copyright (C) 2003

#include "treeparams.h"
#include <memory>
#include <utility>
#include <Rcpp.h>

namespace {
class NodeStack {
public:
  bool empty() const { return stack.empty(); }

  std::pair<int, double> pop() {
    std::pair<int, double> result = stack.back();
    stack.pop_back();
    return result;
  }

  void push(int nodeIndex, double weight = 0.0) {
    stack.push_back(std::make_pair(nodeIndex, weight));
  }

private:
  std::vector<std::pair<int, double> > stack;
};

}

//----------------------------------------
// Functions- Public
//----------------------------------------
extern "C" {

  //-----------------------------------
  // Function: partial_gbm
  //
  // Returns: predicted function - a Rcpp::NumericVector
  //
  // Description: Calculates predictions using only
  //				effects of specific variables.
  //
  // Parameters:
  //  covariates - SEXP containing the predictor values - becomes
  //				  const Rcpp::NumericMatrix.
  //  whichvar- SEXP containing indices specifying which var columns of the
  //			  covariates are important - const Rcpp::IntegerVector.
  //  num_trees - SEXP containing an int or vector of ints specifying the number
  //  of
  //				  trees to make predictions on - stored as const
  // Rcpp::IntegerVector.
  //				  trees to make predictions on - stored as const
  // Rcpp::IntegerVector.
  //  init_func_est - SEXP specifying initial prediction estimate - double.
  //  fitted_trees - SEXP containing lists defining the previously fitted trees -
  //					stored as const Rcpp::GenericVector.
  //  categorical_splits - SEXP containing  list of the categories of the split
  //  variables
  //						defining a tree - stored as a
  // const
  // Rcpp::GenericVector.
  //  var_types -  SEXP containing integers specifying whether the variables of
  //  interest are
  //				continuous/nominal- stored as const
  // Rcpp::IntegerVector.
  //-----------------------------------

  SEXP gbm_plot(
      SEXP covariates,          // vector or matrix of points to make predictions
      SEXP whichvar,            // index of which var cols of X are
      SEXP num_trees,           // number of trees to use
      SEXP init_func_est,       // initial value
      SEXP fitted_trees,        // tree list object
      SEXP categorical_splits,  // categorical split list object
      SEXP var_types            // vector of variable types
  ) {
    BEGIN_RCPP
    int tree_num = 0;
    int obs_num = 0;
    int class_num = 0;
    const Rcpp::NumericMatrix kCovarMat(covariates);
    const int kNumRows = kCovarMat.nrow();
    const int kNumTrees = Rcpp::as<int>(num_trees);
    const Rcpp::IntegerVector kWhichVars(whichvar);
    const Rcpp::GenericVector kFittedTrees(fitted_trees);
    const Rcpp::GenericVector kSplits(categorical_splits);
    const Rcpp::IntegerVector kVarType(var_types);

    Rcpp::NumericVector predicted_func(kNumRows, Rcpp::as<double>(init_func_est));

    if (kCovarMat.ncol() != kWhichVars.size()) {
      throw gbm_exception::InvalidArgument("shape mismatch");
    }

    for (tree_num = 0; tree_num < kNumTrees; tree_num++) {
      const Rcpp::GenericVector kThisTree = kFittedTrees[tree_num];
      const Rcpp::IntegerVector kThisSplitVar = kThisTree[0];
      const Rcpp::NumericVector kThisSplitCode = kThisTree[1];
      const Rcpp::IntegerVector kThisLeftNode = kThisTree[2];
      const Rcpp::IntegerVector kThisRightNode = kThisTree[3];
      const Rcpp::IntegerVector kThisMissingNode = kThisTree[4];
      const Rcpp::NumericVector kThisWeight = kThisTree[6];
      for (obs_num = 0; obs_num < kNumRows; obs_num++) {
        NodeStack stack;
        stack.push(0, 1.0);
        while (!stack.empty()) {
          const std::pair<int, double> top = stack.pop();
          int iCurrentNode = top.first;
          const double kWeight = top.second;

          if (kThisSplitVar[iCurrentNode] == -1)  // terminal node
          {
            predicted_func[class_num * kNumRows + obs_num] +=
              kWeight * kThisSplitCode[iCurrentNode];
          } else  // non-terminal node
          {
            // is this a split variable that interests me?
            const Rcpp::IntegerVector::const_iterator found =
              std::find(kWhichVars.begin(), kWhichVars.end(),
                        kThisSplitVar[iCurrentNode]);

            if (found != kWhichVars.end()) {
              const int kPredVar = found - kWhichVars.begin();
              const double kXValue = kCovarMat(obs_num, kPredVar);
              // missing?
              if (ISNA(kXValue)) {
                stack.push(kThisMissingNode[iCurrentNode], kWeight);
              }
              // continuous?
              else if (kVarType[kThisSplitVar[iCurrentNode]] == 0) {
                if (kXValue < kThisSplitCode[iCurrentNode]) {
                  stack.push(kThisLeftNode[iCurrentNode], kWeight);
                } else {
                  stack.push(kThisRightNode[iCurrentNode], kWeight);
                }
              } else  // categorical
              {
                const Rcpp::IntegerVector kCatSplits =
                  kSplits[kThisSplitCode[iCurrentNode]];

                const int kCatSplitIndicator = kCatSplits[kXValue];
                if (kCatSplitIndicator == -1) {
                  stack.push(kThisLeftNode[iCurrentNode], kWeight);
                } else if (kCatSplitIndicator == 1) {
                  stack.push(kThisRightNode[iCurrentNode], kWeight);
                } else  // handle unused level
                {
                  stack.push(kThisMissingNode[iCurrentNode], kWeight);
                }
              }
            }     // iPredVar != -1
            else  // not interested in this split, average left and right
            {
              const int kRight = kThisRightNode[iCurrentNode];
              const int kLeft = kThisLeftNode[iCurrentNode];
              const double kRightWeight = kThisWeight[kRight];
              const double kLeftWeight = kThisWeight[kLeft];
              stack.push(kRight,
                         kWeight * kRightWeight / (kRightWeight + kLeftWeight));
              stack.push(kLeft,
                         kWeight * kLeftWeight / (kRightWeight + kLeftWeight));
            }
          }  // non-terminal node
        }    // while(cStackNodes > 0)
      }      // iObs
    }        // iTree

    return Rcpp::wrap(predicted_func);
    END_RCPP
  }  // gbm_plot

}  // end extern "C"

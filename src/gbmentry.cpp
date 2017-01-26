// GBM by Greg Ridgeway  Copyright (C) 2003

#include "buildinfo.h"

extern "C" {

#include <R.h>
#include <Rinternals.h>

SEXP gbm_plot
(
    SEXP radX,          // vector or matrix of points to make predictions
    SEXP rcRows,        // number of rows in X
    SEXP rcCols,        // number of columns in X
    SEXP rcNumClasses,  // number of classes
    SEXP raiWhichVar,   // length=cCols, index of which var cols of X are
    SEXP rcTrees,       // number of trees to use
    SEXP rdInitF,       // initial value
    SEXP rTrees,        // tree list object
    SEXP rCSplits,      // categorical split list object
    SEXP raiVarType     // vector of variable types
)
{
    unsigned long hr = 0;
    int i = 0;
    int iTree = 0;
    int iObs = 0;
    int iClass = 0;
    int cRows = INTEGER(rcRows)[0];
    int cCols = INTEGER(rcCols)[0];
    int cTrees = INTEGER(rcTrees)[0];
    int cNumClasses = INTEGER(rcNumClasses)[0];

    SEXP rThisTree = NULL;
    int *aiSplitVar = NULL;
    double *adSplitCode = NULL;
    int *aiLeftNode = NULL;
    int *aiRightNode = NULL;
    int *aiMissingNode = NULL;
    double *adW = NULL;
    int iCurrentNode = 0;
    double dCurrentW = 0.0;
    double dX = 0.0;
    int iCatSplitIndicator = 0;

    SEXP radPredF = NULL;
    int aiNodeStack[40];
    double adWeightStack[40];
    int cStackNodes = 0;
    int iPredVar = 0;

    // allocate the predictions to return
    PROTECT(radPredF = allocVector(REALSXP, cRows*cNumClasses));
    if(radPredF == NULL)
    {
        hr = GBM_OUTOFMEMORY;
        goto Error;
    }
    for(iObs=0; iObs<cRows*cNumClasses; iObs++)
    {
        REAL(radPredF)[iObs] = REAL(rdInitF)[0];
    }
    for(iTree=0; iTree<cTrees; iTree++)
    {
        for (iClass = 0; iClass < cNumClasses; iClass++)
        {
            rThisTree     = VECTOR_ELT(rTrees,iClass + iTree*cNumClasses);
            aiSplitVar    = INTEGER(VECTOR_ELT(rThisTree,0));
            adSplitCode   = REAL   (VECTOR_ELT(rThisTree,1));
            aiLeftNode    = INTEGER(VECTOR_ELT(rThisTree,2));
            aiRightNode   = INTEGER(VECTOR_ELT(rThisTree,3));
            aiMissingNode = INTEGER(VECTOR_ELT(rThisTree,4));
            adW           = REAL   (VECTOR_ELT(rThisTree,6));
            for(iObs=0; iObs<cRows; iObs++)
            {
                aiNodeStack[0] = 0;
                adWeightStack[0] = 1.0;
                cStackNodes = 1;
                while(cStackNodes > 0)
                {
                    cStackNodes--;
                    iCurrentNode = aiNodeStack[cStackNodes];

                    if(aiSplitVar[iCurrentNode] == -1) // terminal node
                    {
                        REAL(radPredF)[iClass*cRows + iObs] +=
                            adWeightStack[cStackNodes]*adSplitCode[iCurrentNode];
                    }
                    else // non-terminal node
                    {
                        // is this a split variable that interests me?
                        iPredVar = -1;
                        for(i=0; (iPredVar == -1) && (i < cCols); i++)
                        {
                            if(INTEGER(raiWhichVar)[i] == aiSplitVar[iCurrentNode])
                            {
                                iPredVar = i; // split is on one that interests me
                            }
                        }

                        if(iPredVar != -1) // this split is among raiWhichVar
                        {
                            dX = REAL(radX)[iPredVar*cRows + iObs];
                            // missing?
                            if(ISNA(dX))
                            {
                                aiNodeStack[cStackNodes] = aiMissingNode[iCurrentNode];
                                cStackNodes++;
                            }
                            // continuous?
                            else if(INTEGER(raiVarType)[aiSplitVar[iCurrentNode]] == 0)
                                {
                                if(dX < adSplitCode[iCurrentNode])
                                {
                                    aiNodeStack[cStackNodes] = aiLeftNode[iCurrentNode];
                                        cStackNodes++;
                                }
                                else
                                {
                                    aiNodeStack[cStackNodes] = aiRightNode[iCurrentNode];
                                    cStackNodes++;
                                }
                            }
                            else // categorical
                            {
                                iCatSplitIndicator = INTEGER(
                                    VECTOR_ELT(rCSplits,
                                               (int)adSplitCode[iCurrentNode]))[(int)dX];
                                if(iCatSplitIndicator==-1)
                                {
                                    aiNodeStack[cStackNodes] = aiLeftNode[iCurrentNode];
                                    cStackNodes++;
                                }
                                else if(iCatSplitIndicator==1)
                                {
                                    aiNodeStack[cStackNodes] = aiRightNode[iCurrentNode];
                                    cStackNodes++;
                                }
                                else // handle unused level
                                {
                                    iCurrentNode = aiMissingNode[iCurrentNode];
                                }
                            }
                        } // iPredVar != -1
                        else // not interested in this split, average left and right
                        {
                            aiNodeStack[cStackNodes] = aiRightNode[iCurrentNode];
                            dCurrentW = adWeightStack[cStackNodes];
                            adWeightStack[cStackNodes] = dCurrentW *
                                adW[aiRightNode[iCurrentNode]]/
                                (adW[aiLeftNode[iCurrentNode]]+
                                 adW[aiRightNode[iCurrentNode]]);
                            cStackNodes++;
                            aiNodeStack[cStackNodes] = aiLeftNode[iCurrentNode];
                            adWeightStack[cStackNodes] =
                                    dCurrentW-adWeightStack[cStackNodes-1];
                            cStackNodes++;
                        }
                    } // non-terminal node
                } // while(cStackNodes > 0)
            } // iObs
        } // iClass
    } // iTree

Cleanup:
    UNPROTECT(1); // radPredF
    return radPredF;
Error:
    goto Cleanup;
} // gbm_plot

} // end extern "C"

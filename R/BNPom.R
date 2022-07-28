
##'Class \code{"PomNode"} as a \code{"Pnode"}
##'
##'
##'The \code{PPomegranite} package supplies the needed methods so that the
##'\code{RNetica::\linkS4class{PomNode}} object is an instance of the
##'\code{Peanut::\linkS4class{Pnode}} object.  As a Pnode is nominally
##'parameterized, the are given the special label \dQuote{pnode} to indicate
##'that this note has parametric information.
##'
##'
##'@name Pnode.PomNode
##'@aliases Pnode.PomNode PnodeNet,PomNode-method
##'PnodeName,PomNode-method PnodeName<-,PomNode-method
##'PnodeTitle,PomNode-method PnodeTitle<-,PomNode-method
##'PnodeDescription,PomNode-method PnodeDescription<-,PomNode-method
##'PnodeLabels,PomNode-method PnodeLabels<-,PomNode-method
##'PnodeProbs,PomNode-method PnodeProbs<-,PomNode-method
##'PnodeNumParents,PomNode-method PnodeParentNames,PomNode-method
##'PnodeParents,PomNode-method PnodeParents<-,PomNode-method
##'isPnodeContinuous,PomNode-method
##'@docType class
##'@note
##'
##'The \dQuote{Pnode properies}, \code{lnAlphas}, \code{betas}, \code{Q},
##'\code{rules}, \code{link}, \code{linkScale}, and \code{priorWeight} are
##'stored in user fields (\code{\link{NodeUserObj}}) of the Netica node.  A
##'\code{\linkS4class{PomNode}} object which has those fields behaves as a
##'\code{\linkS4class{Pnode}} and is suitable for the use with
##'\code{\link[Peanut:Peanut-package]{Peanut}}.  The function \code{Pnode} will
##'add default values for these fields if they are not set.
##'
##'To mark a node as a \code{Pnode}, it is added to the
##'\link[RNetica:NodeSets]{node set} \dQuote{pnode}.  The \code{is.Pnode}
##'function checks for this method.
##'@section Extends:
##'
##'See \code{\linkS4class{PomNode}} for a description of the Netica class.
##'
##'With these methods, \code{\linkS4class{PomNode}} now extends
##'\code{\linkS4class{Pnode}}.
##'
##'All reference classes extend and inherit methods from
##'\code{"\linkS4class{envRefClass}"}.
##'@author Russell Almond
##'@seealso
##'
##'Other methods of this class \code{\link{Pnode.States}},
##'\code{\link{Pnode.Properties}}.
##'
##'Base class: \code{\linkS4class{PomNode}}.
##'
##'Mixin class: \code{\linkS4class{Pnode}}.
##'
##'Generic functions from \code{Peanut} package:
##'
##'\code{\link[Peanut]{Pnode}}, \code{\link[Peanut]{PnodeNet}},
##'\code{\link[Peanut]{PnodeName}}, \code{\link[Peanut]{PnodeTitle}},
##'\code{\link[Peanut]{PnodeDescription}}, \code{\link[Peanut]{PnodeLabels}},
##'\code{\link[Peanut]{PnodeNumParents}},
##'\code{\link[Peanut]{PnodeParentNames}}, \code{\link[Peanut]{PnodeParents}},
##'\code{\link[Peanut]{PnodeProbs}}, \code{\link[Peanut]{as.Pnode}},
##'\code{\link[Peanut]{is.Pnode}}, \code{\link[Peanut]{isPnodeContinuous}}.
##'@keywords classes graphs
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'nsnet <- CreateNetwork("NodeSetExample", session=sess)
##'Ability <- NewDiscreteNode(nsnet,"Ability",c("High","Med","Low"))
##'EssayScore <- NewDiscreteNode(nsnet,"EssayScore",paste("level",5:0,sep="_"))
##'Duration <- NewContinuousNode(nsnet,"Duration")
##'
##'## Pnode, is.Pnode, as.Pnode
##'stopifnot(!is.Pnode(EssayScore),!is.Pnode(Duration))
##'EssayScore <- Pnode(EssayScore)
##'Duration <- as.Pnode(Duration)
##'stopifnot(is.Pnode(EssayScore),is.Pnode(Duration))
##'
##'## PnodeNet
##'
##'stopifnot(PnodeNet(Ability)==nsnet)
##'
##'## PnodeName, PnodeTitle, PnodeDescription
##'PnodeTitle(Ability) <- "Student Ability"
##'PnodeDescription(Ability) <-
##'"Students who have more ability will have more success on the exam."
##'stopifnot(PnodeTitle(Ability) == "Student Ability",
##'PnodeDescription(Ability) ==
##'"Students who have more ability will have more success on the exam."
##')
##'
##'
##'## PnodeLabels
##'stopifnot(
##'  length(PnodeLabels(Ability)) == 0L ## Nothing set yet
##')
##'PnodeLabels(Ability) <- "ReportingVariable"
##'stopifnot(
##'  PnodeLabels(Ability) == "ReportingVariable"
##')
##'PnodeLabels(EssayScore) <- c("Observable",PnodeLabels(EssayScore))
##'stopifnot(
##'  !is.na(match("Observable",PnodeLabels(EssayScore)))
##')
##'## Make EssayScore a reporting variable, too
##'PnodeLabels(EssayScore) <- c("ReportingVariable",PnodeLabels(EssayScore))
##'stopifnot(
##'  setequal(PnodeLabels(EssayScore),c("Observable","ReportingVariable","pnodes"))
##')
##'
##'## Clear out the node set
##'PnodeLabels(Ability) <- character()
##'stopifnot(
##'  length(PnodeLabels(Ability)) == 0L
##')
##'
##'## PnodeNumParents, PnodeParents
##'
##'stopifnot(PnodeNumParents(Ability)==0L, PnodeParents(Ability)==list())
##'PnodeParents(EssayScore) <- list(Ability)
##'stopifnot(PnodeNumParents(EssayScore)==1L,
##'          PnodeParents(EssayScore)[[1]]==Ability,
##'          PnodeParentNames(EssayScore)=="Ability")
##'
##'DeleteNetwork(nsnet)
##'
##'## Node Probs
##'abc <- CreateNetwork("ABC", session=sess)
##'A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
##'B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
##'C <- NewDiscreteNode(abc,"C",c("C1","C2"))
##'
##'PnodeParents(A) <- list()
##'PnodeParents(B) <- list(A)
##'PnodeParents(C) <- list(A,B)
##'
##'PnodeProbs(A)<-c(.1,.2,.3,.4)
##'PnodeProbs(B) <- normalize(matrix(1:12,4,3))
##'PnodeProbs(C) <- normalize(array(1:24,c(A=4,B=3,C=2)))
##'
##'Aprobs <- PnodeProbs(A)
##'Bprobs <- PnodeProbs(B)
##'Cprobs <- PnodeProbs(C)
##'stopifnot(
##'  CPTtools::is.CPA(Aprobs),
##'  CPTtools::is.CPA(Bprobs),
##'  CPTtools::is.CPA(Cprobs)
##')
##'
##'DeleteNetwork(abc)
##'
##'
##'
##'stopSession(sess)
##'
##'
NULL



setMethod("PnodeName","PomNode", function (node) {
    stop("Function Not Implemented")
    nme <- NodeUserField(node,"Truename")
    if (is.null(nme) || is.na(nme)) {
        nme <- NodeName(node)
    }
    nme
})

setMethod("PnodeName<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  NodeUserField(node,"Truename") <- value
  NodeName(node) <- as.IDname(value)
  invisible(node)
})


setMethod("PnodeTitle","PomNode", function (node)
    stop("Function Not Implemented")
  NodeTitle(node))

setMethod("PnodeTitle<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  NodeTitle(node) <- value
  invisible(node)
})

setMethod("PnodeDescription","PomNode", function (node)
    stop("Function Not Implemented")
  NodeDescription(node))

setMethod("PnodeDescription<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  NodeDescription(node) <- value
  invisible(node)
})

setMethod("PnodeLabels","PomNode", function (node)
    stop("Function Not Implemented")
  NodeSets(node))

setMethod("PnodeLabels<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  NodeSets(node) <- value
  invisible(node)
})


### States

##'States of the \code{"PomNode"} as a \code{"Pnode"}
##'
##'
##'The \code{PPomegranite} package supplies the needed methods so that the
##'\code{RNetica::\linkS4class{PomNode}} object is an instance of the
##'\code{Peanut::\linkS4class{Pnode}} object.  As a Pnode is nominally
##'parameterized, the are given the special label \dQuote{pnode} to indicate
##'that this note has parametric information.  This page documents the methods
##'which access the states.
##'
##'
##'@name Pnode.States
##'@aliases Pnode.States PnodeNumStates,PomNode-method
##'PnodeStates,PomNode-method PnodeStates<-,PomNode-method
##'PnodeStateTitles,PomNode-method PnodeStateTitles<-,PomNode-method
##'PnodeStateDescriptions,PomNode-method
##'PnodeStateDescriptions<-,PomNode-method
##'PnodeStateValues,PomNode-method PnodeStateValues<-,PomNode-method
##'PnodeStateBounds,PomNode-method PnodeStateBounds<-,PomNode-method
##'@docType class
##'@note
##'
##'Netica overrides the \code{\link[RNetica]{NodeLevels}} to do different
##'things whether the node is continuous or discrete.  The functions
##'\code{\link[Peanut]{PnodeStateValues}} and
##'\code{\link[Peanut]{PnodeStateBounds}} attempt to untangle these two
##'different use cases.  In particular, \code{NodeLevels} for a continuous node
##'assumes that the range of the node is chopped into a number of contiguous
##'segments, and what is fed to the function is a list of cut points.  Thus, it
##'will encouter problems if the lower bound of one state does not match the
##'upper of the preious one.
##'@section Extends:
##'
##'See \code{\linkS4class{PomNode}} for a description of the Netica class.
##'
##'With these methods, \code{\linkS4class{PomNode}} now extends
##'\code{\linkS4class{Pnode}}.
##'
##'All reference classes extend and inherit methods from
##'\code{"\linkS4class{envRefClass}"}.
##'@author Russell Almond
##'@seealso
##'
##'Other methods of this class \code{\link{Pnode.PomNode}},
##'\code{\link{Pnode.Properties}}.
##'
##'Base class: \code{\linkS4class{PomNode}}.
##'
##'Mixin class: \code{\linkS4class{Pnode}}.
##'
##'Generic functions from \code{Peanut} package:
##'
##'\code{\link[Peanut]{PnodeNumStates}}, \code{\link[Peanut]{PnodeStates}},
##'\code{\link[Peanut]{PnodeStateTitles}},
##'\code{\link[Peanut]{PnodeStateDescriptions}},
##'\code{\link[Peanut]{PnodeStateValues}},
##'\code{\link[Peanut]{PnodeStateBounds}}.
##'@keywords classes graphs
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'curd <- getwd()
##'setwd(file.path(library(help="PPomegranite")$path, "testnets"))
##'
##'## Making states
##'anet <- CreateNetwork("Annette", session=sess)
##'
##'## Discrete Nodes
##'nodel2 <- NewDiscreteNode(anet,"TwoLevelNode")
##'stopifnot(
##'  length(PnodeStates(nodel2))==2,
##'  PnodeStates(nodel2)==c("Yes","No")
##')
##'
##'PnodeStates(nodel2) <- c("True","False")
##'stopifnot(
##'  PnodeNumStates(nodel2) == 2L,
##'  PnodeStates(nodel2)==c("True","False")
##')
##'
##'nodel3 <- NewDiscreteNode(anet,"ThreeLevelNode",c("High","Med","Low"))
##'stopifnot(
##'  PnodeNumStates(nodel3) == 3L,
##'  PnodeStates(nodel3)==c("High","Med","Low"),
##'  PnodeStates(nodel3)[2]=="Med"
##')
##'
##'PnodeStates(nodel3)[2] <- "Median"
##'stopifnot(
##'  PnodeStates(nodel3)[2]=="Median"
##')
##'
##'PnodeStates(nodel3)["Median"] <- "Medium"
##'stopifnot(
##'  PnodeStates(nodel3)[2]=="Medium"
##')
##'
##'
##'DeleteNetwork(anet)
##'
##'## State Metadata (Titles and Descriptions)
##'
##'cnet <- CreateNetwork("CreativeNet", session=sess)
##'
##'orig <- NewDiscreteNode(cnet,"Originality", c("H","M","L"))
##'PnodeStateTitles(orig) <- c("High","Medium","Low")
##'PnodeStateDescriptions(orig)[1] <- "Produces solutions unlike those typically seen."
##'
##'stopifnot(
##'  PnodeStateTitles(orig) == c("High","Medium","Low"),
##'  grep("solutions unlike", PnodeStateDescriptions(orig))==1,
##'  PnodeStateDescriptions(orig)[3]==""
##'  )
##'
##'sol <- NewDiscreteNode(cnet,"Solution",
##'       c("Typical","Unusual","VeryUnusual"))
##'stopifnot(
##'  all(PnodeStateTitles(sol) == ""),
##'  all(PnodeStateDescriptions(sol) == "")
##'  )
##'
##'PnodeStateTitles(sol)["VeryUnusual"] <- "Very Unusual"
##'PnodeStateDescriptions(sol) <- paste("Distance from typical solution",
##'                      c("<1", "1--2", ">2"))
##'stopifnot(
##'  PnodeStateTitles(sol)[3]=="Very Unusual",
##'  PnodeStateDescriptions(sol)[1] == "Distance from typical solution <1"
##'  )
##'
##'DeleteNetwork(cnet)
##'
##'## State Values
##'lnet <- CreateNetwork("LeveledNet", session=sess)
##'
##'vnode <- NewDiscreteNode(lnet,"volt_switch",c("Off","Reverse","Forwards"))
##'stopifnot(
##'  length(PnodeStateValues(vnode))==3,
##'  names(PnodeStateValues(vnode)) == PnodeStates(vnode),
##'  all(is.na(PnodeStateValues(vnode)))
##')
##'
##'## Don't run this until the levels for vnode have been set,
##'## it will generate an error.
##'try(PnodeStateValues(vnode)[2] <- 0)
##'
##'PnodeStateValues(vnode) <- 1:3
##'stopifnot(
##'  length(PnodeStateValues(vnode))==3,
##'  names(PnodeStateValues(vnode)) == PnodeStates(vnode),
##'  PnodeStateValues(vnode)[2]==2
##')
##'
##'PnodeStateValues(vnode)["Reverse"] <- -2
##'
##'## Continuous nodes get the state values from the bounds.
##'theta0 <- NewContinuousNode(lnet,"theta0")
##'stopifnot(length(PnodeStateValues(theta0))==0L)
##'norm5 <- 
##'   matrix(c(qnorm(c(.001,.2,.4,.6,.8)),
##'            qnorm(c(.2,.4,.6,.8,.999))),5,2,
##'          dimnames=list(c("VH","High","Mid","Low","VL"),
##'                        c("LowerBound","UpperBound")))
##'PnodeStateBounds(theta0) <- norm5
##'PnodeStateValues(theta0)  ## Note these are medians not mean wrt normal!
##'PnodeStateBounds(theta0)[1,1] <- -Inf
##'PnodeStateValues(theta0)  ## Infinite value!
##'
##'
##'DeleteNetwork(lnet)
##'
##'stopSession(sess)
##'setwd(curd)
##'
##'
NULL

setMethod("PnodeStates","PomNode", function (node)
    stop("Function Not Implemented")
  NodeStates(node))

setMethod("PnodeStates<-","PomNode", function (node,value) {
  ## Not 100% sure if this is safe, but want to simplify
  ## Interface.
    stop("Function Not Implemented")
  NodeStates(node,resize=TRUE) <- value
  invisible(node)
})

setMethod("PnodeNumStates","PomNode", function (node)
    stop("Function Not Implemented")
  NodeNumStates(node))

setMethod("PnodeStateTitles","PomNode", function (node) {
    stop("Function Not Implemented")
  NodeStateTitles(node)
})

setMethod("PnodeStateTitles<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  NodeStateTitles(node) <- value
  invisible(node)
})


setMethod("PnodeStateDescriptions","PomNode", function (node) {
    stop("Function Not Implemented")
  NodeStateComments(node)
})

setMethod("PnodeStateDescriptions<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  NodeStateComments(node) <- value
  invisible(node)
})

setMethod("PnodeStateValues","PomNode", function (node)
    stop("Function Not Implemented")
  if (is.continuous(node)) {
    apply(PnodeStateBounds(node),1,median)
  } else {
    NodeLevels(node)
  })

setMethod("PnodeStateValues<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  if (is.continuous(node))
    stop("This function only available for discrete nodes, but ",
         PnodeName(node), " is continuous. Use PnodeStateBounds instead.")
  NodeLevels(node) <- value
  invisible(node)
})

setMethod("PnodeStateBounds","PomNode", function (node)
    stop("Function Not Implemented")
  if (is.continuous(node)) {
    vals <- NodeLevels(node)
    k <- length(vals) -1L
    if (k < 1L) {
      bnds <- matrix(vals,0,2,
                     dimnames=list(character(),
                                   c("LowerBound","UpperBound")))
    } else {
      bnds <- matrix(c(vals[1L:k],vals[2L:(k+1L)]),k,2L,
                     dimnames=list(PnodeStates(node),
                                   c("LowerBound","UpperBound")))
    }
    bnds
  } else {
    stop("This function only available for continuous nodes, but ",
         PnodeName(node), " is discrete. Use PnodeStateValues instead.")
  })

setMethod("PnodeStateBounds<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  if (!is.continuous(node))
    stop("This function only available for continuous nodes, but ",
         PnodeName(node), " is discrete. Use PnodeStateValues instead.")
  k <- nrow(value)
  mon1 <- isMonotonic(value[,1L])
  direct <- attr(mon1,"direction")
  mon2 <- isMonotonic(value[,2L])
  if (!mon1 || !mon2 || direct!=attr(mon2,"direction")) {
    stop ("State bounds for node",PnodeName(node),"are not monotonic.")
  }
  if (direct>0) {                       #Increasing
    if (!all(abs(value[2L:k,1L]-value[1L:(k-1L),2L])<.002)) {
      stop("Upper and lower bounds don't match for node ",PnodeName(node))
    }
    bnds <-c(value[1L:k,1L],value[k,2L])
  } else {                              #Decreasing
    if (!all(abs(value[1L:(k-1L),1L]-value[2L:k,2L])<.002)) {
      stop("Upper and lower bounds don't match for node ",PnodeName(node))
    }
    bnds <-c(value[1L,2L],value[1L:k,1L])
  }
  NodeLevels(node) <- bnds
  if (!is.null(rownames(value)) &&
      (length(PnodeStates(node)!=k) || all(nchar(PnodeStates(node))==0L))) {
    PnodeStates(node) <- rownames(value)
  }
  invisible(node)
})

setMethod("isPnodeContinuous","PomNode", function (node)
  is.continuous(node))

#### Parents



setMethod("PnodeParents","PomNode", function (node)
    stop("Function Not Implemented")
  NodeParents(node)
)
setMethod("PnodeParents<-","PomNode", function (node,value) {
    stop("Function Not Implemented")
  if (is.null(value)) value <- list()
  NodeParents(node) <- value
  invisible(node)
})


### Parents

setMethod("PnodeParentNames","PomNode", function (node) {
    stop("Function Not Implemented")
  if (PnodeNumParents(node)==0) {
    character()
  } else {
    parents <- NodeParents(node)
    pnames <- sapply(parents,PnodeName)
    stubsp <- sapply(parents,function(nd) NodeKind(nd)=="Stub")
    if (any(stubsp))
      pnames[stubsp] <- names(parents)[stubsp]
    pnames
  }
})

setMethod("PnodeNumParents","PomNode", function (node)
    stop("Function Not Implemented")
  length(NodeParents(node)))


##'Gets or sets the value of a Pnode.
##'
##'
##'Adding evidence to a Bayesian network is done by setting the value of the
##'node to one of its states.  The generic function
##'\code{Peanut::\link[Peanut]{PnodeEvidence}} (and the method for a
##'\code{\linkS4class{PomNode}}) simply returns the to which it is set, or
##'\code{NA} if the node is not set.  There are a number of different ways of
##'setting the state depending on the type of the value argument (see Details).
##'
##'
##'The generic function \code{\link[Peanut]{PnodeEvidence}} is defined in the
##'\code{Peanut} package.  It returns either the name of a state (discrete
##'node), a numeric value (continuous node) or \code{NA} if the node has not
##'been set.
##'
##'There are different methods for different classes for the \code{value}
##'argument (the RHS of the assignment operator).
##'
##'\describe{
##'
##'\item{ANY}{If no other method is appropriate, does nothing and issues a
##'warning.}
##'
##'\item{NULL}{The value of the node is retracted
##'(\code{\link[RNetica]{RetractNodeFinding}}). }
##'
##'\item{character}{If the \code{value} is the name of a state, then the node
##'will be set to that state (\code{\link[RNetica]{NodeFinding}}).  Otherwise,
##'nothing will be done and a warning will be issued.}
##'
##'\item{factor}{The character value of the \code{value} is uses (see character
##'method).}
##'
##'\item{logical}{This method assumes that the node has exactly two states, and
##'that those states have values (\code{\link[Peanut]{PnodeStateValues}},
##'\code{\link[RNetica]{NodeLevels}}) 0 and 1.  These levels are used to
##'determine the mapping of TRUE and FALSE to states.  If node state values are
##'not set, then the character method is called using \dQuote{TRUE} or
##'\dQuote{FALSE} as the value.  }
##'
##'\item{numeric}{If the \code{value} is of length 1, then the value of the
##'node is set (\code{\link[RNetica]{NodeValue}}) to the argument.  If the
##'\code{value} is a vector of the same length as the number of states of the
##'node, then it is regarded as virtual evidence, and the likelihood is set
##'(\code{\link[RNetica]{NodeLikelihood}}). }
##'
##'\item{difftime}{Difftime \code{value}s are converted to real numbers in
##'seconds, then the node value is set (see numeric method).} }
##'
##'@aliases PnodeEvidence.PomNode PnodeEvidence,PomNode-method
##'PnodeEvidence<-,PomNode,ANY-method PnodeEvidence<-,PomNode,NULL-method
##'PnodeEvidence<-,PomNode,character-method
##'PnodeEvidence<-,PomNode,difftime-method
##'PnodeEvidence<-,PomNode,factor-method
##'PnodeEvidence<-,PomNode,logical-method
##'PnodeEvidence<-,PomNode,numeric-method
##'@param node A \code{\linkS4class{PomNode}} object whose value is to be
##'set.
##'@param value A value representing the new type of the argument.  See
##'details.
##'@return
##'
##'\code{PnodeEvidence}: For all node types, if the node is not set,
##'\code{PnodeEvidence} returns \code{NA}.
##'
##'If the node is continuous, its currently set value is returned as a numeric
##'scalar (\code{NA} if not set).
##'
##'If the node is discrete, usually a character value giving the current state
##'(or \code{NA}) is returned.  However, if the node was assigned a likelihood
##'instead of exact evidence, the likelihood vector is returned.
##'
##'\code{PnodeEvidence<-} returns the node argument invisibly.
##'@note
##'
##'For continuous nodes, \code{PnodeEvidence} is equivalent to
##'\code{\link[RNetica]{NodeValue}}.  For discrete nodes, it maps to either
##'\code{\link[RNetica]{NodeFinding}} or \code{\link[RNetica]{NodeLikelihood}}
##'@author Russell Almond
##'@seealso
##'
##'The function \code{\link{PnetCompile}} usually needs to be run before this
##'function has meaning.
##'
##'The functions \code{\link{PnodeStates}} and \code{\link{PnodeStateBounds}}
##'define the legal values for the value argument.
##'@keywords graphs
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'irt10.base <- ReadNetworks(paste(library(help="PPomegranite")$path,
##'                           "testnets","IRT10.2PL.base.dne",
##'                           sep=.Platform$file.sep),session=sess)
##'irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
##'irt10.theta <- PnetFindNode(irt10.base,"theta")
##'irt10.items <- PnetPnodes(irt10.base)
##'## Flag items as Pnodes
##'for (i in 1:length(irt10.items)) {
##'  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
##'  
##'}
##'
##'BuildAllTables(irt10.base)
##'PnetCompile(irt10.base) ## Netica requirement
##'
##'stopifnot (is.na(PnodeEvidence(irt10.items[[1]])))
##'
##'PnodeEvidence(irt10.items[[1]]) <- "Correct"
##'stopifnot(PnodeEvidence(irt10.items[[1]])=="Correct")
##'
##'PnodeEvidence(irt10.items[[1]]) <- NULL
##'stopifnot (is.na(PnodeEvidence(irt10.items[[1]])))
##'
##'PnodeEvidence(irt10.items[[1]]) <- c(Correct=.6,Incorrect=.3)
##'stopifnot(all.equal(PnodeEvidence(irt10.items[[1]]),
##'                    c(Correct=.6,Incorrect=.3),
##'                    tol=3*sqrt(.Machine$double.eps) ))
##'
##'foo <- NewContinuousNode(irt10.base,"foo")
##'
##'stopifnot(is.na(PnodeEvidence(foo)))
##'
##'PnodeEvidence(foo) <- 1
##'stopifnot(PnodeEvidence(foo)==1)
##'
##'DeleteNetwork(irt10.base)
##'stopSession(sess)
##'@exportMethod
setMethod("PnodeEvidence","PomNode",
          function(node) {
              stop("Function Not Implemented")
            if (is.continuous(node)) {
              res <- NodeValue(node)
            } else {
              res <- NodeFinding(node)
              if (res=="@NO FINDING") {
                res <- NA
              } else if (res=="@LIKELIHOOD") {
                res <- NodeLikelihood(node)
              } else if (res=="@NEGATIVE FINDINGS") {
                res <- NodeLikelihood(node)
              }
            }
            res
          })


setMethod("PnodeEvidence<-",c("PomNode","numeric"),
          function (node,value) {
              stop("Function Not Implemented")
            if (length(value) == 1L) {
              NodeValue(node) <- value
            } else if (length(value)==PnodeNumStates(node)) {
              NodeLikelihood(node) <- value
            }
            invisible(node)
          })

setOldClass("difftime")
setMethod("PnodeEvidence<-",c("PomNode","difftime"),
          function (node,value) {
              stop("Function Not Implemented")
            units(value) <- "secs"
            NodeValue(node) <- as.numeric(value)
          })
setMethod("PnodeEvidence<-",c("PomNode","character"),
          function (node,value) {
              stop("Function Not Implemented")
            ov1 <- value
            sts <- NodeStates(node)
            if (!(ov1 %in% sts)) {
              ov1 <- sts[toupper(sts)==toupper(ov1)]
            }
            if (length(ov1) > 0L) {
              flog.trace("Setting observable %s to %s.",NodeName(node),ov1)
              NodeFinding(node) <- ov1
            } else {
              flog.warn("Observable %s has unknown value %s, skipping.",
                        NodeName(node), ov1)
            }
            invisible(node)
          })

setMethod("PnodeEvidence<-",c("PomNode","factor"),
          function (node,value) {
              stop("Function Not Implemented")
            PnodeEvidence(node) <- as.character(value)
            })
setMethod("PnodeEvidence<-",c("PomNode","NULL"),
          function (node,value) {
              stop("Function Not Implemented")
            RetractNodeFinding(node)
            })
setMethod("PnodeEvidence<-",c("PomNode","logical"),
          function (node,value) {
              stop("Function Not Implemented")
            levs <- NodeLevels(node)
            if (length(levs) != 2L) {
              PnodeEvidence(node) <- as.character(value)
            } else {
              v1 <- names(levs)[levs==as.numeric(value)]
              if (length(v1) != 1L) {
                stop("When setting ",NodeName(node)," expected ",names(levs),
                     " got ",value)
              }
              PnodeEvidence(node) <- v1
            }
            node
          })

setMethod("PnodeEvidence<-",c("PomNode","ANY"),
          function(node,value) {
              stop("Function Not Implemented")
            warning("Don't know how to set ",NodeName(node)," to ",
                 as.character(value),".  Skipping.")
          })



##############################################################
#### Net Functions


### Netica specific implementations for the generics.
##'Class \code{"PomNet"} as a \code{"Pnet"}
##'
##'
##'The \code{PPomegranite} package supplies the needed methods so that the
##'\code{RNetica::\linkS4class{PomNet}} object is an instance of the
##'\code{Peanut::\linkS4class{Pnet}} object.
##'
##'
##'@name Pnet.PomNet
##'@aliases Pnet.PomNet PnetCompile,PomNet-method
##'PnetDescription,PomNet-method PnetDescription<-,PomNet-method
##'PnetHub,PomNet-method PnetHub<-,PomNet-method PnetName,PomNet-method
##'PnetName<-,PomNet-method PnetTitle,PomNet-method
##'PnetTitle<-,PomNet-method PnetDescription,PomNet-method
##'PnetDescription<-,PomNet-method PnetPathname,PomNet-method
##'PnetPathname<-,PomNet-method PnetPriorWeight,PomNet-method
##'PnetPriorWeight<-,PomNet-method as.Pnet,PomNet-method
##'is.Pnet,PomNet-method
##'@docType class
##'@section Extends:
##'
##'See \code{\linkS4class{PomNet}} for a description of the Netica class.
##'
##'With these methods, \code{\linkS4class{PomNet}} now extends
##'\code{\linkS4class{Pnet}}.
##'
##'All reference classes extend and inherit methods from
##'\code{"\linkS4class{envRefClass}"}.
##'@author Russell Almond
##'@seealso
##'
##'Base class: \code{\linkS4class{PomNet}}.
##'
##'Mixin class: \code{\linkS4class{Pnet}}.
##'
##'Methods (from \code{Peanut} package.):
##'
##'\code{\link[Peanut]{PnetCompile}}, \code{\link[Peanut]{PnetHub}},
##'\code{\link[Peanut]{PnetName}}, \code{\link[Peanut]{PnetTitle}},
##'\code{\link[Peanut]{PnetDescription}}, \code{\link[Peanut]{PnetPathname}},
##'\code{\link[Peanut]{as.Pnet}}, \code{\link[Peanut]{is.Pnet}}.
##'@keywords classes graphs
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'curd <- getwd()
##'setwd(file.path(library(help="PPomegranite")$path, "testnets"))
##'
##'## PnetHub
##'PM <- ReadNetworks("miniPP-CM.dne", session=sess)
##'stopifnot(PnetHub(PM)=="")
##'
##'EM1 <- ReadNetworks("PPcompEM.dne", session=sess)
##'stopifnot(PnetHub(EM1)=="miniPP_CM")
##'
##'foo <- CreateNetwork("foo",sess)
##'stopifnot(is.na(PnetHub(foo)))
##'PnetHub(foo) <- PnetName(PM)
##'stopifnot(PnetHub(foo)=="miniPP_CM")
##'
##'## PnetCompile
##'PnetCompile(PM)
##'marginPhysics <- Statistic("PnodeMargin","Physics","Pr(Physics)")
##'calcStat(marginPhysics,PM)
##'
##'net <- CreateNetwork("funNet",sess)
##'stopifnot(PnetName(net)=="funNet")
##'
##'PnetName(net)<-"SomethingElse"
##'stopifnot(PnetName(net)=="SomethingElse")
##'
##'## PnetPathname
##'stopifnot(PnetPathname(PM)=="miniPP-CM.dne")
##'PnetPathname(PM) <- "StudentModel1.dne"
##'stopifnot(PnetPathname(PM)=="StudentModel1.dne")
##'
##'##PnetTitle and PnetDescirption
##'firstNet <- CreateNetwork("firstNet",sess)
##'
##'PnetTitle(firstNet) <- "My First Bayesian Network"
##'stopifnot(PnetTitle(firstNet)=="My First Bayesian Network")
##'
##'now <- date()
##'PnetDescription(firstNet)<-c("Network created on",now)
##'## Print here escapes the newline, so is harder to read
##'cat(PnetDescription(firstNet),"\n")
##'stopifnot(PnetDescription(firstNet) ==
##'  paste(c("Network created on",now),collapse="\n"))
##'
##'
##'
##'DeleteNetwork(list(PM,EM1,foo,net,firstNet))
##'stopSession(sess)
##'setwd(curd)
##'
##'
NULL
##'Gets or Sets the name of a Netica network.
##'
##'Gets or sets the name of the network. Names must conform to the
##'\code{\link{IDname}} rules
##'
##'Network names must conform to the \code{\link{IDname}} rules for Netica
##'identifiers.  Trying to set the network to a name that does not conform to
##'the rules will produce an error, as will trying to set the network name to a
##'name that corresponds to another different network.
##'
##'The \code{\link{PnetTitle}()} function provides another way to name a
##'network which is not subject to the \code{IDname} restrictions.
##'
##'@aliases PnetName PnetName<-
##'@param net A \code{\link{PomNet}} object which links to the network.
##'@param value A character scalar containing the new name.
##'@return The name of the network as a character vector of length 1.
##'
##'The setter method returns the modified object.
##'@note \code{PomNet} objects are internally implemented as character
##'vectors giving the name of the network.  If a network is renamed, then it is
##'possible that R will hold onto an old reference that still using the old
##'name.  In this case, \code{PnetName(net)} will give the correct name, and
##'\code{GetNamedNets(PnetName(net))} will return a reference to a corrected
##'object.
##'@author Russell Almond
##'@seealso \code{\link{CreateNetwork}()}, \code{\link{PomNet}},
##'\code{\link{GetNamedNetworks}()}, \code{\link{PnetTitle}()}
##'@references 281 \url{http://norsys.com/onLineAPIManual/index.html}:
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"GetNetName_bn")\href{http://norsys.com/onLineAPIManual/functions/GetNetName_bn.htmlGetNetName_bn()},
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"SetNetName_bn")\href{http://norsys.com/onLineAPIManual/functions/SetNetName_bn.htmlSetNetName_bn()}
##'@keywords interface attribute
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'net <- CreateNetwork("funNet",sess)
##'netcached <- net
##'
##'stopifnot(PnetName(net)=="funNet")
##'
##'PnetName(net)<-"SomethingElse"
##'stopifnot(PnetName(net)=="SomethingElse")
##'
##'stopifnot(PnetName(net)==PnetName(netcached))
##'
##'DeleteNetwork(net)
##'@exportMethod
setMethod("PnetName","PomNet", function (net){
    stop("Function Not Implemented")
    nme <- NetworkUserField(net,"Truename")
    if (is.null(nme) || is.na(nme)) {
        nme <- NetworkName(net)
    }
    nme
})

setMethod("PnetName<-","PomNet", function (net, value) {
    stop("Function Not Implemented")
  NetworkUserField(net,"Truename")
  NetworkName(net) <- as.IDname(value)
  invisible(net)
})

##'Gets the title or comments associated with a Netica network.
##'
##'The title is a longer name for a network which is not subject to the Netica
##'\code{\link{IDname}} restrictions.  The comment is a free form text
##'associated with a network.
##'
##'The title is meant to be a human readable alternative to the name, which is
##'not limited to the \code{\link{IDname}} restrictions.  The title also
##'affects how the network is displayed in the Netica GUI.
##'
##'The comment is any text the user chooses to attach to the network.  If
##'\code{value} has length greater than 1, the vector is collapsed into a long
##'string with newlines separating the components.
##'
##'@aliases PnetTitle PnetDescription PnetTitle<- PnetDescription<-
##'@param net A \code{\link{PomNet}} object.
##'@param value A character object giving the new title or comment.
##'@return A character vector of length 1 providing the title or comment.
##'@author Russell Almond
##'@seealso \code{\link{PomNet}}, \code{\link{NetworkName}()}
##'@references 281 \url{http://norsys.com/onLineAPIManual/index.html}:
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"GetNetTitle_bn")\href{http://norsys.com/onLineAPIManual/functions/GetNetTitle_bn.htmlGetNetTitle_bn()},
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"SetNetTitle_bn")\href{http://norsys.com/onLineAPIManual/functions/SetNetTitle_bn.htmlSetNetTitle_bn()},
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"GetNetComments_bn")\href{http://norsys.com/onLineAPIManual/functions/GetNetComments_bn.htmlGetNetComments_bn()},
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"SetNetComments_bn")\href{http://norsys.com/onLineAPIManual/functions/SetNetComments_bn.htmlSetNetComments_bn()}
##'@keywords graph interface attribute
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'firstNet <- CreateNetwork("firstNet",sess)
##'
##'PnetTitle(firstNet) <- "My First Bayesian Network"
##'stopifnot(PnetTitle(firstNet)=="My First Bayesian Network")
##'
##'now <- date()
##'NetworkComment(firstNet)<-c("Network created on",now)
##'## Print here escapes the newline, so is harder to read
##'cat(NetworkComment(firstNet),"\n")
##'stopifnot(NetworkComment(firstNet) ==
##'  paste(c("Network created on",now),collapse="\n"))
##'
##'
##'DeleteNetwork(firstNet)
##'@exportMethod
setMethod("PnetTitle","PomNet", function (net) {
    stop("Function Not Implemented")
  NetworkTitle(net)
})


setMethod("PnetTitle<-","PomNet", function (net, value) {
    stop("Function Not Implemented")
  NetworkTitle(net) <- value
  invisible(net)
})


## The HUB is the name of the CM for an EM, or "" for an CM.
setMethod("PnetHub","PomNet", function (net) {
    stop("Function Not Implemented")
  NetworkUserField(net,"Hub")
})


## Value could be the actual model or its name.
setMethod("PnetHub<-","PomNet", function (net, value) {
  NetworkUserField(net,"Hub") <-value
  invisible(net)
})

## Note:  This is not necessarily the same as the GetNeticaPathname() function.
setMethod("PnetPathname","PomNet", function (net) {
  value <- NetworkUserField(net,"Pathname")
  if (is.na(value) || is.null(value) || nchar(value)==0L) {
    value <- attr(net,"Filename")
  }
  value
})

setMethod("PnetPathname<-","PomNet", function (net, value) {
  NetworkUserField(net,"Pathname") <-value
  invisible(net)
})

setMethod("PnetDescription","PomNet", function (net) {
  NetworkComment(net)
})

setMethod("PnetDescription<-","PomNet", function (net, value) {
  NetworkComment(net) <- value
  invisible(net)
})

##'Finds nodes in a Netica Pnet.
##'
##'
##'The function \code{\link[Peanut]{PnetFindNode}} finds a node in a
##'\code{\linkS4class{Pnet}} with the given name.  If no node with the
##'specified name found, it will return \code{NULL}.
##'
##'The function \code{\link[Peanut]{PnetPnodes}} returns nodes which have been
##'marked as pnodes, that is nodes that have \dQuote{pnodes} in their
##'\code{\link[Peanut]{PnodeLabels}}.
##'
##'Although each \code{\link[Peanut]{Pnode}} belongs to a single network, a
##'network contains many nodes.  Within a network, a node is uniquely
##'identified by its name.  However, nodes can be renamed (see
##'\code{\link[RNetica]{NodeName}()}).
##'
##'A \code{\linkS4class{PomNode}} is also a \code{\linkS4class{Pnode}} if it
##'has the label (node set) \dQuote{pnodes}.
##'
##'The function \code{PnetPnodes()} returns all the Pnodes in the network,
##'however, the order of the nodes in the network could be different in
##'different calls to this function.
##'
##'The form \code{PnetPnodes(net)<-value} sets the list of nodes in
##'\code{value} to be the set of Pnodes; removing nodes which are not in the
##'\code{value} from the set of Pndoes.
##'
##'The Pnodes are not necesarily all of the nodes in the Netica network.  The
##'complete list of ndoes can be found through the
##'\code{RNetica::\link[RNetica]{NetworkAllNodes}} function.
##'
##'@aliases PnetFindNode,PomNet-method PnetPnodes,PomNet-method
##'PnetPnodes<-,PomNet-method
##'@param net The \code{Pnet} to search.
##'@param name A character vector giving the name or names of the desired
##'nodes.  Names must follow the \code{\link{IDname}} protocol.
##'@param value A list of \code{\linkS4class{PomNode}} objects in the
##'network to be marked as Pnodes.
##'@return The \code{\link[Peanut]{Pnode}} object or list of \code{Pnode}
##'objects corresponding to \code{names}, or a list of all node objects for
##'\code{PnetPnodes()}.  In the latter case, the names will be set to the node
##'names.
##'@note \code{PomNode} objects do not survive the life of a Netica session
##'(or by implication an R session).  So the safest way to "save" a
##'\code{PomNode} object is to recreate it using \code{PnetFindNode()} after
##'the network is reloaded.
##'@author Russell Almond
##'@seealso
##'
##'Generic functions: \code{\link[Peanut]{PnetPnodes}()},
##'\code{\link[Peanut]{PnetFindNode}()},
##'
##'Related functions in RNetica package:
##'\code{\link[RNetica]{NetworkFindNode}},
##'\code{\link[RNetica]{NetworkAllNodes}}
##'@references 281 \url{http://norsys.com/onLineAPIManual/index.html},
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"GetNodeNamed_bn")\href{http://norsys.com/onLineAPIManual/functions/GetNodeNamed_bn.htmlGetNodeNamed_bn()},
##'c("\\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}",
##'"GetNetNodes_bn")\href{http://norsys.com/onLineAPIManual/functions/GetNetNodes_bn.htmlGetNetNodes_bn()}
##'@keywords interface graphs utilities
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'tnet <- CreateNetwork("TestNet",sess)
##'nodes <- NewDiscreteNode(tnet,c("A","B","C"))
##'
##'nodeA <- PnetFindNode(tnet,"A")
##'stopifnot (nodeA==nodes[[1]])
##'
##'nodeBC <- PnetFindNode(tnet,c("B","C"))
##'stopifnot(nodeBC[[1]]==nodes[[2]])
##'stopifnot(nodeBC[[2]]==nodes[[3]])
##'
##'allnodes <- PnetPnodes(tnet)
##'stopifnot(length(allnodes)==0)
##'
##'## Need to mark nodes a Pnodes before they will be seen.
##'nodes <- lapply(nodes,as.Pnode)
##'allnodes <- PnetPnodes(tnet)
##'stopifnot(length(allnodes)==3)
##'stopifnot(any(sapply(allnodes,"==",nodeA))) ## NodeA in there somewhere.
##'
##'DeleteNetwork(tnet)
##'@exportMethod
setMethod("PnetFindNode","PomNet", function(net,name) {
    stop("Function Not Implemented")
  NetworkFindNode(net,as.IDname(name))
})

##'Methods for (un)serializing a Netica Network
##'
##'Methods for functions \code{\link[Peanut]{PnetSerialize}} and
##'\code{\link[Peanut]{unserializePnet}} in package \pkg{Peanut}, which
##'serialize \code{\link[RNetica]{PomNet}} objects.  Note that in this case,
##'the factory is the \code{\link[RNetica]{PomSession}} object.  These
##'methods assume that there is a global variable with the name of the session
##'object which points to the Netica session.
##'
##'
##'@name PnetSerialize
##'@aliases PnetSerialize-methods unserializePnet-methods
##'PnetSerialize,PomNet-method unserializePnet,PomSession-method
##'@docType methods
##'@section Methods: \describe{ \item{list("PnetSerialize")}{ Returns a vector
##'with three components.  The \code{name} field is the name of the network.
##'The \code{data} component is a raw vector produced by calling
##'\code{\link[base]{serialize}(...,NULL)} on the output of a
##'\code{\link[RNetica]{WriteNetworks}} operation.  The \code{factory}
##'component is the name of the \code{\link[RNetica]{PomSession}} object.
##'Note that the \code{\link[Peanut]{PnetUnserialize}} function assumes that
##'there is a global variable with name given by the factory argument which
##'contains an appropriate \code{PomSession} object for the restoration.
##'}\item{, }{ Returns a vector with three components.  The \code{name} field
##'is the name of the network.  The \code{data} component is a raw vector
##'produced by calling \code{\link[base]{serialize}(...,NULL)} on the output of
##'a \code{\link[RNetica]{WriteNetworks}} operation.  The \code{factory}
##'component is the name of the \code{\link[RNetica]{PomSession}} object.
##'Note that the \code{\link[Peanut]{PnetUnserialize}} function assumes that
##'there is a global variable with name given by the factory argument which
##'contains an appropriate \code{PomSession} object for the restoration.
##'}\item{list("signature(net = \"PomNet\")")}{ Returns a vector with three
##'components.  The \code{name} field is the name of the network.  The
##'\code{data} component is a raw vector produced by calling
##'\code{\link[base]{serialize}(...,NULL)} on the output of a
##'\code{\link[RNetica]{WriteNetworks}} operation.  The \code{factory}
##'component is the name of the \code{\link[RNetica]{PomSession}} object.
##'Note that the \code{\link[Peanut]{PnetUnserialize}} function assumes that
##'there is a global variable with name given by the factory argument which
##'contains an appropriate \code{PomSession} object for the restoration. }
##'\item{list("unserializePnet")}{ This method reverses the previous one.  In
##'particular, it applies \code{\link[RNetica]{ReadNetworks}} to the serialized
##'object.
##'
##'}\item{, }{ This method reverses the previous one.  In particular, it
##'applies \code{\link[RNetica]{ReadNetworks}} to the serialized object.
##'
##'}\item{list("signature(factory = \"PomSession\")")}{ This method reverses
##'the previous one.  In particular, it applies
##'\code{\link[RNetica]{ReadNetworks}} to the serialized object.
##'
##'} }
##'@keywords methods graphs IO
##'@examples
##'
##'## Need to create session whose name is is the same a the symbol it is
##'## stored in. 
##'MySession <- PomSession(SessionName="MySession")
##'startSession(MySession)
##'
##'irt5 <- ReadNetworks(file.path(library(help="RNetica")$path,
##'                           "sampleNets","IRT5.dne"), session=MySession)
##'NetworkAllNodes(irt5)
##'CompileNetwork(irt5) ## Ready to enter findings
##'NodeFinding(irt5$nodes$Item_1) <- "Right"
##'NodeFinding(irt5$nodes$Item_2) <- "Wrong"
##'
##'## Serialize the network
##'irt5.ser <- PnetSerialize(irt5)
##'stopifnot (irt5.ser$name=="IRT5",irt5.ser$factory=="MySession")
##'
##'NodeFinding(irt5$nodes$Item_3) <- "Right"
##'
##'
##'## now revert by unserializing.
##'irt5 <- PnetUnserialize(irt5.ser)
##'NetworkAllNodes(irt5)
##'stopifnot(NodeFinding(irt5$nodes$Item_1)=="Right",
##'          NodeFinding(irt5$nodes$Item_2)=="Wrong",
##'          NodeFinding(irt5$nodes$Item_3)=="@NO FINDING")
##'
##'DeleteNetwork(irt5)
##'stopSession(MySession)          
##'
##'
##'@exportMethod
setMethod("PnetSerialize","PomNet",
          function (net) {
              stop("Function Not Implemented")
            factory <- net$Session$SessionName
            name <- PnetName(net)
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            WriteNetworks(net,tmpfile)
            data <- serialize(readLines(tmpfile),NULL)
            list(name=name,factory=factory,data=data)
          })


setMethod("unserializePnet","PomSession",
          function(factory,data) {
              stop("Function Not Implemented")
            name <- data$name
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            writeLines(unserialize(data$data),tmpfile)
            oldnet <- factory$findNet(name)
            if (!is.null(oldnet) && is.active(oldnet)) {
              flog.warn("Replacing old version of network %s.",
                           name)
              DeleteNetwork(oldnet)
            }
            ReadNetworks(tmpfile,factory)
          })

setMethod("PnetCompile","PomNet",function(net) {
    stop("Function Not Implemented")
    CompileNetwork(net)
})

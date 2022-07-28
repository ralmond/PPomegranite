##'Class \code{"PomNetWarehouse"}
##'
##'
##'A \code{\link[Peanut]{Warehouse}} (specifically a \code{PnetWarehouse})
##'object which holds and builds \code{\link[RNetica]{PomNet}} objects.  In
##'particular, its \code{\link[Peanut]{WarehouseManifest}} contains a network
##'manifest (see \code{\link[Peanut]{BuildNetManifest}}) which contains
##'information about how to either load the networks from the file system, or
##'build them on demand.
##'
##'
##'The \code{PomNetWarehouse} either supplies prebuilt (i.e., already in the Netica
##'session) nets or builds them from the instructions found in the manifest.
##'In particular, the function \code{WarehouseSupply} will attempt to:
##'\enumerate{ \itemFind an existing network with \code{name} in the session.
##'\itemTry to read the network from the location given in the \code{Pathname}
##'column of the manifest.  \itemBuild a blank network, using the metadata in
##'the manifest.  }
##'
##'The manifest is an object of type \code{\link[base]{data.frame}} where the
##'columns have the values show below.  The key is the \dQuote{Name} column
##'which should be unique for each row. The \var{name} argument to
##'\code{WarehouseData} should be a character scalar corresponding to name, and
##'it will return a \code{data.frame} with a single row.
##'
##'\describe{ \item{Name}{A character value giving the name of the network.
##'This should be unique for each row and normally must conform to variable
##'naming conventions.  Corresponds to the function
##'\code{\link[Peanut]{PnetName}}.} \item{Title}{An optional character value
##'giving a longer human readable name for the netowrk. Corresponds to the
##'function \code{\link[Peanut]{PnetTitle}}.} \item{Hub}{If this model is
##'incomplete without being joined to another network, then the name of the hub
##'network.  Otherwise an empty character vector. Corresponds to the function
##'\code{\link[Peanut]{PnetHub}}.} \item{Pathname}{The location of the file
##'from which the network should be read or to which it should be written.
##'Corresponds to the function \code{\link[Peanut]{PnetPathname}}.}
##'\item{Description}{An optional character value documenting the purpose of
##'the network. Corresponds to the function
##'\code{\link[Peanut]{PnetDescription}}.} }
##'
##'The function \code{\link[Peanut]{BuildNetManifest}} will build a manifest
##'for an existing collection of networks.
##'
##'@name PomNetWarehouse-class
##'@aliases PomNetWarehouse-class ClearWarehouse,PomNetWarehouse-method
##'is.PnetWarehouse,PomNetWarehouse-method WarehouseData,PomNetWarehouse-method
##'WarehouseDirectory,PomNetWarehouse-method
##'WarehouseDirectory<-,PomNetWarehouse-method WarehouseFetch,PomNetWarehouse-method
##'WarehouseFree,PomNetWarehouse-method WarehouseInventory,PomNetWarehouse-method
##'WarehouseMake,PomNetWarehouse-method WarehouseManifest,PomNetWarehouse-method
##'WarehouseManifest<-,PomNetWarehouse,data.frame-method
##'WarehouseUnpack,PomNetWarehouse-method WarehouseSupply,PomNetWarehouse-method
##'WarehouseCopy,PomNetWarehouse,PomNet-method
##'WarehouseSave,PomNetWarehouse,PomNet-method
##'WarehouseSave,PomNetWarehouse,character-method as.legal.name,PomNetWarehouse-method
##'is.legal.name,PomNetWarehouse-method is.valid,PomNetWarehouse-method
##'@docType class
##'@note
##'
##'The \code{PomNetWarehouse} implementatation contains an embedded
##'\code{\link[RNetica]{PomSession}} object.  When \code{WarehouseSupply} is
##'called, it attempts to satisfy the demand by trying in order: \enumerate{
##'\itemSearch for the named network in the active networks in the session.
##'\itemIf not found in the session, it will attempt to load the network from
##'the \code{Pathname} field in the manifest.  \itemIf the network is not found
##'and there is not file at the target pathename, a new blank network is built
##'and the appropriate fields are set from the metadata.  }
##'@section Objects from the Class:
##'
##'Objects can be created by calls of the form \code{\link{PomNetWarehouse}( ...)}.
##'
##'This class is a subclass of \code{PnetWarehouse} in the
##'\code{\link{Peanut-package}}.
##'
##'This is a reference object and typically there is only one instance per
##'project.
##'@author Russell Almond
##'@seealso In Peanut Package: \code{\link[Peanut]{Warehouse}},
##'\code{\link[Peanut]{WarehouseManifest}},
##'\code{\link[Peanut]{BuildNetManifest}}
##'
##'Implementation in the \code{PPomegranite} package: \code{\link{PomNetWarehouse}},
##'\code{\link{MakePnet.PomNet}}
##'@references
##'
##'The following is a Google sheet where an example network manifest can be
##'found on the \code{nets} tab.
##'\url{https://docs.google.com/spreadsheets/d/1SiHQTLBNHQ-FUPnNzf9jPm9ifUG-c8f_6ljOrEcdl9M/}
##'@keywords classes graphs
##'@examples
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'## PomNetWarehouse is the PPomegranite Net Warehouse.
##'## This provides an example network manifest.
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'Nethouse <- PomNetWarehouse(manifest=netman1,session=sess,key="Name")
##'
##'## is.PnetWarehouse -- tests for PnetWarehouse.
##'stopifnot(is.PnetWarehouse(Nethouse))
##'
##'## WarehouseManifest
##'stopifnot(all.equal(WarehouseManifest(Nethouse),netman1))
##'
##'## WarehouseData
##'stopifnot(all.equal(WarehouseData(Nethouse,"miniPP_CM")[-4],
##'   netman1["miniPP_CM",-4]),
##'   ## Pathname has leading address prefix instered.
##'   basename(WarehouseData(Nethouse,"miniPP_CM")$Pathname) ==
##'   basename(netman1["miniPP_CM","Pathname"]))
##'
##'## WarehouseManifest<- 
##'netman2 <- netman1
##'netman2["miniPP_CM","Pathname"] <- "mini_CM.dne"
##'WarehouseManifest(Nethouse) <- netman2
##'
##'stopifnot(all.equal(WarehouseData(Nethouse,"miniPP_CM")[,-4],
##'   netman2["miniPP_CM",-4]),
##'   basename(WarehouseData(Nethouse,"miniPP_CM")$Pathname) ==
##'   basename(netman2["miniPP_CM","Pathname"]))
##'WarehouseManifest(Nethouse) <- netman1
##'
##'## Usually way to access nets is through warehouse supply
##'CM <- WarehouseSupply(Nethouse, "miniPP_CM")
##'EM <- WarehouseSupply(Nethouse, "PPcompEM")
##'stopifnot(is.active(CM),is.active(EM))
##'
##'## WarehouseFetch -- Returns NULL if does not exist
##'stopifnot(is.null(WarehouseFetch(Nethouse,"PPconjEM")))
##'
##'## WarehouseMake -- Make the net anew.
##'EM1 <- WarehouseMake(Nethouse,"PPconjEM")
##'EM1a <- WarehouseFetch(Nethouse,"PPconjEM")
##'stopifnot(PnetName(EM1)==PnetName(EM1a))
##'
##'## WarehouseFree -- Deletes the Net
##'WarehouseFree(Nethouse,"PPconjEM")
##'stopifnot(!is.active(EM1))
##'
##'## ClearWarehouse -- Deletes all nets
##'ClearWarehouse(Nethouse)
##'stopifnot(!is.active(EM),!is.active(CM))
##'
##'stopSession(sess)
##'
##'
##'@exportClass
PomNetWarehouse <- setClass("PomNetWarehouse",
                        slots=c(manifest="data.frame",
                                session="PomSession",
                                address="character",
                                key="character",
                                prefix="character")
)


##'Constructor for the \code{BNWarehosue} class.
##'
##'
##'This is the constructor for the \code{\linkS4class{PomNetWarehouse}} class.
##'This produces \code{\link[RNetica]{PomNet}} objects, which are instances
##'of the \code{\link[Peanut]{Pnet}} abstract class.
##'
##'
##'@param manifest A data frame containing instructions for building the nets.
##'See \code{\link[Peanut]{BuildNetManifest}}.
##'@param session A link to a \code{\link[RNetica]{PomSession}} object for
##'managing the nets.
##'@param address A character scalar giving the path in which the \dQuote{.dne}
##'files containing the networks are stored.
##'@param key A character scalar giving the name of the column in the manifest
##'which contains the network name.
##'@param prefix A character scaler used in front of numeric names to make
##'legal Netica names. (See \code{\link[RNetica]{as.IDname}}).
##'@return
##'
##'An object of class \code{\linkS4class{PomNetWarehouse}}.
##'@author Russell Almond
##'@seealso
##'
##'\code{\link[Peanut]{Warehouse}} for the general warehouse protocol.
##'@keywords manip graph
##'@examples
##'
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'### This tests the manifest and factory protocols.
##'
##'nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
##'                     row.names=1,stringsAsFactors=FALSE)
##'
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'
##'
##'### Test Net building
##'Nethouse <- PomNetWarehouse(manifest=netman1,session=sess,key="Name")
##'stopifnot(is.PnetWarehouse(Nethouse))
##'
##'setwd(paste(library(help="PPomegranite")$path, "testnets",sep=.Platform$file.sep))
##'CM <- WarehouseSupply(Nethouse,"miniPP_CM")
##'stopifnot(is.null(WarehouseFetch(Nethouse,"PPcompEM")))
##'EM1 <- WarehouseMake(Nethouse,"PPcompEM")
##'
##'EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM", "PPdurAttEM"),
##'              function(nm) WarehouseSupply(Nethouse,nm))
##'
##'
##'
##'@export PomNetWarehouse

PomNetWarehouse <- function(manifest=data.frame(),session=getDefaultSession(),
                        address=".",key=c("Name"),prefix="S")
    stop("Function Not Implemented")
  new("PomNetWarehouse",manifest=manifest, session=session, address=address,
      key=key, prefix=prefix)

setIs("PomNetWarehouse","PnetWarehouse")

setMethod(ClearWarehouse,"PomNetWarehouse",
          function(warehouse) {
    stop("Function Not Implemented")
            objs <- objects(warehouse@session$nets)
            for (obj in objs) {
              net <- warehouse@session$nets[[obj]]
              if (is.PomNet(net) && is.active(net)) {
                flog.trace("Clearing Network %s",obj)
                DeleteNetwork(net)
              }
            }
          })

setMethod(WarehouseManifest,"PomNetWarehouse",
          function(warehouse) {warehouse@manifest})
setMethod("WarehouseManifest<-",c("PomNetWarehouse","data.frame"),
          function(warehouse,value) {
            for (ky in warehouse@key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<- value; warehouse})

##'Gets or sets the directory associated with an PomNetWarehouse
##'
##'
##'If a network is not available, a \code{\link{PomNetWarehouse}} will look in the
##'specified directory to find the \code{.dne} or \code{.neta} files associated
##'with the Bayesian networks.
##'
##'
##'@aliases WarehouseDirectory WarehouseDirectory<-
##'@param warehouse An object of type \code{\link{PomNetWarehouse}}.
##'@param value A character scalar giving the new pathname for the net
##'directory.
##'@return
##'
##'A character string giving the path associated with a Warehouse.
##'@author Russell Almond
##'@seealso \code{\link{PomNetWarehouse}}, \code{\link{MakePnet.PomNet}}
##'@keywords manip interface
##'@examples
##'
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'
##'Nethouse <- PomNetWarehouse(manifest=netman1,session=sess,key="Name")
##'stopifnot(WarehouseDirectory(Nethouse)==".")
##'
##'## Set up to use a temporary directory (all networks will be built fresh)
##'td <- tempdir()
##'WarehouseDirectory(Nethouse) <- td
##'stopifnot(WarehouseDirectory(Nethouse)==td)
##'
##'@export
setGeneric("WarehouseDirectory",
           function (warehouse) standardGeneric("WarehouseDirectory"))
setMethod("WarehouseDirectory","PomNetWarehouse",
          function (warehouse) {
            warehouse@address
          })
setGeneric("WarehouseDirectory<-",
           function (warehouse, value) standardGeneric("WarehouseDirectory<-"))
setMethod("WarehouseDirectory<-","PomNetWarehouse",
          function (warehouse,value) {
            warehouse@address <- value
            warehouse
          })



setMethod(WarehouseData,"PomNetWarehouse",
          function(warehouse,name) {
              stop("Function Not Implemented")
            key <- warehouse@key
            if (length(name) != length(key))
              stop("Expected name to contain elements",key)
            manifest <- warehouse@manifest
            whch = rep(TRUE,nrow(manifest))
            for (i in 1:length(key)) {
              whch <- whch & manifest[[key[i]]] == name[i]
            }
            dat <- manifest[whch,,drop=FALSE]
            ## Add directory information to pathnames.
            dir <- do.call("file.path",as.list(warehouse@address))
            if (length(dir) > 0L)
              dat$Pathname <- file.path(dir,dat$Pathname)
            dat
          })

setMethod(WarehouseFetch,"PomNetWarehouse",
          function(warehouse,name) {
              stop("Function Not Implemented")
            if (length(name) != 1L)
              stop("Expected name ",name," to be unique.")
            warehouse@session$nets[[as.IDname(name)]]
          })

setMethod("WarehouseSupply", c("PomNetWarehouse"), function(warehouse,name) {
              stop("Function Not Implemented")
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  if (!is.active(val)) {
    warehouse@session$nets[[as.IDname(name)]] <- NULL
    val <- WarehouseMake(warehouse,name)
  }
  val
})


setMethod(WarehouseMake,"PomNetWarehouse",
          function(warehouse,name) {
              stop("Function Not Implemented")
            if (length(name) != 1L)
              stop("Expected name to be unique.")
            dat <- WarehouseData(warehouse,name)
            if (nrow(dat) <1L)
              stop("Cannot find manifest data for network ",name)
            if (nrow(dat) >2L)
              warning("Multiple manifest data row for network ",name)
            sess <- warehouse@session
            if (!is.null(sess$nets[[as.IDname(name)]])) {
              warning("Deleting old network ",name)
              DeleteNetwork(sess$nets[[as.IDname(name)]])
            }
            MakePnet.PomNet(sess,name,dat)
          })

setMethod(WarehouseSave,c("PomNetWarehouse","character"),
          function(warehouse,obj) {
            net <- warehouse@session$nets[[as.IDname(obj)]]
            if (is.null(net)) {
              warning("Network named ",obj," does not exist, not saving.")
            } else {
              WarehouseSave(warehouse,net)
            }
          })

setMethod(WarehouseSave,c("PomNetWarehouse","PomNet"),
          function(warehouse,obj) {
              stop("Function Not Implemented")
            name <- PnetName(obj)
            pname <- PnetPathname(obj)
            WriteNetworks(obj,pname)
          })


setMethod(WarehouseFree,"PomNetWarehouse",
          function(warehouse,name) {
            net <- WarehouseFetch(warehouse,name)
            if (is.null(net)) {
              flog.trace("Network for name %s not found, skipping.",name)
            } else {
              if (is.active(net))
                DeleteNetwork(net)
              if (!is.null(warehouse@session$nets[[name]]))
                rm(list=name,envir=warehouse@session$nets)
            }
          })

setMethod(WarehouseCopy,c("PomNetWarehouse","PomNet"),
          function(warehouse,obj,newname) {
              stop("Function Not Implemented")
            newname <- as.legal.name(warehouse,newname)
            CopyNetworks(obj,newname)
          })

setMethod(is.legal.name,"PomNetWarehouse",
          function(warehouse,name)
              stop("Function Not Implemented")
            is.IDname(name)
          )

setMethod(as.legal.name,"PomNetWarehouse",
          function(warehouse,name)
              stop("Function Not Implemented")
            as.IDname(name,warehouse@prefix)
          )

setMethod(is.valid,"PomNetWarehouse",
          function(warehouse,object)
              stop("Function Not Implemented")
            is.active(object)
          )


setMethod(WarehouseInventory,"PomNetWarehouse",
          function(warehouse) {
              stop("Function Not Implemented")
            allKeys <- warehouse@manifest[,warehouse@key,drop=FALSE]
            built <- sapply(1L:nrow(allKeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allKeys[built, ,drop=FALSE]})

setMethod(is.PnetWarehouse,"PomNetWarehouse",
          function(obj) {TRUE})

setMethod("WarehouseUnpack", "PomNetWarehouse",
          function(warehouse,serial) {
              stop("Function Not Implemented")
            unserializePnet(warehouse@session,serial)
            warehouse@session$nets[[as.IDname(serial$name)]]
          })



#######  Node Warehouse

##'Class \code{"PomNodeWarehouse"}
##'
##'
##'This is a container for node objects, which are instances of the
##'\code{\link[Peanut]{Pnode}} class.  If a requested node is not already
##'built, it can be built from the description found in the warehouse.  In
##'implements the \code{\link[Peanut]{Warehouse}} protocol.
##'
##'
##'The \code{PomNodeWarehouse} generally works with a paired
##'\code{\link{PomNetWarehouse}} which supplies the network.  It assumes that the
##'referenced network already exists or has been loaded from a file.  If the
##'node already exists in the network, it simply returns it.  If not, it
##'creates it using the metadata in the manifest.
##'
##'The manifest is an object of type \code{\link[base]{data.frame}} where the
##'columns have the values show below.  The key is the pair of columns
##'(\dQuote{Model}, \dQuote{NodeName}), with each pair identifying a set of
##'rows correpsonding to the possible states of the node.  The \var{name}
##'argument to \code{WarehouseData} should be a character vector of length 2
##'with the first component corresonding to the network name and the second to
##'the node name; it will return a \code{data.frame} with multiple rows.
##'
##'Some of the fields of the manifest data apply to the whole node.  In these
##'fields, the value in the first row is used and the rest are ignored.
##'
##'\describe{ \item{NStates}{A integer giving the number of states for a
##'discrete variable or the discritzation of a continuous one.  The number of
##'rows of the manifest data for this node should match this.}
##'\item{Continuous}{A logical value telling whether or not the node should be
##'regarded as continuous.} \item{NodeTitle}{This is a longer unconstrained
##'name for the node.} \item{NodeDescription}{This is a longer string
##'describing the node.} \item{NodeLabels}{This is a comma separated list of
##'tags identifying sets to which the node belongs.  See
##'\code{\link[Peanut]{PnodeLabels}}.} }
##'
##'These fields are repeated for each of the states in the node, as they are
##'different for each state.  The \dQuote{StateName} field is required and must
##'be unique for each row.
##'
##'\describe{ \item{StateName}{The name of the state, this should follow the
##'Netica \link[RNetica]{IDname} conventions.} \item{StateTitle}{This is a
##'longer unconstrained name for the state.} \item{StateDescription}{This is a
##'longer string describing the state.} }
##'
##'Additionally, the following field is used only for discrete nodes:
##'\describe{ \item{StateValue}{This is a numeric value assigned to the state.
##'This value is used when calculating the node expected value.} } The
##'StateValue plays two important roles.  First, when used with the
##'\code{\link[Peanut]{PnodeEAP}} and \code{\link[Peanut]{PnodeSD}} functions,
##'it is the value assigned to the node.  Second, when constructing CPTs using
##'the DiBello framework, it is used at the effective thetas.  See
##'\code{\link[Peanut]{PnodeParentTvals}} and
##'\code{\link[Peanut]{PnodeStateValues}}
##'
##'Continuous nodes in Netica are handled by breaking the interval up into
##'pieces.  This is the function \code{\link[Peanut]{PnodeStateBounds}}.  Note
##'that the bounds should be either monotonically increasing or decreasing and
##'that the lower bound for one category should match lower bound for the next
##'to within a tolerance of .002.  The values \code{Inf} and \code{-Inf} can be
##'used where appropriate.
##'
##'\describe{ \item{LowerBound}{This is a numeric value giving the lower bound
##'for the range for the discritization of the node.} \item{UpperBound}{This is
##'a numeric value giving the upper bound for the range for the} }
##'
##'@name PomNodeWarehouse-class
##'@aliases PomNodeWarehouse-class as.legal.name,PomNodeWarehouse-method
##'ClearWarehouse,PomNodeWarehouse-method is.legal.name,PomNodeWarehouse-method
##'is.PnodeWarehouse,PomNodeWarehouse-method is.valid,PomNodeWarehouse-method
##'WarehouseCopy,PomNodeWarehouse,PomNode-method WarehouseData,PomNodeWarehouse-method
##'WarehouseFetch,PomNodeWarehouse-method WarehouseFree,PomNodeWarehouse-method
##'WarehouseInventory,PomNodeWarehouse-method WarehouseMake,PomNodeWarehouse-method
##'WarehouseSupply,PomNodeWarehouse-method WarehouseManifest,PomNodeWarehouse-method
##'WarehouseManifest<-,PomNodeWarehouse,data.frame-method
##'WarehouseSave,PomNodeWarehouse,ANY-method
##'@docType class
##'@note
##'
##'The test for matching upper and lower bounds is perhaps too strict.  In
##'particular, if the upper and lower bounds mismatch by the least significant
##'digit (e.g., a rounding difference) they will not match.  This is a frequent
##'cause of errors.
##'@section Objects from the Class:
##'
##'Objects can be using the constructor \code{\link{PomNodeWarehouse}}.
##'
##'This class is a subclass of \code{PnodeWarehouse} in the
##'\code{\link{Peanut-package}}.
##'
##'This is a reference object and typically there is only one instance per
##'project.
##'@author Russell Almond
##'@seealso In Peanut Package: \code{\link[Peanut]{Warehouse}},
##'\code{\link[Peanut]{WarehouseManifest}},
##'\code{\link[Peanut]{BuildNodeManifest}}
##'
##'Implementation in the \code{PPomegranite} package: \code{\link{PomNodeWarehouse}},
##'\code{\link{MakePnode.PomNode}}
##'@references
##'
##'The following is a Google sheet where an example node manifest can be found
##'on the \code{nodes} tab.
##'\url{https://docs.google.com/spreadsheets/d/1SiHQTLBNHQ-FUPnNzf9jPm9ifUG-c8f_6ljOrEcdl9M/}
##'@keywords classes
##'@examples
##'
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'## PomNetWarehouse is the PPomegranite Net Warehouse.
##'## This provides an example network manifest.
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'Nethouse <- PomNetWarehouse(manifest=netman1,session=sess,key="Name")
##'
##'nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
##'                     row.names=1,stringsAsFactors=FALSE)
##'
##'Nodehouse <- PomNodeWarehouse(manifest=nodeman1,
##'                         key=c("Model","NodeName"),
##'                         session=sess)
##'
##'CM <- WarehouseSupply(Nethouse,"miniPP_CM")
##'WarehouseSupply(Nethouse,"PPdurAttEM")
##'
##'WarehouseData(Nodehouse,c("miniPP_CM","Physics"))
##'WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))
##'
##'WarehouseData(Nodehouse,c("PPdurAttEM","Attempts"))
##'WarehouseSupply(Nodehouse,c("PPdurAttEM","Attempts"))
##'
##'WarehouseData(Nodehouse,c("PPdurAttEM","Duration"))
##'WarehouseSupply(Nodehouse,c("PPdurAttEM","Duration"))
##'
##'WarehouseFree(Nethouse,"miniPP_CM")
##'WarehouseFree(Nethouse,"PPdurAttEM")
##'stopSession(sess)
##'
##'@exportClass
PomNodeWarehouse <- setClass("PomNodeWarehouse",
                        slots=c(manifest="data.frame",
                                session="PomSession",
                                key="character",
                                prefix="character")
)


##'Constructor for the \code{NNWarehosue} class.
##'
##'
##'This is the constructor for the \code{\linkS4class{PomNodeWarehouse}} class.
##'This produces \code{\link[RNetica]{PomNode}} objects, which are instances
##'of the \code{\link[Peanut]{Pnode}} abstract class.
##'
##'
##'Each network defines its own namespace for nodes, so the key to the node
##'manifest is a pair (\emph{Model},\emph{NodeName}) where \emph{Model} is the
##'name of the net and \code{NodeName} is the name of the node.
##'
##'@param manifest A data frame containing instructions for building the nodes.
##'See \code{\link[Peanut]{BuildNodeManifest}}.
##'@param session A link to a \code{\link[RNetica]{PomSession}} object for
##'managing the nets.
##'@param key A character vector giving the name of the column in the manifest
##'which contains the network name and the node name.
##'@param prefix A character scaler used in front of numeric names to make
##'legal Netica names. (See \code{\link[RNetica]{as.IDname}}).
##'@return
##'
##'An object of class \code{\linkS4class{PomNodeWarehouse}}.
##'@author Russell Almond
##'@seealso
##'
##'\code{\link[Peanut]{Warehouse}} for the general warehouse protocol.
##'@keywords manip graph
##'@examples
##'
##'
##'sess <- PomSession()
##'startSession(sess)
##'
##'### This tests the manifest and factory protocols.
##'
##'nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
##'                     row.names=1,stringsAsFactors=FALSE)
##'
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'
##'
##'### Test Net building
##'Nethouse <- PomNetWarehouse(manifest=netman1,session=sess,key="Name")
##'stopifnot(is.PnetWarehouse(Nethouse))
##'
##'setwd(paste(library(help="PPomegranite")$path, "testnets",sep=.Platform$file.sep))
##'CM <- WarehouseSupply(Nethouse,"miniPP_CM")
##'stopifnot(is.null(WarehouseFetch(Nethouse,"PPcompEM")))
##'EM1 <- WarehouseMake(Nethouse,"PPcompEM")
##'
##'EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM", "PPdurAttEM"),
##'              function(nm) WarehouseSupply(Nethouse,nm))
##'
##'### Test Node Building with already loaded nets
##'
##'Nodehouse <- PomNodeWarehouse(manifest=nodeman1,
##'                         key=c("Model","NodeName"),
##'                         session=sess)
##'stopifnot(is.PnodeWarehouse(Nodehouse))
##'
##'phyd <- WarehouseData(Nodehouse,c("miniPP_CM","Physics"))
##'
##'p3 <- MakePnode.PomNode(CM,"Physics",phyd)
##'
##'phys <- WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))
##'stopifnot(p3==phys)
##'
##'for (n in 1:nrow(nodeman1)) {
##'  name <- as.character(nodeman1[n,c("Model","NodeName")])
##'  if (is.null(WarehouseFetch(Nodehouse,name))) {
##'    cat("Building Node ",paste(name,collapse="::"),"\n")
##'    WarehouseSupply(Nodehouse,name)
##'  }
##'}
##'
##'WarehouseFree(Nethouse,PnetName(EM1))
##'stopifnot(!is.valid(Nethouse,EM1))
##'
##'
##'@export PomNodeWarehouse
PomNodeWarehouse <- function(manifest=data.frame(),session=getDefaultSession(),
                        key=c("Model","NodeName"),prefix="V")
              stop("Function Not Implemented")
  new("PomNodeWarehouse",manifest=manifest, session=session,
      key=key, prefix=prefix)

setIs("PomNodeWarehouse","PnodeWarehouse")

setMethod(ClearWarehouse,"PomNodeWarehouse",
          function(warehouse) {
              stop("Function Not Implemented")
            warning("To clear warehouse, delete and recreate network.")
          })


setMethod(WarehouseManifest,"PomNodeWarehouse",
          function(warehouse) {warehouse@manifest})
setMethod("WarehouseManifest<-",c("PomNodeWarehouse","data.frame"),
          function(warehouse,value) {
            for (ky in warehouse@key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<-value;
            warehouse})

setMethod(WarehouseData,"PomNodeWarehouse",
          function(warehouse,name) {
            key <- warehouse@key
            if (length(name) != length(key))
              stop("Expected name to contain elements",key)
            manifest <- warehouse@manifest
            whch = rep(TRUE,nrow(manifest))
            for (i in 1:length(key)) {
              whch <- whch & manifest[[key[i]]] == name[i]
            }
            manifest[whch,,drop=FALSE]
          })

setMethod(WarehouseFetch,"PomNodeWarehouse",
          function(warehouse,name) {
              stop("Function Not Implemented")
            if (length(name) != 2L)
              stop("Expected key to look like (net, node).")
            sess <- warehouse@session
            sess$nets[[as.IDname(name[1])]]$nodes[[as.IDname(name[2])]]
          })

setMethod(WarehouseMake,"PomNodeWarehouse",
          function(warehouse,name) {
              stop("Function Not Implemented")
            if (length(name) != 2L)
              stop("Expected name to be of the form (net,node).")
            net <- warehouse@session$nets[[as.IDname(name[1])]]
            if (is.null(net)) {
              stop("Network ",name[1]," does not exist.")
            }
            if (!is.null(net$nodes[[as.IDname(name[2])]])) {
              warning("Deleting old node ",paste(name,collapse="::"))
              DeleteNodes(net$nodes[[as.IDname(name[2])]])
            }
            dat <- WarehouseData(warehouse,name)
            MakePnode.PomNode(net,name[2],dat)
          })


setMethod(WarehouseFree,"PomNodeWarehouse",
          function(warehouse,name) {
              stop("Function Not Implemented")
            node <- WarehouseFetch(warehouse,name)
            if (is.null(node)) {
              flog.trace("Node for name %s not found, skipping.",name)
            } else {
              if (is.active(node))
                DeleteNodes(node)
            }
          })

setMethod(WarehouseSave,"PomNodeWarehouse",
          function(warehouse,obj) {
              stop("Function Not Implemented")
          })   #Null Action.


setMethod(WarehouseCopy,c("PomNodeWarehouse","PomNode"),
          function(warehouse,obj,newname) {
            newname <- as.legal.name(warehouse,newname)
            if (length(newname) != 2L)
              stop("Expected key to look like (net, node).")
            newnet <- warehouse@session$nets[[newname[1]]]
            if (is.null(newnet))
              stop("Network ",newname[1]," does not exist.")
            CopyNodes(obj,newname[2],newnet=newnet)
          })

setMethod(is.legal.name,"PomNodeWarehouse",
          function(warehouse,name)
              stop("Function Not Implemented")
            is.IDname(name)
          )

setMethod(as.legal.name,"PomNodeWarehouse",
          function(warehouse,name)
              stop("Function Not Implemented")
            as.IDname(name,warehouse@prefix)
          )

setMethod(is.valid,"PomNodeWarehouse",
          function(warehouse,object)
              stop("Function Not Implemented")
            is.active(object)
          )

setMethod(is.PnodeWarehouse,"PomNodeWarehouse",
          function(obj) {TRUE})

setMethod(WarehouseInventory,"PomNodeWarehouse",
          function(warehouse) {
              stop("Function Not Implemented")
            allKeys <- warehouse@manifest[,warehouse@key,drop=FALSE]
            built <- sapply(1L:nrow(allKeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allKeys[built, ,drop=FALSE]})
setMethod("WarehouseSupply", c("PomNodeWarehouse"), function(warehouse,name) {
              stop("Function Not Implemented")
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  if (!is.active(val)) {
    warehouse@session$nets[[as.IDname(name[1])]]$nodes[[as.IDname(name[2])]] <- NULL
    val <- WarehouseMake(warehouse,name)
  }
  val
})









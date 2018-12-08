#' Entropy computation between documents or features
#' 
#' Calculate entropy for a dfm or fcm object, for the documents or features margins.
#' @param x an input \link{dfm} object
#' @param selection a subset of \link{dfm} on which entropy is supposed to be computed
#' @param margin a character indicating on which margin to compress a dfm, either "documents" or "features"
#' @param method a character defining the entropy measure to be calculated
#' @param unit a character vector defining the unit in which entropy is measured. The default is "nats" (natural units). For computing entropy in "bits" set unit="log2".
#' @export
textstat_entropy <- function(x, selection = NULL, margin = c("documents", "features"),
                             method = c("ml", "mm", "jeffreys", "laplace", "sg", "minimax", "cs", "nsb", "shrink"),
                             unit = c("nat", "log", "log2", "log10")) {
    UseMethod("textstat_entropy")
}

#' @export
textstat_entropy.default <- function(x, selection = NULL, margin = c("documents", "features"),
                                    method = c("ml", "mm", "jeffreys", "laplace", "sg", "minimax", "cs", "nsb", "shrink"),
                                    unit = c("nat", "log", "log2", "log10")) {
    stop(friendly_class_undefined_message(class(x), "textstat_entropy"))
}

#' @export
textstat_entropy.dfm <- function(x, selection = NULL, margin = c("documents", "features"),
                                     method = c("ml", "mm", "jeffreys", "laplace", "sg", "minimax", "cs", "nsb", "shrink"),
                                     unit = c("nat", "log", "log2", "log10")) {
    if (!is.dfm(x)) stop("object x is not dfm")
    method <- match.arg(method)
    unit <- match.arg(unit)
    if ( (unit == "nat") | (unit == "log2") ) {
        base <- 2
        }
    else if (unit == "log") {
        base <- exp(1)
        }
    else if (unit == "log") {
        base <- 10
    }
    if (method == "ml") {
        prop <- t(x) %>%
            dfm_weight(scheme = "prop")
        entropy <- basic_entropy(prop, base = base)
    }
    if (method == "mm") {
        prop <- t(x) %>%
            dfm_weight(scheme = "prop")
        mm_correction <- apply(t(x), 1, function(y) {
            total_count <- sum(y)
            non_zeros_count <- sum(y > 0)
            ( (non_zeros_count - 1) / (2 * total_count) ) / log(base)
            })
        entropy <- basic_entropy(prop, base = base) + mm_correction
    }
    if (method == "jeffreys"){
        a <- 1 / 2
        adj_prop <- apply(t(x), 1, function(y) {
            ya <- y + a # Counts + pseudocounts
            na <- sum(ya) #Total Counts + Pseudo Counts
            ya / na #Adjusted frequencies
        })
        entropy <- basic_entropy(t(adj_prop), base = base)
    }
    if (method == "laplace"){
        a <- 1 #  Laplace Uniform Prior
        adj_prop <- apply(t(x), 1, function(y) {
            ya <- y + a # Counts + pseudocounts
            na <- sum(ya) #Total Counts + Pseudo Counts
            ya / na #Adjusted frequencies
        })
        entropy <- basic_entropy(t(adj_prop), base = base)
    }
    if (method == "sg"){
        a <- (1 / ndoc(x)) # a = 1 / p where p is the number of documents
        adj_prop <- apply(t(x), 1, function(y) {
            ya <- y + a # Counts + pseudocounts
            na <- sum(ya) #Total Counts + Pseudo Counts
            ya / na #Adjusted frequencies
        })
        entropy <- basic_entropy(t(adj_prop), base = base)
    }
    if (method == "minimax"){
        adj_prop <- apply(t(x), 1, function(y){
            a <- sqrt(sum(y)) / ndoc(x)
            # a = sqrt(n)/p where n is the number of counts per word & p is the number of documents
            ya <- y + a # Counts + pseudocounts
            na <- sum(ya) #Total Counts + Pseudo Counts
            ya / na #Adjusted frequencies
        })
        entropy <- basic_entropy(t(adj_prop), base = base)
    }
    
    if (method == 'cs'){
        entropy <- apply(t(x), 1, function(y) {
            nonzero = y[y>0] #Subset non-zero entires
            total = sum(nonzero) #Count total entries
            prop = nonzero/total #Count proportion of non-zero entries
            
            f1 = sum(nonzero == 1) # singletons - nfeatures with only one appearance in corpus
            if (f1 == total) f1 = total - 1
            
            C = 1 - f1/total # Estimate for sample coverage
            pa = C * prop
            la = (1-(1-pa)^total)
            H = -sum(pa*log(pa, base= base)/la)
        })
    }
    
    
    return(entropy)
}
# Helper Function for Basic Entropy on a Transposed DFM
# x has features as rows and documents as columns, 
# which is opposed to the quanteda::dfm object which has features as columns and documents as rows
basic_entropy <- function(x, base){
    apply(x, 1, function(y) {
        # remove zeros
        y <- subset(y, y != 0)
        # entropy formula
        -1 * sum(y * log(y, base = base))
    })
}

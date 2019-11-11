#'Translate any DNA or RNA sequence to IUPAC amino acid code
#'
#'@param sequence a text string containing a DNA or RNA sequence
#'@return A text string that represents the translated amino acid sequence
#'@examples
#'sequence <- "ATGCTGTAG"
#'AminA(sequence)
#'@export
AminA <- function(sequence){
  if(nchar(sequence)%%3 !=0){
    warning("DNA sequence isn't a multiple of 3. The function will assume that the first base
            is in position one\n")
  }
  table <- matrix(c("TTT","TTC","TTA","TTG","CTT","CTC","CTA","CTG","ATT","ATC","ATA","ATG",
                    "GTT","GTC","GTA","GTG","TCT","TCC","TCA","TCG","CCT","CCC","CCA","CCG",
                    "ACT","ACC","ACA","ACG","GCT","GCC","GCA","GCG","TAT","TAC","TAA","TAG",
                    "CAT","CAC","CAA","CAG","AAT","AAC","AAA","AAG","GAT","GAC","GAA","GAG",
                    "TGT","TGC","TGA","TGG","CGT","CGC","CGA","CGG","AGT","AGC","AGA","AGG",
                    "GGT","GGC","GGA","GGG","F","F","L","L","L","L","L","L","I","I","I","M",
                    "V","V","V","V","S","S","S","S","P","P","P","P","T","T","T","T","A","A",
                    "A","A","Y","Y","*","*","H","H","Q","Q","N","N","K","K","D","D","E","E",
                    "C","C","*","W","R","R","R","R","S","S","R","R","G","G","G","G"), 64, 2)
  dna <- toupper(sequence)
  dna <- gsub("U", "T", dna)
  codons <- list()
  num.codons <- nchar(dna)/3
  starts <- seq(from=1, by=3, length.out = num.codons)
  for(i in 1:num.codons){
    codons[i] <- substring(dna, starts[i], starts[i]+2)
  }
  result <- c()
  for(i in 1:length(codons)){
    result[i] <- table[table[,1] == codons[[i]] , 2]
  }
  return(paste(unlist(result), sep="", collapse=""))
}


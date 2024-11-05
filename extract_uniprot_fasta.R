#' Extract UniProt Information from FASTA File
#'
#' This function extracts key information (Accession ID, Protein Name, Gene Name, Organism, and Sequence)
#' from a FASTA file downloaded from UniProt. If any information is missing, it is returned as NA.
#'
#' @param fasta_file Path to the FASTA file (as a string).
#'
#' @return A data frame containing the following columns:
#'   - AccessionID: UniProt accession ID
#'   - ProteinName: Protein name
#'   - GeneName: Gene name
#'   - Organism: Organism name
#'   - Sequence: The protein sequence
#'
#' @examples
#' if (interactive()) {
#'   fasta_file <- "pub/Mus musculus_2023_05.fasta"  # Path to your FASTA file
#'   uniprot_data <- extract_uniprot_fasta(fasta_file)
#'   # Print the first few rows to verify
#'   head(uniprot_data)
#' }
extract_uniprot_fasta <- function(fasta_file) {
  
  # Step 1: Read the FASTA file
  fasta_lines <- readLines(fasta_file)
  
  # Step 2: Preallocate vectors to store extracted data
  n <- sum(startsWith(fasta_lines, ">"))  # Count the number of sequences (headers)
  accession_ids <- vector("character", n)
  protein_names <- vector("character", n)
  gene_names <- vector("character", n)
  organisms <- vector("character", n)
  fasta_seqs <- vector("character", n)
  
  # Step 3: Initialize index for storing results
  idx <- 1
  current_seq <- ""
  
  # Step 4: Loop through each line in the FASTA file
  for (line in fasta_lines) {
    
    if (startsWith(line, ">")) {  # Check if it's a header line
      # If it's not the first header, save the previous sequence
      if (current_seq != "") {
        fasta_seqs[idx - 1] <- current_seq
        current_seq <- ""  # Reset for the next sequence
      }
      
      # Step 5: Extract information from header line using regex
      header <- line
      
      # Extract Accession ID (before the first "|")
      accession_id <- sub(".*\\|(\\w+)\\|.*", "\\1", header)
      accession_ids[idx] <- accession_id
      
      # Extract Protein Name (between the first space and " OS=")
      protein_name <- sub(".*\\|[^|]+\\|[^ ]+ (.+?) OS=.*", "\\1", header)
      protein_names[idx] <- protein_name
      
      # Extract Gene Name (if available, after " GN=")
      gene_name <- ifelse(grepl(" GN=", header), sub(".* GN=([^ ]+).*", "\\1", header), NA)
      gene_names[idx] <- gene_name
      
      # Extract Organism (after " OS=")
      organism <- ifelse(grepl(" OS=", header), sub(".* OS=([^ ]+ [^ ]+).*", "\\1", header), NA)
      organisms[idx] <- organism
      
      # Increment the index for the next sequence
      idx <- idx + 1
    } else {
      # Step 6: Accumulate the sequence lines
      current_seq <- paste0(current_seq, line)
    }
  }
  
  # Step 7: Append the last sequence after the loop
  if (current_seq != "") {
    fasta_seqs[idx - 1] <- current_seq
  }
  
  # Step 8: Create a data frame with extracted information
  result <- data.frame(
    AccessionID = accession_ids,
    ProteinName = protein_names,
    GeneName = gene_names,
    Organism = organisms,
    Sequence = fasta_seqs,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

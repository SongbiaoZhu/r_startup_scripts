#' Calculate the m/z value for a given peptide sequence and charge
#'
#' This function calculates the mass-to-charge ratio (m/z) for a peptide sequence, taking 
#' into account the modifications and the charge state. It also calculates the total mass 
#' of the peptide, including any modifications and protons added due to the charge.
#'
#' @param peptide_seq A string representing the peptide sequence, which may include 
#'                    modifications in the format `(UniMod:<modification_id>)`. 
#'                    For example, "AC(UniMod:4)EFAGFQC(UniMod:4)QIQFGPHNEQK".
#' @param charge An integer representing the charge state of the peptide. For example, 
#'              a charge of 2 corresponds to two protons added to the peptide.
#' 
#' @return A list containing:
#'   - `mz`: The mass-to-charge ratio (m/z) of the peptide.
#'   - `peptide_mass`: The total mass of the peptide (in Da).
#'   - `charge`: The charge state used in the calculation.
#'

calculate_mz <- function(peptide_seq, charge) {
  
  # Step 1: Define the masses of amino acids
  # These are the standard masses for each amino acid in Daltons (Da).
  amino_acid_masses <- c(
    A = 71.03711,  # Alanine
    R = 156.10111, # Arginine
    N = 114.04293, # Asparagine
    D = 115.02694, # Aspartic acid
    C = 103.00919, # Cysteine
    Q = 128.05858, # Glutamine
    E = 129.04259, # Glutamic acid
    G = 57.02146,  # Glycine
    H = 137.05891, # Histidine
    I = 113.08406, # Isoleucine
    L = 113.08406, # Leucine
    K = 128.09496, # Lysine
    M = 131.04049, # Methionine
    F = 147.06841, # Phenylalanine
    P = 97.05276,  # Proline
    S = 87.03203,  # Serine
    T = 101.04768, # Threonine
    W = 186.07931, # Tryptophan
    Y = 163.06333, # Tyrosine
    V = 99.06841   # Valine
  )
  
  # Step 2: Define modification masses
  # These are predefined modification masses in Daltons. Each modification corresponds 
  # to a change in mass associated with a specific modification ID.
  # Example modifications are (UniMod:4), (UniMod:1), and (UniMod:35), each adding 
  # specific amounts to the mass of the peptide.
  modification_masses <- list(
    `4` = 57.021,   # (UniMod:4) - typically the mass for Carbamidomethylation
    `1` = 42.0106,  # (UniMod:1) - typically the mass for Acetylation
    `35` = 15.9949,  # (UniMod:35) - typically the mass for Oxidation
    `21` = 79.9663  # (UniMod:21) - typically the mass for Phosphorylation
  )
  
  # Step 3: Parse the peptide sequence and calculate the base mass
  # The sequence is split into individual characters (amino acids), while modifications
  # (inside parentheses, e.g., (UniMod:4)) are extracted separately and stored.
  
  peptide_base_mass <- 0          # Initialize the base mass of the peptide
  modifications <- list()         # List to hold modification IDs
  
  # Replace the modification parts in the peptide sequence with UniMod IDs
  peptide_seq_split <- gsub("\\(UniMod:(\\d+)\\)", " \\1", peptide_seq)  # Match and replace (UniMod:ID) with space and ID
  peptide_chars <- unlist(strsplit(peptide_seq_split, ""))  # Split the sequence into individual characters
  
  # Process each amino acid in the peptide sequence
  for (i in seq_along(peptide_chars)) {
    char <- peptide_chars[i]  # Current character (amino acid or modification ID)
    
    # If the character is an amino acid, add its mass to the peptide's base mass
    if (char %in% names(amino_acid_masses)) {
      peptide_base_mass <- peptide_base_mass + amino_acid_masses[char]
    } else if (grepl("^\\d+$", char)) {
      # If the character is a modification ID, add it to the list of modifications
      mod_id <- char
      modifications <- c(modifications, mod_id)
    }
  }
  
  # Step 4: Add mass changes due to modifications
  # For each modification, its corresponding mass change (from the `modification_masses` list)
  # is added to the total modification mass.
  
  mod_mass <- 0  # Initialize the modification mass
  
  # Loop through the modifications and add their respective mass changes
  for (mod in modifications) {
    if (mod %in% names(modification_masses)) {
      mod_mass <- mod_mass + modification_masses[[mod]]
    }
  }
  
  # Step 5: Add the mass of protons (H⁺ ions) for the charge state
  # Each proton (H⁺) adds a mass of approximately 1.0078 Da. For a peptide with charge `charge`,
  # the total mass of protons added is calculated by multiplying the charge by the proton mass.
  
  proton_mass <- charge * 1.0078  # Mass of protons (H⁺), considering the charge state
  
  # Step 6: Calculate the total mass of the peptide
  # The total mass includes the base mass of the peptide, any modifications, and the protons added.
  
  total_mass <- peptide_base_mass + mod_mass + proton_mass
  
  # Step 7: Calculate the mass-to-charge ratio (m/z)
  # The m/z value is calculated by dividing the total mass of the peptide by its charge.
  
  mz <- total_mass / charge  # Calculate m/z
  
  # Step 8: Return the results
  # The function returns a list containing:
  # - `mz`: the calculated m/z value
  # - `peptide_mass`: the total mass of the peptide (in Da)
  # - `charge`: the charge state used in the calculation
  
  return(list(
    mz = mz,                # Mass-to-charge ratio
    peptide_mass = total_mass,  # Total mass of the peptide (Da)
    charge = charge         # Charge state used
  ))
}

#' @examples
#' Uncomment the following lines to test the function
#' peptide_sequence <- "AC(UniMod:4)EFAGFQC(UniMod:4)QIQFGPHNEQK"
#' charge_state <- 2
#' result <- calculate_mz(peptide_sequence, charge_state)
#' print(result)


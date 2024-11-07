# load functions and packages ----------------------------------------------
source("https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/startup.R")
source_git(script_names = c("make_r_friendly_names.R",
                            "write_excel.R",
                            "extract_uniprot_fasta.R"))

library(purrr)
library(ggsci)
library(glue)


fns <- list.files(data_path)
i <- 1
fn <- fns[i]

dfs <- lapply(fns, function(x) readxl::read_excel(file.path(data_path, x)) %>% 
                make_r_friendly_names())
names(dfs) <- stringr::str_replace(stringr::str_replace(fns, "MSF240039_zzk_phospho_quan_normal_modsites_", ""), "\\.xlsx", "")


purrr::map(dfs, colnames)
fn_pep <- "MSF240039_zzk_phospho_quan_normal_modsites_PeptideGroups.xlsx"
df_pep <- readxl::read_excel(file.path(data_path, fn_pep)) %>% 
  make_r_friendly_names() %>% 
  # remove Protein_Accessions, save Master_Protein_Accessions
  dplyr::select(Sequence,
                Master_Protein_Accessions,
                Positions_in_Master_Proteins,
                dplyr::starts_with("Abundance_Ratio"),
                dplyr::starts_with("Abundance_Ratio_log2"),
                dplyr::starts_with("Abundances_Normalized"),
                dplyr::matches("Abundance_F\\d+"),
                Theo_MH_Da,
                PSMs,
                q_Value,
                XCorr_by_Search_Engine_Sequest_HT,
                Top_Apex_RT_min) %>% 
  # Master_Protein_Accessions, only save the first before ;
  dplyr::mutate(Master_Protein_Accessions = ifelse(grepl(";", Master_Protein_Accessions), 
                                                   sub("([;].*)", "", Master_Protein_Accessions), 
                                                   Master_Protein_Accessions)) %>%
  # Positions_in_Master_Proteins, only save the first before ;
  dplyr::mutate(Positions_in_Master_Proteins = ifelse(grepl(";", Positions_in_Master_Proteins), 
                                                      sub("([;].*)", "", Positions_in_Master_Proteins), 
                                                      Positions_in_Master_Proteins)) %>%
  # remove _F1_n_a_Sample_ in Abundances_Normalized columns
  dplyr::rename_with(~ gsub("Abundances_Normalized_F\\d+_n_a_Sample_", "Abundances_Normalized_", .),
                     starts_with("Abundances_Normalized_")) %>% 
  # remove _F1_n_a_Sample_ in Abundance columns
  dplyr::rename_with(~ gsub("Abundance_F\\d+_n_a_Sample_", "Abundance_", .),
                     starts_with("Abundance_F")) 


fn_pro <- "MSF240039_zzk_phospho_quan_normal_modsites_Proteins.xlsx"
df_pro <- readxl::read_excel(file.path(data_path, fn_pro)) %>% 
  make_r_friendly_names() %>% 
  # remove Protein_Accessions, save Master_Protein_Accessions
  dplyr::select(Accession,
                Gene_Symbol,
                Description,
                Coverage,
                Peptides,
                PSMs,
                Unique_Peptides,
                dplyr::starts_with("Abundance_Ratio"),
                dplyr::starts_with("Abundance_Ratio_log2"),
                dplyr::starts_with("Abundances_Normalized"),
                dplyr::matches("Abundance_F\\d+")) %>% 
  # remove _F1_n_a_Sample_ in Abundances_Normalized columns
  dplyr::rename_with(~ gsub("Abundances_Normalized_F\\d+_n_a_Sample_", "Abundances_Normalized_", .),
                     starts_with("Abundances_Normalized_")) %>% 
  # remove _F1_n_a_Sample_ in Abundance columns
  dplyr::rename_with(~ gsub("Abundance_F\\d+_n_a_Sample_", "Abundance_", .),
                     starts_with("Abundance_F")) 

fn_sites <- "MSF240039_zzk_phospho_quan_normal_modsites_ModificationSites.xlsx"
df_sites <- readxl::read_excel(file.path(data_path, fn_sites)) %>% 
  make_r_friendly_names() %>% 
  # remove Protein_Accessions, save Master_Protein_Accessions
  dplyr::select(Modification_Name,
                Target_Amino_Acid,
                Position_in_Peptide,
                Peptide_Sequence,
                Protein_Accession,
                Protein_Description,
                Position,
                Motif
  ) 


# Export tables -----------------------------------------------------------

write_excel(data = list(df_pep,
                        df_pro,
                        df_sites),
            file_path = file.path(res_path, "output.xlsx"),
            sheet_names = c(glue::glue("Phospho_peptides ({nrow(df_pep)})"),
                            glue::glue("Phospho_proteins ({nrow(df_pro)})"),
                            glue::glue("Phospho_sites ({nrow(df_sites)})"))  )

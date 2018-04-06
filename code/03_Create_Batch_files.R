library(XML)

# Create Batch file -------------------------------------------------------


PIPR_files <- list.files(path="D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Inputs",
                         pattern = "PIPR",   full.names = T)

PIPA_files <- list.files(path="D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Inputs",
                         pattern = "PIPA",   full.names = T)

PIHA_files <- list.files(path="D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Inputs",
                         pattern = "PIHA",   full.names = T)

PINI_files <- list.files(path="D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Inputs",
                         pattern = "PINI",   full.names = T)

PISY_files <- list.files(path="D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Inputs",
                         pattern = "PISY",   full.names = T)

PIUN_files <- list.files(path="D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Inputs",
                         pattern = "PIUN",   full.names = T)


create_batch <- function (x, nreps, name) {
      batchFile <- newXMLNode('batchFile', attrs = c (fileCode="06010401"))
      for (i in 1:length(x)) {
            ba_parFile <- newXMLNode('ba_parFile', parent = batchFile)
            newXMLNode('ba_fileName', x[i], parent = ba_parFile)
            newXMLNode('ba_numTimesToRun', nreps, parent = ba_parFile) 
      }
      
      saveXML(xmlDoc(batchFile, addFinalizer = TRUE),
              paste0('D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/Batch/',name, '.xml'))
      
}

create_batch(PIPR_files, 1, "PIPR")
create_batch(PIPA_files, 1, "PIPA")
create_batch(PIHA_files, 1, "PIHA")
create_batch(PINI_files, 1, "PINI")
create_batch(PISY_files, 1, "PISY")
create_batch(PIUN_files, 1, "PIUN")


# system("\Program Files (x86)\SORTIE\bin\coremodel.exe" SORTIE_files\Batch\Batch_Plots_pn_1.xml)





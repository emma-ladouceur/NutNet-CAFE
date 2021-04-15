##### NUTNET TAXONOMIC ADJUSTMENTS
## Based on analyses from 2017 NutNet workshop and previous efforts
## JD Bakker et al
## 200924

## Separate taxonomic adjustments for each site to increase temporal consistency in naming

## Based on data in 'full-cover-24-Sept-2020.csv'
## Includes all sites with >= 4 years of data


Taxonomic.Adjustments <- function(datafile = datafile) {
  
  data1 <- datafile
  
  # Drop non-living taxa
  # Confirmed with Glenda Wardle that 'OTHER ARISTIDA CONTORTA (DEAD)' was live in this growing season so need to avoid dropping it
  data1[data1$site_code %in% c("ethamc.au", "ethass.au") & data1$Taxon == "OTHER ARISTIDA CONTORTA (DEAD)", "live"] <- 1
  data1[data1$site_code %in% c("ethamc.au", "ethass.au") & data1$Taxon == "OTHER ARISTIDA CONTORTA (DEAD)", "Family"] <- "Poaceae"
  data1 <- data1[data1$live == 1,]
  
  # Drop mosses, lichens, fungi
  data1 <- data1[! data1$functional_group %in% c("BRYOPHYTE", "LICHEN", "CLUBMOSS", "LIVERWORT", "NON-LIVE") , ]
  #some families not consistently identified to functional group
  data1 <- data1[! data1$Family %in% c("Dicranaceae", "Lycopodiaceae", "Phallales", "Pottiaceae",
                                       "Selaginellaceae", "Thuidiaceae", "MNIACEAE", "Polytrichaceae") , ]
  
  # Drop records not assigned to family
  data1 <- data1[! data1$Family %in% c("NULL") , ]  # 206642 x 18
  
  # Site-specific taxonomic adjustments

  # ahth.is
  Site = "ahth.is"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("FESTUCA RICHARDSONII", "FESTUCA VIVIPARA")] <- "FESTUCA SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # amlr.is
  Site = "amlr.is"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("AGROSTIS CAPILLARIS", "AGROSTIS VINEALIS")] <- "AGROSTIS SP."
  temp$Taxon[temp$Taxon %in% c("FESTUCA RICHARDSONII", "FESTUCA VIVIPARA", "FESTUCA RUBRA")] <- "FESTUCA SP."
  temp$Taxon[temp$Taxon %in% c("RUMEX ACETOSA", "RUMEX ACETOSELLA")] <- "RUMEX SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # arch.us
  Site = "arch.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CARPHEPHORUS CARPHEPHORUS")] <- "CARPHEPHORUS SP."
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO FISTULOSA")] <- "SOLIDAGO SP."
  temp$Taxon[temp$Taxon %in% c("CYPERUS SP.")] <- "CYPERUS RETRORSUS"
  temp$Taxon[temp$Taxon %in% c("RHYNCHOSPORA FASCICULARIS", "RHYNCHOSPORA FILIFOLIA")] <- "RHYNCHOSPORA SP."
  temp$Taxon[temp$Taxon %in% c("SCLERIA PAUCIFLORA", "SCLERIA RETICULARIS")] <- "SCLERIA SP."
  temp$Taxon[temp$Taxon %in% c("LUDWIGIA ALATA", "LUDWIGIA PILOSA", "LUDWIGIA SUFFRUTICOSA")] <- "LUDWIGIA SP."
  temp$Taxon[temp$Taxon %in% c("ANDROPOGON SP.", "ANDROPOGON VIRGINICUS var. GLAUCUS",
                               "ANDROPOGON VIRGINICUS var. VIRGINICUS")] <- "ANDROPOGON VIRGINICUS"
  temp$Taxon[temp$Taxon %in% c("AXONOPUS FURCATUS")] <- "AXONOPUS SP."
  temp$Taxon[temp$Taxon %in% c("PANICUM AGROSTOIDES", "PANICUM HEMITOMON")] <- "PANICUM SP."
  temp <- temp[temp$Taxon != "UNKNOWN GRASS SP." , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # azi.cn
  #TIBETIA == GUELDENSTAEDTIA VERNA??
  Site = "azi.cn"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("SAUSSUREA NEOFRANCHETII", "SAUSSUREA NIGRESCENS", "SAUSSUREA PACHYNEURA",
                               "SAUSSUREA STELLA")] <- "SAUSSUREA SP."
  temp$Taxon[temp$Taxon %in% c("POA ATTENUATA var. KENG")] <- "POA ATTENUATA"
  temp$Taxon[temp$Taxon %in% c("VIOLA STRIATELLA")] <- "VIOLA SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # badlau.de
  Site = "badlau.de"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("VICIA SATIVA SSP. NIGRA")] <- "VICIA SATIVA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # barta.us - no changes
  
  # bldr.us
  Site = "bldr.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("PODOSPERMUM LACINIATUM", "PODOSPERMUM SCORZONEROIDES", "PODOSPERMUM SP.")] <- "SCORZONERA LACINIATA"
  temp$Taxon[temp$Taxon %in% c("MEDICAGO SP.")] <- "MEDICAGO LUPULINA"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN GRASS")] <- "VULPIA BROMOIDES"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # bnch.us
  # no Agrostis or Danthonia in year_trt = 3??
  Site = "bnch.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ASTER SP.")] <- "SYMPHYOTRICHUM SPATHULATUM"
  temp$Taxon[temp$Taxon %in% c("CAREX HOODII")] <- "CAREX PENSYLVANICA"
  temp$Taxon[temp$Taxon %in% c("ELYMUS GLAUCUS")] <- "ELYMUS REPENS"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN POLEMONIACEAE SP.")] <- "MICROSTERIS GRACILIS"
  temp <- temp[temp$Taxon != "UNKNOWN GRASS" , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # bogong.au
  Site = "bogong.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ERIGERON BELLIDIOIDES", "ERIGERON NITIDUS")] <- "ERIGERON SP."
  temp$Taxon[temp$Taxon %in% c("MICROSERIS SP.")] <- "MICROSERIS LANCEOLATA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # burrawan.au - species list confirmed with J. Firn on 170324.
  
  # burren.ie
  Site = "burren.ie"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("DACTYLORHIZA FUCHSII", "DACTYLORHIZA MACULATA")] <- "DACTYLORHIZA SP."
  temp$Taxon[temp$Taxon %in% c("ROSA XANTHINA")] <- "ROSA SPINOSISSIMA"
  temp <- temp[temp$Taxon != "UNKNOWN ORCHIDACEAE SP." , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cbgb.us
  Site = "cbgb.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CIRSIUM ARVENSE", "CIRSIUM VULGARE")] <- "CIRSIUM SP."
  temp$Taxon[temp$Taxon %in% c("HELIANTHUS SP.")] <- "HELIANTHUS GROSSESERRATUS"
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO SP.")] <- "SOLIDAGO CANADENSIS"
  temp$Taxon[temp$Taxon %in% c("SYMPHYOTRICHUM SP.")] <- "SYMPHYOTRICHUM PILOSUM"
  temp$Taxon[temp$Taxon %in% c("GALIUM SP.")] <- "GALIUM CONCINNUM"
  temp$Taxon[temp$Taxon %in% c("SOLANUM SP.")] <- "SOLANUM CAROLINENSE"
  temp <- temp[temp$Taxon != "UNKNOWN GRASS" , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cdcr.us
  Site = "cdcr.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CHENOPODIUM SP.")] <- "CHENOPODIUM ALBUM"
  temp$Taxon[temp$Taxon %in% c("ASCLEPIAS SP.")] <- "ASCLEPIAS SYRIACA"
  temp$Taxon[temp$Taxon %in% c("ASTER SP.")] <- "SYMPHYOTRICHUM BOREALE"
  temp$Taxon[temp$Taxon %in% c("ERIGERON CANADENSIS")] <- "CONYZA CANADENSIS"
  temp$Taxon[temp$Taxon %in% c("ERIGERON SP.")] <- "ERIGERON STRIGOSUS"
  temp$Taxon[temp$Taxon %in% c("RUDBECKIA SP.", "RUDBECKIA HIRTA var. PULCHERRIMA")] <- "RUDBECKIA HIRTA"
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO SP.")] <- "SOLIDAGO MISSOURIENSIS"
  temp$Taxon[temp$Taxon %in% c("CAREX SCOPARIA")] <- "CAREX SP."
  temp$Taxon[temp$Taxon %in% c("CYPERUS FILICULMIS", "CYPERUS GRAYI", "CYPERUS LUPULINUS", "CYPERUS SCHWEINITZII")] <- "CYPERUS SP."
  temp$Taxon[temp$Taxon %in% c("UNKNOWN LAMIACEAE ")] <- "HEDEOMA HISPIDA"
  temp$Taxon[temp$Taxon %in% c("PANICUM SP.")] <- "PANICUM ACUMINATUM"
  temp <- temp[temp$Taxon != "UNKNOWN FABACEAE" , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cdpt.us - no changes
  
  # cereep.fr
  Site = "cereep.fr"
  # why so much Hypochaeris radicata in year 0?
  # why minimum value of 2.5 in year 0?
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("MYOSOTIS ARVENSIS", "MYOSOTIS RAMOSISSIMA")] <- "MYOSOTIS SP."
  temp$Taxon[temp$Taxon %in% c("SILENE LATIFOLIA")] <- "SILENE LATIFOLIA SSP. ALBA"
  temp$Taxon[temp$Taxon %in% c("CREPIS SETOSA")] <- "CREPIS SP."
  temp$Taxon[temp$Taxon %in% c("JACOBAEA VULGARIS")] <- "SENECIO JACOBAEA"
  temp$Taxon[temp$Taxon %in% c("PICRIS SP.")] <- "PICRIS HIERACIOIDES"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM ARVENSE", "TRIFOLIUM CAMPESTRE", "TRIFOLIUM DUBIUM", "TRIFOLIUM PRATENSE",
                               "TRIFOLIUM REPENS")] <- "TRIFOLIUM SP."
  temp$Taxon[temp$Taxon %in% c("VICIA sativa SSP. nigra")] <- "VICIA SATIVA"
  temp$Taxon[temp$Taxon %in% c("VICIA CRACCA", "VICIA LATHYROIDES", "VICIA SEPIUM", "VICIA TETRASPERMA")] <- "VICIA SP."
  temp$Taxon[temp$Taxon %in% c("ERODIUM SP.")] <- "ERODIUM CICUTARIUM"
  temp$Taxon[temp$Taxon %in% c("GERANIUM DISSECTUM", "GERANIUM MOLLE", "GERANIUM PUSILLUM", "GERANIUM PYRENAICUM",
                               "GERANIUM ROTUNDIFOLIUM")] <- "GERANIUM SP."
  temp$Taxon[temp$Taxon %in% c("HYPERICUM MACULATUM SSP. OBTUSIUSCULUM", "HYPERICUM PERFORATUM")] <- "HYPERICUM SP."
  temp$Taxon[temp$Taxon %in% c("AGROSTIS CANINA", "AGROSTIS GIGANTEA")] <- "AGROSTIS SP."
  temp$Taxon[temp$Taxon %in% c("ARRHENATHERUM elatius SSP. bulbosum")] <- "ARRHENATHERUM ELATIUS"
  temp$Taxon[temp$Taxon %in% c("BROMUS SP.")] <- "BROMUS HORDEACEUS"
  temp$Taxon[temp$Taxon %in% c("FESTUCA SP.")] <- "FESTUCA RUBRA"
  temp$Taxon[temp$Taxon %in% c("POA pratensis SSP. latifolia")] <- "POA PRATENSIS"
  temp$Taxon[temp$Taxon %in% c("POA SP.")] <- "POA TRIVIALIS"
  temp$Taxon[temp$Taxon %in% c("RUMEX SP.")] <- "RUMEX ACETOSELLA"
  temp$Taxon[temp$Taxon %in% c("RANUNCULUS SP.")] <- "RANUNCULUS REPENS"
  temp$Taxon[temp$Taxon %in% c("VALERIANELLA SP.")] <- "VALERIANELLA LOCUSTA"
  temp <- temp[temp$Taxon != "UNKNOWN CARYOPHYLLACEAE SP." , ]
  temp <- temp[temp$Taxon != "UNKNOWN ASTERACEAE SP." , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # chilcas.ar
  Site = "chilcas.ar"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CAREX SP.")] <- "CAREX PHALAROIDES"
  temp$Taxon[temp$Taxon %in% c("CYPERUS SP.")] <- "CYPERUS ERAGROSTIS"
  temp$Taxon[temp$Taxon %in% c("BROMUS AULETICUS")] <- "BROMUS BRACHYANTHERUS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # comp.pt
  # plot 14 unusual: Asteraceae only in 2012
  Site = "comp.pt"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CREPIS CAPILLARIS", "CREPIS VESICARIA")] <- "CREPIS SP."
  temp$Taxon[temp$Taxon %in% c("HYPOCHAERIS", "LEONTODON TARAXACOIDES")] <- "HYPOCHAERIS GLABRA"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM GLOMERATUM", "TRIFOLIUM GEMELLUM", "TRIFOLIUM LIGUSTICUM")] <- "TRIFOLIUM SP."
  temp$Taxon[temp$Taxon %in% c("ERODIUM AETHIOPICUM", "ERODIUM BOTRYS", "ERODIUM CICUTARIUM")] <- "ERODIUM SP."
  temp$Taxon[temp$Taxon %in% c("OROBANCHE SP.")] <- "OROBANCHE MINOR"
  temp$Taxon[temp$Taxon %in% c("PLANTAGO BELLARDI")] <- "PLANTAGO BELLARDII"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN GRASS")] <- "AGROSTIS POURRETII"
  temp$Taxon[temp$Taxon %in% c("VULPIA")] <- "VULPIA BROMOIDES"
  temp <- temp[! temp$Taxon %in% c("UNKNOWN ASTERACEAE SP.", "UNKNOWN ASTERACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cowi.ca - no changes
  # Bellis perennis == Leucanthemum vulgare??
  
  # doane.us
  Site = "doane.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO SP.")] <- "SOLIDAGO CANADENSIS"
  temp$Taxon[temp$Taxon %in% c("PHYSALIS VIRGINIANA")] <- "PHYSALIS LONGIFOLIA"
  temp <- temp[! temp$Taxon %in% c("UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # elliot.us
  Site = "elliot.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CROTON SETIGER")] <- "CROTON SETIGERUS"
  temp$Taxon[temp$Taxon %in% c("JUNCUS SP.")] <- "JUNCUS DUBIUS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # ethamc.au
  Site = "ethamc.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ARISTIDA CONTORTA", "OTHER ARISTIDA CONTORTA (DEAD)")] <- "ARISTIDA HOLATHERA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)

    # ethass.au
  Site = "ethass.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("PORTULACA SP.")] <- "PORTULACA INTRATERRANEA"
  temp$Taxon[temp$Taxon %in% c("ARISTIDA CONTORTA", "OTHER ARISTIDA CONTORTA (DEAD)")] <- "ARISTIDA HOLATHERA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # frue.ch - no changes
  
  # gilb.za - no changes

  # hall.us
  Site = "hall.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("RUBUS SP.")] <- "RUBUS ALLEGHENIENSIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # hart.us - UNKNOWN = MICROSTERIS?
  Site = "hart.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ALLIUM SP.")] <- "ALLIUM ACUMINATUM"
  temp$Taxon[temp$Taxon %in% c("AGOSERIS SP.")] <- "AGOSERIS GLAUCA"
  temp$Taxon[temp$Taxon %in% c("CREPIS OCCIDENTALIS")] <- "CREPIS SP."
  temp$Taxon[temp$Taxon %in% c("ASTRAGALUS SP.")] <- "ASTRAGALUS FILIPES"
  temp$Taxon[temp$Taxon %in% c("LUPINUS SP.")] <- "LUPINUS UNCIALIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # hero.uk - no changes
  
  # hnvr.us
  Site = "hnvr.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("IPOMOEA SP.")] <- "CALYSTEGIA SEPIUM"
  temp$Taxon[temp$Taxon %in% c("CAREX SP.")] <- "CAREX GRACILLIMA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # hopl.us
  Site = "hopl.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("UNKNOWN APIACEAE SP.", "DAUCUS SP.")] <- "DAUCUS PUSILLUS"
  temp$Taxon[temp$Taxon %in% c("SANICULA SP.")] <- "SANICULA BIPINNATA"
  temp$Taxon[temp$Taxon %in% c("TORILIS ARVENSIS", "TORILIS NODOSA")] <- "TORILIS SP."
  temp$Taxon[temp$Taxon %in% c("BRODIAEA CORONARIA", "BRODIAEA SP.")] <- "TRITELEIA SP."
  temp$Taxon[temp$Taxon %in% c("PLAGIOBOTHRYS SP.")] <- "PLAGIOBOTHRYS NOTHOFULVUS"
  temp$Taxon[temp$Taxon %in% c("CENTAUREA SP.")] <- "CENTAUREA MELITENSIS"
  temp$Taxon[temp$Taxon %in% c("MADIA GRACILIS", "MADIA SATIVA")] <- "MADIA SP."
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA SP.")] <- "EUPHORBIA PEPLUS"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN EUPHORBIACEAE")] <- "EUPHORBIA SPATHULATA"
  temp$Taxon[temp$Taxon %in% c("LATHYRUS SP.")] <- "LATHYRUS SPHAERICUS"
  temp$Taxon[temp$Taxon %in% c("LOTUS SP.")] <- "ACMISPON WRANGELIANUS"
  temp$Taxon[temp$Taxon %in% c("LUPINUS BICOLOR", "LUPINUS NANUS")] <- "LUPINUS SP."
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM ALBOPURPUREUM", "TRIFOLIUM BIFIDUM", "TRIFOLIUM CILIOLATUM",
                               "TRIFOLIUM DUBIUM", "TRIFOLIUM GRACILENTUM", "TRIFOLIUM MICROCEPHALUM",
                               "TRIFOLIUM WILLDENOVII")] <- "TRIFOLIUM SP."
  temp$Taxon[temp$Taxon %in% c("VICIA AMERICANA", "VICIA SP.")] <- "VICIA SATIVA"
  temp$Taxon[temp$Taxon %in% c("CLARKIA SP.")] <- "CLARKIA PURPUREA"
  temp$Taxon[temp$Taxon %in% c("AEGILOPS SP.")] <- "AEGILOPS TRIUNCIALIS"
  temp$Taxon[temp$Taxon %in% c("AVENA FATUA")] <- "AVENA BARBATA"
  temp$Taxon[temp$Taxon %in% c("CRATAEGUS SP.")] <- "CRATAEGUS DOUGLASII"
  temp$Taxon[temp$Taxon %in% c("GALIUM APARINE", "GALIUM PARISIENSE")] <- "GALIUM SP."
  temp <- temp[! temp$Taxon %in% c("UNKNOWN ASTERACEAE SP.", "UNKNOWN LILIACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # jena.de - no changes
  
  # kbs.us
  Site = "kbs.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("MELILOTUS OFFICINALIS SSP. ALBA", "MELILOTUS SP.")] <- "MELILOTUS OFFICINALIS"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM")] <- "TRIFOLIUM REPENS"
  temp$Taxon[temp$Taxon %in% c("SETARIA SP.")] <- "SETARIA PUMILA"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN GRASS")] <- "ARRHENATHERUM ELATIUS"
  temp$Taxon[temp$Taxon %in% c("PRUNUS SP.")] <- "MALUS SP."
  temp$Taxon[temp$Taxon %in% c("ACER SP.")] <- "ACER NEGUNDO"
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # kibber.in
  Site = "kibber.in"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("UNKNOWN GRASS SP.")] <- "ELYMUS LONGIARISTATUS"
  temp$Taxon[temp$Taxon %in% c("POLYGONUM AVICULARE")] <- "POLYGONUM SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # kilp.fi
  Site = "kilp.fi"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("LEONTODON AUTUMNALIS var. TARAXACI")] <- "SCORZONEROIDES AUTUMNALIS"
  temp$Taxon[temp$Taxon %in% c("EMPETRUM NIGRUM SSP. HERMAPHRODITUM")] <- "EMPETRUM NIGRUM"
  temp$Taxon[temp$Taxon %in% c("RANUNCULUS ACRIS SSP. PUMILUS")] <- "RANUNCULUS ACRIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # kiny.au
  Site = "kiny.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ARTHROPODIUM SP.")] <- "ARTHROPODIUM MINUS"
  temp$Taxon[temp$Taxon %in% c("ATRIPLEX SP.")] <- "ATRIPLEX SEMIBACCATA"
  temp$Taxon[temp$Taxon %in% c("MAIREANA SP.")] <- "MAIREANA ENCHYLAENOIDES"
  temp$Taxon[temp$Taxon %in% c("RHODANTHE SP.")] <- "RHODANTHE CORYMBIFLORA"
  temp$Taxon[temp$Taxon %in% c("SONCHUS SP.", "SONCHUS ASPER")] <- "SONCHUS OLERACEUS"
  temp$Taxon[temp$Taxon %in% c("CRASSULA SP.")] <- "CRASSULA SIEBERIANA"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM ", "TRIFOLIUM DUBIUM", "TRIFOLIUM GLOMERATUM",
                               "TRIFOLIUM ARVENSE")] <- "TRIFOLIUM SP."
  temp$Taxon[temp$Taxon %in% c("AVENA SP.", "AVENA FATUA")] <- "AVENA BARBATA"
  temp$Taxon[temp$Taxon %in% c("BROMUS SP.")] <- "BROMUS RUBENS"
  temp$Taxon[temp$Taxon %in% c("LOLIUM PERENNE", "LOLIUM RIGIDUM")] <- "LOLIUM SP."
  temp$Taxon[temp$Taxon %in% c("RYTIDOSPERMA ")] <- "RYTIDOSPERMA SP."
  temp$Taxon[temp$Taxon %in% c("VULPIA BROMOIDES")] <- "VULPIA SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE ") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # koffler.ca - email with M. Cadotte in March 2017
  Site = "koffler.ca"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CAREX PLANTAGINEA")] <- "CAREX SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE ") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # konz.us
  Site = "konz.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA SERPENS")] <- "EUPHORBIA NUTANS"
  temp$Taxon[temp$Taxon %in% c("MUHLENBERGIA CUSPIDATA")] <- "MUHLENBERGIA RACEMOSA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # lake.us
  Site = "lake.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("DALEA SP.")] <- "DALEA CANDIDA"
  temp$Taxon[temp$Taxon %in% c("MELILOTUS ALBUS", "MELILOTUS OFFICINALIS")] <- "MELILOTUS SP."
  temp$Taxon[temp$Taxon %in% c("SOLANUM SP.")] <- "SOLANUM AMERICANUM"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # lancaster.uk - no changes
  
  # look.us
  Site = "look.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("UNKNOWN APIACEAE SP.")] <- "ANGELICA GENUFLEXA"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN BRASSICACEAE SP.")] <- "ERYSIMUM CAPITATUM"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN CARYOPHYLLACEAE SP.")] <- "MOEHRINGIA LATERIFLORA"
  temp$Taxon[temp$Taxon %in% c("CAREX HOODII")] <- "CAREX PENSYLVANICA"
  temp$Taxon[temp$Taxon %in% c("GALIUM SP.")] <- "GALIUM OREGANUM"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # marc.ar - email with J. Alberti in April 2017; NutNet data updated since then
  Site = "marc.ar"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("MEDICAGO MINIMA")] <- "MEDICAGO LUPULINA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN GRASS ", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # mcla.us
  Site = "mcla.us"
  temp <- data1[data1$site_code == Site,]
  temp$Family[temp$Taxon == "UNKNOWN LILIACEAE"] <- "Asparagaceae"
  temp$Taxon[temp$Taxon %in% c("BRODIAEA SP.", "TRITELEIA LAXA", "TRITELEIA SP.", "UNKNOWN LILIACEAE")] <- "DICHELOSTEMMA CAPITATUM"
  temp$Taxon[temp$Taxon %in% c("CENTAUREA MELITENSIS", "CENTAUREA SOLSTITIALIS")] <- "CENTAUREA SP."
  temp$Taxon[temp$Taxon %in% c("LUPINUS BICOLOR", "LUPINUS NANUS", "LUPINUS SUCCULENTUS")] <- "LUPINUS SP."
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM SP.")] <- "TRIFOLIUM FUCATUM"
  temp$Taxon[temp$Taxon %in% c("AVENA BARBATA")] <- "AVENA FATUA"
  temp$Taxon[temp$Taxon %in% c("POA SP.", "UNKNOWN GRASS")] <- "POA SECUNDA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN BRASSICACEAE", "UNKNOWN ASTERACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # mtca.au
  Site = "mtca.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("HORDEUM MURINUM SSP. LEPORINUM")] <- "HORDEUM MURINUM"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # pape.de - no changes

  # ping.au - no changes
  Site = "ping.au"
  temp <- data1[data1$site_code == Site,]
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN GRASS SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # pinj.au
  Site = "pinj.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("MEDICAGO POLYMORPHA")] <- "MEDICAGO SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN FABACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # potrok.ar - no changes (but data are unusually consistent over time)

  # rook.uk - no changes
  
  # saana.fi - no changes
  
  # sage.us - no changes

  # saline.us
  # Asclepias syriaca == Apocynum cannabinum??
  # note strong reversal in Sporobolus sp. in years 1 and 2
  Site = "saline.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("LIATRIS PUNCTATA")] <- "LIATRIS MUCRONATA"
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO RIGIDA")] <- "SOLIDAGO CANADENSIS"
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA DENTATA", "EUPHORBIA MARGINATA",
                               "EUPHORBIA NUTANS", "EUPHORBIA SERPENS", "EUPHORBIA SPATHULATA")] <- "EUPHORBIA SP."
  temp$Taxon[temp$Taxon %in% c("SOLANUM SP.", "SOLANUM ROSTRATUM")] <- "SOLANUM PTYCHANTHUM"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN LAMIACEAE", "FORB SP.", "SHRUB SP.", "UNKNOWN", "UNKNOWN SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sava.us
  Site = "sava.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("VACCINIUM VIRGATUM")] <- "VACCINIUM STAMINEUM"
  temp$Taxon[temp$Taxon %in% c("QUERCUS FALCATA")] <- "QUERCUS MARGARETTIAE"
  temp$Taxon[temp$Taxon %in% c("DANTHONIA SERICEA", "MUHLENBERGIA CAPILLARIS")] <- "ERAGROSTIS SPECTABILIS"
  temp$Taxon[temp$Taxon %in% c("RUBUS CUNEIFOLIUS")] <- "RUBUS FLAGELLARIS"
  temp$Taxon[temp$Taxon %in% c("SMILAX BONA-NOX")] <- "SMILAX MARITIMA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # sedg.us
  Site = "sedg.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ASTER SP.")] <- "CORETHROGYNE FILAGINIFOLIA"
  temp$Taxon[temp$Taxon %in% c("CIRSIUM SP.")] <- "CARDUUS PYCNOCEPHALUS"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM MICROCEPHALUM")] <- "TRIFOLIUM SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sereng.tz
  Site = "sereng.tz"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CYPERUS MOLLIPES", "MARISCUS SP.")] <- "CYPERUS NIVEUS var. LEUCOCEPHALUS"
  temp$Taxon[temp$Taxon %in% c("CYPERUS REMOTUS")] <- "CYPERUS SQUARROSUS"
  temp$Taxon[temp$Taxon %in% c("KYLLINGA NERVOSA", "KYLLINGA ALBA")] <- "KYLLINGA SP."
  temp$Taxon[temp$Taxon %in% c("DACTYLOCTENIUM SP.")] <- "DACTYLOCTENIUM AEGYPTIUM"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sevi.us
  Site = "sevi.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("SALSOLA KALI SSP. TRAGUS")] <- "SALSOLA KALI"
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA SP.")] <- "EUPHORBIA ALBOMARGINATA"
  temp$Taxon[temp$Taxon %in% c("SPHAERALCEA COCCINEA", "SPHAERALCEA HASTULATA",
                               "SPHAERALCEA POLYCHROMA")] <- "SPHAERALCEA SP."
  temp$Taxon[temp$Taxon %in% c("SPOROBOLUS CONTRACTUS", "SPOROBOLUS CRYPTANDRUS",
                               "SPOROBOLUS FLEXUOSUS")] <- "SPOROBOLUS SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # sgs.us
  # Oxytropis widespread but only in year 2??
  Site = "sgs.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("AMARANTHUS SP.")] <- "AMARANTHUS GRAECIZANS"
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA BRACHYCERA", "EUPHORBIA SP.")] <- "EUPHORBIA GLYPTOSPERMA"
  temp$Taxon[temp$Taxon %in% c("ASTRAGALUS SP.")] <- "ASTRAGALUS BISULCATUS"
  temp$Taxon[temp$Taxon %in% c("ELYMUS CANADENSIS", "SCHEDONNARDUS PANICULATUS")] <- "ELYMUS ELYMOIDES"
  temp$Taxon[temp$Taxon %in% c("SOLANUM SP.")] <- "SOLANUM ROSTRATUM"
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # shps.us
  Site = "shps.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("LAPPULA OCCIDENTALIS var. OCCIDENTALIS")] <- "LAPPULA REDOWSKII"
  temp$Taxon[temp$Taxon %in% c("ANTENNARIA SP.")] <- "ANTENNARIA DIMORPHA"
  temp$Taxon[temp$Taxon %in% c("PSEUDOSCLEROCHLOA RUPESTRIS")] <- "POA SECUNDA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN BRASSICACEAE SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sier.us - ASTER?
  Site = "sier.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("TORILIS ARVENSIS", "TORILIS NODOSA", "TORILIS")] <- "TORILIS SP."
  temp$Taxon[temp$Taxon %in% c("UNKNOWN APIACEAE")] <- "DAUCUS PUSILLUS"
  temp$Taxon[temp$Taxon %in% c("BRODIAEA SP.", "TRITELEIA LAXA", "TRITELEIA SP.",
                               "UNKNOWN LILIACEAE")] <- "TRITELEIA HYACINTHINA"
  temp$Taxon[temp$Taxon %in% c("DICHELOSTEMMA MULTIFLORUM", "DICHELOSTEMMA VOLUBILE")] <- "DICHELOSTEMMA CAPITATUM"
  temp$Taxon[temp$Taxon %in% c("PLAGIOBOTHRYS SP.")] <- "PLAGIOBOTHRYS NOTHOFULVUS"
  temp$Taxon[temp$Taxon %in% c("CARDAMINE SP.")] <- "CARDAMINE OLIGOSPERMA"
  temp$Taxon[temp$Taxon %in% c("SILENE GALLICA")] <- "PETRORHAGIA DUBIA"
  temp$Taxon[temp$Taxon %in% c("STELLARIA SP.")] <- "STELLARIA MEDIA"
  temp$Taxon[temp$Taxon %in% c("HEMIZONIA")] <- "HEMIZONIA CONGESTA"
  temp$Taxon[temp$Taxon %in% c("MADIA GRACILIS", "MADIA SATIVA")] <- "MADIA SP."
  temp$Taxon[temp$Taxon %in% c("UNKNOWN ASTERACEAE")] <- "ASTER SP."
  temp$Taxon[temp$Taxon %in% c("CONVOLVULUS SP.")] <- "CONVOLVULUS ARVENSIS"
  temp$Taxon[temp$Taxon %in% c("LUPINUS BICOLOR", "LUPINUS NANUS", "LUPINUS")] <- "LUPINUS SP."
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM SP.")] <- "TRIFOLIUM DUBIUM"
  temp$Taxon[temp$Taxon %in% c("VICIA SP.")] <- "VICIA SATIVA"
  temp$Taxon[temp$Taxon %in% c("ERODIUM SP.")] <- "ERODIUM BOTRYS"
  temp$Taxon[temp$Taxon %in% c("CLARKIA SP.")] <- "CLARKIA PURPUREA"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN ONAGRACEAE SP.")] <- "EPILOBIUM SP."
  temp$Taxon[temp$Taxon %in% c("HORDEUM MARINUM")] <- "HORDEUM MURINUM"
  temp$Taxon[temp$Taxon %in% c("LINANTHUS SP.")] <- "LINANTHUS BICOLOR"
  temp$Taxon[temp$Taxon %in% c("GALIUM SP.")] <- "GALIUM PARISIENSE"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN CARYOPHYLLACEAE", "UNKNOWN ASTERACEAE SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # smith.us
  Site = "smith.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("TRITELEIA GRANDIFLORA")] <- "BRODIAEA CORONARIA"
  temp$Taxon[temp$Taxon %in% c("SYMPHORICARPOS ALBUS var. LAEVIGATUS")] <- "SYMPHORICARPOS ALBUS"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN ASTERACEAE")] <- "CREPIS CAPILLARIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  ### check for other changes to smith.us composition ####
  
  # spin.us
  Site = "spin.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("LYCHNIS LATIFOLIA SSP. ALBA")] <- "SILENE LATIFOLIA"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN CYPERACEAE")] <- "CAREX SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # summ.za - no changes

  # temple.us
  Site = "temple.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("POLYTAENIA TEXANA")] <- "POLYTAENIA NUTTALLII" # conversation with P. Fay
  temp$Taxon[temp$Taxon %in% c("TORILIS SP.")] <- "TORILIS ARVENSIS"
  temp$Taxon[temp$Taxon %in% c("LACTUCA SP.")] <- "LACTUCA SERRIOLA"
  temp$Taxon[temp$Taxon %in% c("AGALINIS SP.")] <- "AGALINIS FASCICULATA"
  temp$Taxon[temp$Taxon %in% c("SPOROBOLUS SP.")] <- "SPOROBOLUS COMPOSITUS"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN FABACEAE", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # thth.is
  Site = "thth.is"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("FESTUCA RICHARDSONII", "FESTUCA RUBRA")] <- "FESTUCA SP."
  temp$Taxon[temp$Taxon %in% c("RUMEX SP.")] <- "RUMEX ACETOSA"
  temp$Taxon[temp$Taxon %in% c("VIOLA SP.")] <- "VIOLA PALUSTRIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # tmlr.is
  Site = "tmlr.is"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("AGROSTIS SP.")] <- "AGROSTIS STOLONIFERA"
  temp$Taxon[temp$Taxon %in% c("FESTUCA RICHARDSONII", "FESTUCA RUBRA")] <- "FESTUCA SP."
  temp$Taxon[temp$Taxon %in% c("RUMEX SP.")] <- "RUMEX ACETOSA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # trel.us - no JUNCUS in 2008??  No CAREX in 2009 or 2010?
  Site = "trel.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("BROMUS SP.")] <- "BROMUS INERMIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # ukul.za
  Site = "ukul.za"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("PACHYCARPUS SP.")] <- "PACHYCARPUS SCABER"
  temp$Taxon[temp$Taxon %in% c("ACACIA SP.")] <- "ACACIA NILOTICA SSP. KRAUSSIANA"
  temp$Taxon[temp$Taxon %in% c("CHAMAECRISTA COMOSA", "CHAMAECRISTA PLUMOSA")] <- "CHAMAECRISTA SP."
  temp$Taxon[temp$Taxon %in% c("CROTALARIA GLOBIFERA")] <- "CROTALARIA SP."
  temp$Taxon[temp$Taxon %in% c("ALBUCA")] <- "ALBUCA SETOSA"
  temp$Taxon[temp$Taxon %in% c("LEDEBOURIA SP.")] <- "LEDEBOURIA COOPERI"
  temp$Taxon[temp$Taxon %in% c("HYPERICUM AETHIOPICUM SSP. SONDERI")] <- "HYPERICUM SP."
  temp$Taxon[temp$Taxon %in% c("LEONOTIS OCYMIFOLIA var. RAINERIANA")] <- "LEONOTIS LEONURUS"
  temp$Taxon[temp$Taxon %in% c("SOLANUM SP.")] <- "SOLANUM MAURITIANUM"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN FABACEAE SP.", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # unc.us
  Site = "unc.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ERIGERON SP.")] <- "ERIGERON ANNUUS"
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO SP.")] <- "SOLIDAGO PINETORUM"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN ASTERACEAE")] <- "VERBESINA OCCIDENTALIS"
  temp$Taxon[temp$Taxon %in% c("PANICUM DICHOTOMUM", "PANICUM LINEARIFOLIUM")] <- "PANICUM SP."
  temp$Taxon[temp$Taxon %in% c("SMILAX BONA-NOX")] <- "SMILAX ROTUNDIFOLIA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # valm.ch - no changes
  
  # veluwe.nl
  Site = "veluwe.nl"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("VERONICA SP.")] <- "VERONICA OFFICINALIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)

    # yarra.au
  Site = "yarra.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("VICIA SATIVA")] <- "VICIA SP."
  temp$Taxon[temp$Taxon %in% c("DICHELACHNE SP.")] <- "DICHELACHNE SQUAMULOSUM"
  temp$Taxon[temp$Taxon %in% c("PASPALUM DILATATUM", "PASPALUM NOTATUM")] <- "PASPALUM SP."
  temp$Taxon[temp$Taxon %in% c("UNKNOWN GRASS ")] <- "SPOROBOLUS SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  temp <- data1 %>%
    group_by(site_code, block, plot, subplot, trt, year, year_trt, Taxon) %>%
    summarize(max_cover = sum(max_cover), .groups = "keep")

  temp
  
}

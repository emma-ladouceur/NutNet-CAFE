##### NUTNET TAXONOMIC ADJUSTMENTS
## Based on analyses from 2017 NutNet workshop and previous efforts
## JD Bakker et al
## 191105

## Separate taxonomic adjustments for each site to ensure temporal consistency in naming

Taxonomic.Adjustments <- function(datafile = datafile) {
  
  data1 <- datafile
  
  # Drop non-living taxa
  data1 <- data1[data1$live == 1,]
  
  # Drop mosses, lichens, fungi
  data1 <- data1[! data1$functional_group %in% c("BRYOPHYTE", "LICHEN", "CLUBMOSS", "LIVERWORT", "NON-LIVE") , ]
  #some families not consistently identified to functional group
  data1 <- data1[! data1$Family %in% c("Dicranaceae", "Lycopodiaceae", "Phallales", "Pottiaceae",
                                       "Selaginellaceae", "Thuidiaceae", "MNIACEAE") , ]
  
  # Drop records not assigned to family
  data1 <- data1[! data1$Family %in% c("NULL") , ]  # 170079 x 18
  
  # Site-specific taxonomic adjustments

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
  # POA ATTENUATA, VIOLA; PEDICULARIS?; SAUSSUREA?
  
  # barta.us - no changes
  
  # bldr.us
  # PODOSPERMUM, MEDICAGO, UNKNOWN GRASS?
  
  # bnch.us
  # UNKNOWN GRASS?
  
  # bogong.au
  Site = "bogong.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ERIGERON BELLIDIOIDES", "ERIGERON NITIDUS")] <- "ERIGERON SP."
  temp$Taxon[temp$Taxon %in% c("MICROSERIS SP.")] <- "MICROSERIS LANCEOLATA"
  temp$Taxon[temp$Taxon %in% c("PHEBALIUM SQUAMULOSUM SSP. OZOTHAMNOIDES")] <- "PHEBALIUM SQUAMULOSUM"
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
  temp$Taxon[temp$Taxon %in% c("SOLANUM SP.")] <- "SOLANUM CAROLINENSE"
  temp <- temp[temp$Taxon != "UNKNOWN GRASS" , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cdcr.us # also Asclepias, Solidago, Cyperus
  Site = "cdcr.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CHENOPODIUM SP.")] <- "CHENOPODIUM ALBUM"
  temp$Taxon[temp$Taxon %in% c("ASTER SP.")] <- "SYMPHYOTRICHUM BOREALE"
  temp$Taxon[temp$Taxon %in% c("ERIGERON CANADENSIS")] <- "CONYZA CANADENSIS"
  temp$Taxon[temp$Taxon %in% c("ERIGERON SP.")] <- "ERIGERON STRIGOSUS"
  temp$Taxon[temp$Taxon %in% c("RUDBECKIA SP.", "RUDBECKIA HIRTA var. PULCHERRIMA")] <- "RUDBECKIA HIRTA"
  temp$Taxon[temp$Taxon %in% c("TRAGOPOGON SP.")] <- "TRAGOPOGON DUBIUS"
  temp$Taxon[temp$Taxon %in% c("CAREX SCOPARIA")] <- "CAREX SP."
  temp$Taxon[temp$Taxon %in% c("CYPERUS FILICULMIS", "CYPERUS GRAYI", "CYPERUS LUPULINUS", "CYPERUS SCHWEINITZII")] <- "CYPERUS SP."
  temp <- temp[temp$Taxon != "UNKNOWN FABACEAE" , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cdpt.us - no changes
  
  # cereep.fr
  Site = "cereep.fr"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("MYOSOTIS ARVENSIS", "MYOSOTIS RAMOSISSIMA")] <- "MYOSOTIS SP."
  temp$Taxon[temp$Taxon %in% c("SILENE LATIFOLIA")] <- "SILENE LATIFOLIA SSP. ALBA"
  temp$Taxon[temp$Taxon %in% c("CREPIS SETOSA")] <- "CREPIS SP."
  temp$Taxon[temp$Taxon %in% c("JACOBAEA VULGARIS")] <- "SENECIO JACOBAEA"
  temp$Taxon[temp$Taxon %in% c("PICRIS SP.")] <- "PICRIS HIERACIOIDES"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM ARVENSE", "TRIFOLIUM CAMPESTRE", "TRIFOLIUM DUBIUM", "TRIFOLIUM PRATENSE",
                               "TRIFOLIUM REPENS")] <- "TRIFOLIUM SP."
  temp$Taxon[temp$Taxon %in% c("VICIA CRACCA", "VICIA HIRSUTA", "VICIA LATHYROIDES", "VICIA SATIVA",
                               "VICIA sativa SSP. nigra", "VICIA SEPIUM", "VICIA TETRASPERMA")] <- "VICIA SP."
  temp$Taxon[temp$Taxon %in% c("ERODIUM SP.")] <- "ERODIUM CICUTARIUM"
  temp$Taxon[temp$Taxon %in% c("GERANIUM SP.")] <- "GERANIUM MOLLE"
  temp$Taxon[temp$Taxon %in% c("HYPERICUM MACULATUM SSP. OBTUSIUSCULUM", "HYPERICUM PERFORATUM")] <- "HYPERICUM SP."
  temp$Taxon[temp$Taxon %in% c("AGROSTIS CANINA", "AGROSTIS GIGANTEA")] <- "AGROSTIS SP."
  temp$Taxon[temp$Taxon %in% c("ARRHENATHERUM elatius SSP. bulbosum")] <- "ARRHENATHERUM ELATIUS"
  temp$Taxon[temp$Taxon %in% c("BROMUS SP.", "BROMUS PECTINATUS", "BROMUS RACEMOSUS")] <- "BROMUS HORDEACEUS"
  temp$Taxon[temp$Taxon %in% c("FESTUCA ARUNDINACEA", "FESTUCA SP.")] <- "FESTUCA RUBRA"
  temp$Taxon[temp$Taxon %in% c("POA pratensis SSP. latifolia", "POA SP.", "POA TRIVIALIS")] <- "POA PRATENSIS"
  temp$Taxon[temp$Taxon %in% c("RUMEX SP.")] <- "RUMEX ACETOSELLA"
  temp <- temp[temp$Taxon != "UNKNOWN CARYOPHYLLACEAE SP." , ]
  temp <- temp[temp$Taxon != "UNKNOWN ASTERACEAE SP." , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # chilcas.ar
  Site = "chilcas.ar"
  temp <- data1[data1$site_code == Site,]
  # CAREX SP., CYPERUS SP.?
  temp$Taxon[temp$Taxon %in% c("BROMUS AULETICUS")] <- "BROMUS BRACHYANTHERUS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # comp.pt # Trifolium?
  Site = "comp.pt"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CREPIS CAPILLARIS", "CREPIS VESICARIA")] <- "CREPIS SP."
  temp$Taxon[temp$Taxon %in% c("HYPOCHAERIS", "LEONTODON TARAXACOIDES")] <- "HYPOCHAERIS GLABRA"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM SP.")] <- "TRIFOLIUM GLOMERATUM"
  temp$Taxon[temp$Taxon %in% c("ERODIUM AETHIOPICUM", "ERODIUM BOTRYS", "ERODIUM CICUTARIUM")] <- "ERODIUM SP."
  temp$Taxon[temp$Taxon %in% c("OROBANCHE SP.")] <- "OROBANCHE MINOR"
  temp$Taxon[temp$Taxon %in% c("PLANTAGO BELLARDII")] <- "PLANTAGO BELLARDI"
  temp$Taxon[temp$Taxon %in% c("VULPIA")] <- "VULPIA BROMOIDES"
  temp <- temp[! temp$Taxon %in% c("UNKNOWN ASTERACEAE SP.", "UNKNOWN ASTERACEAE", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # cowi.ca - no changes
  
  # doane.us
  # SOLIDAGO; UNKNOWN GRASS
  
  # elliot.us
  Site = "elliot.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("JUNCUS SP.")] <- "JUNCUS DUBIUS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # ethass.au - Aristida?
  Site = "ethass.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("PORTULACA SP.")] <- "PORTULACA INTRATERRANEA"
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
  # DAUCUS, SANICULA, TORILIS, UNKNOWN APIACEAE, BRODIAEA, CENTAUREA, MADIA, UNKNOWN ASTERACEAE, AMSINCKIA, PLAGIOBOTHRYS, EUPHORBIA, 
  #  UNKNOWN EUPHORBIACEAE, ACMISPON, LATHYRUS, LUPINUS, ...
  
  # jena.de - no changes
  
  # kbs.us
  Site = "kbs.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("MELILOTUS OFFICINALIS SSP. ALBA", "MELILOTUS SP.")] <- "MELILOTUS OFFICINALIS"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM")] <- "TRIFOLIUM REPENS"
  temp$Taxon[temp$Taxon %in% c("SETARIA SP.")] <- "SETARIA PUMILA"
  temp$Taxon[temp$Taxon %in% c("PRUNUS SP.")] <- "MALUS SP."
  temp$Taxon[temp$Taxon %in% c("ACER SP.")] <- "ACER NEGUNDO"
  temp <- temp[! temp$Taxon %in% c("UNKNOWN BRASSICACEAE", "UNKNOWN ASTERACEAE", "UNKNOWN GRASS") , ]
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
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE ", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # koffler.ca - email with M. Cadotte in March 2017
  Site = "koffler.ca"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CAREX PLANTAGINEA")] <- "CAREX SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE ", "UNKNOWN CYPERACEAE SP.", "UNKNOWN GRASS", "UNKNOWN ROSACEAE ") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # konz.us
  Site = "konz.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO CANADENSIS")] <- "SOLIDAGO MISSOURIENSIS" # Kim to verify yet. 191115
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA SERPENS")] <- "EUPHORBIA NUTANS"
  temp$Taxon[temp$Taxon %in% c("CALYLOPHUS SP.")] <- "CALYLOPHUS SERRULATUS"
  temp$Taxon[temp$Taxon %in% c("MUHLENBERGIA CUSPIDATA")] <- "MUHLENBERGIA RACEMOSA"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # lake.us - DALEA, MELILOTUS, FORB SP.

  # lancaster.uk - no changes
  
  # look.us
  Site = "look.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("CAREX HOODII")] <- "CAREX PENSYLVANICA"
  temp$Taxon[temp$Taxon %in% c("GALIUM SP.")] <- "GALIUM OREGANUM"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN APIACEAE SP.", "UNKNOWN BRASSICACEAE SP.",
                                    "UNKNOWN CARYOPHYLLACEAE SP.", "UNKNOWN ASTERACEAE SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # marc.ar - email with J. Alberti in April 2017; NutNet data updated since then
  Site = "marc.ar"
  temp <- data1[data1$site_code == Site,]
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN GRASS ", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # mcla.us
  # TRITELEIA (and others in family?), CENTAUREA, LUPINUS, TRIFOLIUM, POA, UNKNOWN GRASS
  
  # mtca.au
  Site = "mtca.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("HORDEUM MURINUM SSP. LEPORINUM")] <- "HORDEUM MURINUM"
  temp$Taxon[temp$Taxon %in% c("STIPA NITIDA", "STIPA TRICHOPHYLLA")] <- "STIPA SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
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
  
  # sage.us - no changes

  # saana.fi - no changes

  # sava.us - VACCINIUM, HYPERICUM, RUBUS, SMILAX
  
  # sedg.us - ASTER?
  Site = "sedg.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM MICROCEPHALUM")] <- "TRIFOLIUM SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sereng.tz - MARISCUS == UNKNOWN CYPERACEAE?
  Site = "sereng.tz"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("KYLLINGA NERVOSA", "KYLLINGA ALBA")] <- "KYLLINGA SP."
  temp$Taxon[temp$Taxon %in% c("DACTYLOCTENIUM SP.")] <- "DACTYLOCTENIUM AEGYPTIUM"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ACANTHACEAE", "UNKNOWN CYPERACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sevi.us
  Site = "sevi.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("SALSOLA KALI SSP. TRAGUS")] <- "SALSOLA KALI"
  temp$Taxon[temp$Taxon %in% c("EUPHORBIA EXSTIPULATA", "EUPHORBIA FENDLERI", "EUPHORBIA SERPYLLIFOLIA",
                               "EUPHORBIA SERRULA")] <- "EUPHORBIA SP."
  temp$Taxon[temp$Taxon %in% c("SPHAERALCEA COCCINEA", "SPHAERALCEA HASTULATA", "SPHAERALCEA POLYCHROMA",
                               "SPHAERALCEA WRIGHTII")] <- "SPHAERALCEA SP."
  temp$Taxon[temp$Taxon %in% c("ARISTIDA ADSCENSIONIS", "ARISTIDA PURPUREA")] <- "ARISTIDA SP."
  temp$Taxon[temp$Taxon %in% c("SPOROBOLUS CONTRACTUS", "SPOROBOLUS CRYPTANDRUS", "SPOROBOLUS FLEXUOSUS")] <- "SPOROBOLUS SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  # sgs.us
  # LACTUCA, PICRADENIOPSIS, EUPHORBIA, SCUTELLARIA, UNKNOWN?, ELYMUS
  
  # shps.us
  Site = "shps.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("LAPPULA REDOWSKII")] <- "LAPPULA OCCIDENTALIS var. OCCIDENTALIS"
  temp$Taxon[temp$Taxon %in% c("ANTENNARIA SP.")] <- "ANTENNARIA DIMORPHA"
  temp$Taxon[temp$Taxon %in% c("PSEUDOSCLEROCHLOA RUPESTRIS")] <- "POA SECUNDA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN AMARANTHACEAE SP.", "UNKNOWN BRASSICACEAE SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # sier.us - ASTER?
  Site = "sier.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("TORILIS ARVENSIS", "TORILIS NODOSA", "TORILIS")] <- "TORILIS SP."
  temp$Taxon[temp$Taxon %in% c("BRODIAEA SP.", "DICHELOSTEMMA CAPITATUM", "DICHELOSTEMMA MULTIFLORUM", 
                               "DICHELOSTEMMA VOLUBILE", "TRITELEIA LAXA", "TRITELEIA SP.",
                               "UNKNOWN LILIACEAE")] <- "TRITELEIA HYACINTHINA"
  temp$Taxon[temp$Taxon %in% c("PLAGIOBOTHRYS SP.")] <- "PLAGIOBOTHRYS NOTHOFULVUS"
  temp$Taxon[temp$Taxon %in% c("CARDAMINE SP.")] <- "CARDAMINE OLIGOSPERMA"
  temp$Taxon[temp$Taxon %in% c("CENTAUREA SOLSTITIALIS")] <- "CENTAUREA MELITENSIS"
  temp$Taxon[temp$Taxon %in% c("HEMIZONIA")] <- "HEMIZONIA CONGESTA"
  temp$Taxon[temp$Taxon %in% c("UNKNOWN ASTERACEAE")] <- "ASTER SP."
  temp$Taxon[temp$Taxon %in% c("CONVOLVULUS SP.")] <- "CONVOLVULUS ARVENSIS"
  temp$Taxon[temp$Taxon %in% c("LUPINUS BICOLOR", "LUPINUS NANUS")] <- "LUPINUS"
  temp$Taxon[temp$Taxon %in% c("TRIFOLIUM SP.")] <- "TRIFOLIUM DUBIUM"
  temp$Taxon[temp$Taxon %in% c("VICIA SP.")] <- "VICIA SATIVA"
  temp$Taxon[temp$Taxon %in% c("ERODIUM BOTRYS", "ERODIUM CICUTARIUM", "ERODIUM MOSCHATUM")] <- "ERODIUM SP."
  temp$Taxon[temp$Taxon %in% c("CLARKIA SP.")] <- "CLARKIA PURPUREA"
  temp$Taxon[temp$Taxon %in% c("HORDEUM MARINUM")] <- "HORDEUM MURINUM"
  temp$Taxon[temp$Taxon %in% c("LINANTHUS BICOLOR", "LINANTHUS PARVIFLORUS")] <- "LINANTHUS SP."
  temp$Taxon[temp$Taxon %in% c("GALIUM APARINE", "GALIUM PARISIENSE", "GALIUM PORRIGENS")] <- "GALIUM SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN APIACEAE", "UNKNOWN CARYOPHYLLACEAE", "UNKNOWN ASTERACEAE SP.",
                                    "UNKNOWN ONAGRACEAE SP.") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # smith.us
  Site = "smith.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("TRITELEIA GRANDIFLORA")] <- "BRODIAEA CORONARIA"
  temp$Taxon[temp$Taxon %in% c("SYMPHORICARPOS ALBUS var. LAEVIGATUS")] <- "SYMPHORICARPOS ALBUS"
  temp$Taxon[temp$Taxon %in% c("CREPIS CAPILLARIS")] <- "TARAXACUM CAMPYLODES"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # spin.us
  Site = "spin.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("UNKNOWN CYPERACEAE")] <- "CAREX SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # summ.za
  Site = "summ.za"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("SEBAEA GRANDIS")] <- "SEBAEA SP."
  temp$Taxon[temp$Taxon %in% c("EULOPHIA TENELLA")] <- "EULOPHIA SP."
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # temple.us
  Site = "temple.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("POLYTAENIA TEXANA")] <- "POLYTAENIA NUTTALLII" # conversation with P. Fay
  temp$Taxon[temp$Taxon %in% c("TORILIS SP.")] <- "TORILIS ARVENSIS"
  temp$Taxon[temp$Taxon %in% c("LACTUCA SP.")] <- "LACTUCA SERRIOLA"
  temp$Taxon[temp$Taxon %in% c("AGALINIS SP.")] <- "AGALINIS FASCICULATA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN FABACEAE", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # trel.us
  Site = "trel.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("BROMUS SP.")] <- "BROMUS INERMIS"
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # ukul.za - LEONOTIS?
  Site = "ukul.za"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("PACHYCARPUS ASPERIFOLIUS", "PACHYCARPUS SCABER")] <- "PACHYCARPUS SP."
  temp$Taxon[temp$Taxon %in% c("ACACIA SP.")] <- "ACACIA NILOTICA SSP. KRAUSSIANA"
  temp$Taxon[temp$Taxon %in% c("CHAMAECRISTA COMOSA", "CHAMAECRISTA PLUMOSA")] <- "CHAMAECRISTA SP."
  temp$Taxon[temp$Taxon %in% c("CROTALARIA GLOBIFERA")] <- "CROTALARIA SP."
  temp$Taxon[temp$Taxon %in% c("ALBUCA")] <- "ALBUCA SETOSA"
  temp$Taxon[temp$Taxon %in% c("LEDEBOURIA COOPERI")] <- "LEDEBOURIA SP."
  temp$Taxon[temp$Taxon %in% c("HYPERICUM AETHIOPICUM SSP. SONDERI")] <- "HYPERICUM SP."
  temp$Taxon[temp$Taxon %in% c("LEONOTIS OCYMIFOLIA var. RAINERIANA")] <- "LEONOTIS LEONURUS"
  temp$Taxon[temp$Taxon %in% c("SOLANUM AMERICANUM", "SOLANUM MAURITIANUM", "SOLANUM PANDURIFORME",
                               "SOLANUM RETROFLEXUM")] <- "SOLANUM SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN CYPERACEAE SP.", "UNKNOWN FABACEAE SP.", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # unc.us
  Site = "unc.us"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("ERIGERON ANNUUS", "ERIGERON STRIGOSUS")] <- "ERIGERON SP."
  temp$Taxon[temp$Taxon %in% c("SOLIDAGO SP.")] <- "SOLIDAGO PINETORUM"
  temp$Taxon[temp$Taxon %in% c("PANICUM DICHOTOMUM", "PANICUM LINEARIFOLIUM")] <- "PANICUM SP."
  temp$Taxon[temp$Taxon %in% c("SMILAX BONA-NOX")] <- "SMILAX ROTUNDIFOLIA"
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN ASTERACEAE", "UNKNOWN GRASS") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)
  
  # valm.ch - no changes
  
  # yarra.au
  Site = "yarra.au"
  temp <- data1[data1$site_code == Site,]
  temp$Taxon[temp$Taxon %in% c("VICIA SATIVA")] <- "VICIA SP."
  temp$Taxon[temp$Taxon %in% c("DICHELACHNE SQUAMULOSUM")] <- "DICHELACHNE SP."
  temp$Taxon[temp$Taxon %in% c("PASPALUM DILATATUM", "PASPALUM NOTATUM")] <- "PASPALUM SP."
  temp <- temp[ ! temp$Taxon %in% c("UNKNOWN GRASS ") , ]
  data1 <- rbind(data1[data1$site_code != Site,], temp)

  temp <- ddply(data1, .(site_code, block, plot, subplot, trt, year, year_trt, Taxon), summarize, max_cover = sum(max_cover))
  
  temp
  
}

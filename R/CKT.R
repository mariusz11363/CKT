CKT <- function(){
  
  if (!require("shiny")) install.packages("shiny")
  if (!require("stringr")) install.packages("stringr")
  if (!require("splitstackshape")) install.packages("splitstackshape")
  if (!require("stringdist")) install.packages("stringdist")
  if (!require("lettercase")) install.packages("lettercase")
  if (!require("dplyr")) install.packages("dplyr")
  
  library("shiny")
  library("stringr")
  library(splitstackshape)
  library("stringdist")
  library("lettercase")
  library("dplyr")
  
  options(defaultPackages=c(getOption("defaultPackages"),"CKT"))
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Wybierz plik CKT", multiple = T),
        tags$hr(),
        checkboxInput("header", "Zapis", T),
        actionButton("do", "Zapisz")
      ),
      mainPanel(
        verbatimTextOutput("zapis"),
        tableOutput("contents")
        
      )
    )
  )
  
  
  server <- function(input, output) {
    
    
    options(shiny.maxRequestSize=300*1024^2) 
    
    
    
    nazwy <- function(plik_do_poprawy, plik_do_zapisu, zapis=F){
      
      
      
      
      nazwy <- readxl::read_xlsx(plik_do_poprawy, 1, col_names = F)
      
      nazwy <- as.data.frame(nazwy)
      
      #nazwy <- cSplit(nazwy, 'X__2', sep=" ", type.convert=FALSE)
      
      
      #nazwy <- nazwy[1:500,]
      
 
      
      for(i in 4:4){
        
        cat("Uruchomiono proces...\n")
        
        nazwy$jednostki <- as.vector(str_match(nazwy[[i]], "[0-9]cm|[0-9] cm|[0-9]mm|[0-9] mm|[0-9]MM|[0-9] MM|[0-9]CM|[0-9] CM|[0-9] KG|[0-9] ML|[0-9] L|[0-9] G|[0-9]KG|[0-9]ML|[0-9]L|[0-9]G|[0-9] kg|[0-9] ml|[0-9] l|[0-9] g|[0-9]kg|[0-9]ml|[0-9]l|[0-9]g|[[:digit:]]+[\\,\\.]*[[:digit:]]+kg|[[:digit:]]+[\\,\\.]*[[:digit:]]+ kg|[[:digit:]]+[\\,\\.]*[[:digit:]]+g|[[:digit:]]+[\\,\\.]*[[:digit:]]+ g|[[:digit:]]+[\\,\\.]*[[:digit:]]+ml|[[:digit:]]+[\\,\\.]*[[:digit:]]+ ml|[[:digit:]]+[\\,\\.]*[[:digit:]]+l|[[:digit:]]+[\\,\\.]*[[:digit:]]+ l|[[:digit:]]+[\\,\\.]*[[:digit:]]+KG|[[:digit:]]+[\\,\\.]*[[:digit:]]+ KG|[[:digit:]]+[\\,\\.]*[[:digit:]]+G|[[:digit:]]+[\\,\\.]*[[:digit:]]+ G|[[:digit:]]+[\\,\\.]*[[:digit:]]+ML|[[:digit:]]+[\\,\\.]*[[:digit:]]+ ML|[[:digit:]]+[\\,\\.]*[[:digit:]]+L|[[:digit:]]+[\\,\\.]*[[:digit:]]+ L|[[:digit:]]+[\\,\\.]*[[:digit:]]+cm|[[:digit:]]+[\\,\\.]*[[:digit:]]+ cm|[[:digit:]]+[\\,\\.]*[[:digit:]]+CM|[[:digit:]]+[\\,\\.]*[[:digit:]]+ CM|[[:digit:]]+[\\,\\.]*[[:digit:]]+mm|[[:digit:]]+[\\,\\.]*[[:digit:]]+ mm|[[:digit:]]+[\\,\\.]*[[:digit:]]+MM|[[:digit:]]+[\\,\\.]*[[:digit:]]+ MM"))
        nazwy$bez_spacji <- gsub(" ","",str_decapitalize(nazwy$jednostki))
        nazwy$bez_spacji <- gsub("kg"," kg",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("g"," g",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("k g","kg",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("ml"," ml",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("l"," L",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("m L","ml",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("cm"," cm",nazwy$bez_spacji)
        nazwy$bez_spacji <- gsub("mm"," mm",nazwy$bez_spacji)
        
        for(r in 2:nrow(nazwy)){
          
          if(is.na(nazwy$jednostki[r])==F){
        nazwy[[i]][r] <- gsub(nazwy$jednostki[r],nazwy$bez_spacji[r],nazwy[[i]][r])
          }
        }
        
        
        cat("Gramatury zostały przetworzone.\n")
        cat("Przetwarzanie jednostek wymiarowych...\n")

        
        cat("Jednostki wymiarów zostały opracowane.\n")
        cat("Trwa przetwarzanie wyjątków...\n")
        
        nazwy[i] <- gsub("KAN.SANDWICH", "KANAPKA SANDWICH", nazwy[[i]])
        nazwy[i] <- gsub("NAP.ŻYWIEC", "NAPÓJ ŻYWIEC", nazwy[[i]])
        nazwy[i] <- gsub("NEKTARYNA", "NEKTARYNKA", nazwy[[i]])
        nazwy[i] <- gsub("SAŁ.WIELKANOCNA", "SAŁATKA WIELKANOCNA", nazwy[[i]])
        nazwy[i] <- gsub("SZYMB.CHLEB", "CHLEB SZYMBARCZANKA", nazwy[[i]])
        
        nazwy[i] <- gsub("KROJ.", "KROJONY", nazwy[[i]])
        
        nazwy[i] <- gsub("([0-9])([KR])+", "\\1 K", nazwy[[i]])
        
        nazwy[i] <- gsub(" KR ","KROJONY", nazwy[[i]])
        nazwy[i] <- gsub("ZBOŻ","ZBOŻOWY", nazwy[[i]])
        nazwy[i] <- gsub("OW.TROP.","OWOCE TROPIKALNE", nazwy[[i]])
        
        
        
        nazwy[i] <- gsub("TAB.MUS.MULTI.","TABLETKI MUSUJĄCE MUTIWITAMINA ", nazwy[[i]])
        nazwy[i] <- gsub("CYTR-LIM","CYTRYNA LIMONKA", nazwy[[i]])
        
        nazwy[i] <- gsub("JAG-LIM-MIĘ","JAGODA LIMONKA MIĘTA", nazwy[[i]])
        nazwy[i] <- gsub("O SM.","O SMAKU ", nazwy[[i]])
        nazwy[i] <- gsub("LIM-MIĘT.","LIMONKA MIĘTA ", nazwy[[i]])
        nazwy[i] <- gsub("CHUST.","CHUSTECZKI ", nazwy[[i]])
        nazwy[i] <- gsub("PRZYPR.","PRZYPRAWA ", nazwy[[i]])
        nazwy[i] <- gsub("KIEŁB.","KIEŁBASA ", nazwy[[i]])
        nazwy[i] <- gsub("DROŻDŻ.","DROŻDŻÓWKA ", nazwy[[i]])
        nazwy[i] <- gsub("CUK.","CUKIERKI ", nazwy[[i]])
        nazwy[i] <- gsub("CYG.","CYGARETKI ", nazwy[[i]])
        nazwy[i] <- gsub("DEO.","DEZODORANT ", nazwy[[i]])
        nazwy[i] <- gsub("POD PR.","PRYSZNIC ", nazwy[[i]])
        nazwy[i] <- gsub("INTEN.REGENER.","INTENSE REGENERUJĄCY", nazwy[[i]])
        nazwy[i] <- gsub("REGENER.","REGENERUJĄCY ", nazwy[[i]])
        nazwy[i] <- gsub("ENER.","ENERGETYCZNY ", nazwy[[i]])
        
        nazwy[i] <- gsub("KAN.TRÓJ","KANAPKA TRÓJKĄTNA", nazwy[[i]])
        nazwy[i] <- gsub("KAN.TRÓJKAT","KANAPKA TRÓJKĄTNA", nazwy[[i]])
        nazwy[i] <- gsub("D/NIEMOW.","DLA NIEMOWLĄT ", nazwy[[i]])
        nazwy[i] <- gsub("D/NIEMO.","DLA NIEMOWLĄT", nazwy[[i]])
        nazwy[i] <- gsub("D/NIEM.","DLA NIEMOWLĄT", nazwy[[i]])
        nazwy[i] <- gsub("KGg","KG", nazwy[[i]])
        nazwy[i] <- gsub("LL","L", nazwy[[i]])
        nazwy[i] <- gsub("PIEKIEL.","PIEKIELNE ", nazwy[[i]])
        nazwy[i] <- gsub("KIEL.","KIELISZEK ", nazwy[[i]])
        nazwy[i] <- gsub("ANTYBAK.","ANTYBAKTERYJNE ", nazwy[[i]])
        nazwy[i] <- gsub("BROZSK.","BRZOSKWINIA ", nazwy[[i]])
        nazwy[i] <- gsub("BRZOSK.","BRZOSKWINIA", nazwy[[i]])
        nazwy[i] <- gsub("KROJONYNE","KROJONE", nazwy[[i]])
        nazwy[i] <- gsub("BAZ/MIET","BAZYLIA MIĘTA", nazwy[[i]])
        nazwy[i] <- gsub("ANGIEL.ŻOŁNIER.","ANGIELSKI ŻOŁNIERSKI", nazwy[[i]])
        nazwy[i] <- gsub("//","/", nazwy[[i]])
        nazwy[i] <- gsub("ENERGETYCZNY IZER","ENERGIZER", nazwy[[i]])
        nazwy[i] <- gsub("KIEŁBASA SA","KIEŁBASA", nazwy[[i]])
        nazwy[i] <- gsub("SPIRYT.","SPIRYTUSOWY ", nazwy[[i]])
        
        
        
        nazwy[i] <- gsub("LIM-","LIMONKA ", nazwy[[i]])
        nazwy[i] <- gsub("JABŁ-","JABŁKO ", nazwy[[i]])
        nazwy[i] <- gsub("WOŁOWOWINĄ","WOŁOWINĄ", nazwy[[i]])
        nazwy[i] <- gsub("WP GARMAŻ","WIEPRZOWE GARMAŻERYJNE", nazwy[[i]])
        nazwy[i] <- gsub("HERBATA EXP.","HERBATA EKSPRESOWA ", nazwy[[i]])
        nazwy[i] <- gsub("KAWA ROZP.","KAWA ROZPUSZCZALNA", nazwy[[i]])
        nazwy[i] <- gsub("KAWA ZIAR.","KAWA ZIARNISTA ", nazwy[[i]])
        nazwy[i] <- gsub(" gAT."," GRATIS", nazwy[[i]])
        nazwy[i] <- gsub("TRZEB.MASŁO EXTRA","Mlekovita Masło ekstra z Trzebowniska", nazwy[[i]])
        nazwy[i] <- gsub("ŻEBERKA WP","ŻEBERKA WIEPRZOWE", nazwy[[i]])
        nazwy[i] <- gsub("PO/GOL.","PO GOLENIU ", nazwy[[i]])
        nazwy[i] <- gsub("ESPR.","ESPRESSO ", nazwy[[i]])
        nazwy[i] <- gsub("SPR.","SPRAY ", nazwy[[i]])
        nazwy[i] <- gsub("SEREWTKI","SERWETKI", nazwy[[i]])
        nazwy[i] <- gsub("SUSZ.","SUSZONE ", nazwy[[i]])
        nazwy[i] <- gsub("CUKIERKI ERKI","CUKIERKI", nazwy[[i]])
        nazwy[i] <- gsub("PRZYPRAWA WA","PRZYPRAWA WARZYWNA", nazwy[[i]])
        nazwy[i] <- gsub("KARMA D/P.","KARMA DLA PSA ", nazwy[[i]])
        nazwy[i] <- gsub("KARMA D/K.","KARMA DLA KOTA ", nazwy[[i]])
        nazwy[i] <- gsub("MR CAT D/K","MR CAT DLA KOTA", nazwy[[i]])
        nazwy[i] <- gsub("GOLONKA WP","GOLONKA WIEPRZOWA", nazwy[[i]])
        nazwy[i] <- gsub("GULASZ WP","GULASZ WPIERZOWY", nazwy[[i]])
        nazwy[i] <- gsub("GOLONKI WP"," GOLONKI WIEPRZOWEJ", nazwy[[i]])
        nazwy[i] <- gsub("SZYNKI WP","SZYNKI WIEPRZOWEJ", nazwy[[i]])
        nazwy[i] <- gsub("CHUSTECZKI CZKI","CHUSTECZKI", nazwy[[i]])
        nazwy[i] <- gsub("CHUSTECZKI CZKA","CHUSTECZKI", nazwy[[i]])
        nazwy[i] <- gsub("HOMOG.","HOMOGENIZOWANY ", nazwy[[i]])
        nazwy[i] <- gsub("SMAKU KU","SMAKU", nazwy[[i]])
        nazwy[i] <- gsub("W PANIERZE","W PANIERCE", nazwy[[i]])
        nazwy[i] <- gsub("EDAL EXP.","EDAL EXPRESOWA ", nazwy[[i]])
        nazwy[i] <- gsub("EDAL EXP","EDAL EXPRESOWA ", nazwy[[i]])
        nazwy[i] <- gsub("TABLETKI MUS.","TABLETKI MUSUJĄCE ", nazwy[[i]])
        nazwy[i] <- gsub("MIOD PODRAVKA","MIÓD PODRAVKA", nazwy[[i]])
        nazwy[i] <- gsub("SER TW.","SER TWARDY ", nazwy[[i]])
        nazwy[i] <- gsub("CYGARETKI RETKI","CYGARETKI", nazwy[[i]])
        nazwy[i] <- gsub("BEZ DOD. CUKIERKI","BEZ DOD. CUKRU", nazwy[[i]])
        nazwy[i] <- gsub("WHISKAS D/K","WHISKAS DLA KOTA ", nazwy[[i]])
        nazwy[i] <- gsub("KARMA SHEBA D/K","KARMA SHEBA DLA KOTA ", nazwy[[i]])
        nazwy[i] <- gsub("KARMA DREAMIES D/K","KARMA DREAMIES DLA KOTA ", nazwy[[i]])
        nazwy[i] <- gsub("KARMA DARLING D/K.","KARMA DARLING DLA KOTA ", nazwy[[i]])
        nazwy[i] <- gsub("KARMA PERFECT FIT D/K.","KARMA PERFECT FIT DLA KOTA ", nazwy[[i]])
        nazwy[i] <- gsub("KARMA GAMA D/K","KARMA GAMA DLA KOTA", nazwy[[i]])
        nazwy[i] <- gsub("ONE ADULT D/K.","ONE ADULT DLA KOTA", nazwy[[i]])
        nazwy[i] <- gsub("KARMA FRISKIES D/K","KARMA FRISKIES DLA KOTA", nazwy[[i]])
        nazwy[i] <- gsub("KARMA WHISK.D/K","KARMA WHISKAS DLA KOTA", nazwy[[i]])
        nazwy[i] <- gsub(" WP "," WIEPRZ.", nazwy[[i]])
        nazwy[i] <- gsub("BEZ CUKIERKI","BEZ CUKRU", nazwy[[i]])
        nazwy[i] <- gsub("B/CUKIERKI","BEZ CUKRU", nazwy[[i]])
        nazwy[i] <- gsub("BEZ.DOD.CUKIERKI","BEZ CUKRU", nazwy[[i]])
        nazwy[i] <- gsub("KROJONYNY ","KROJONY", nazwy[[i]])
        #nazwy[i] <- gsub("","", nazwy[[i]])
        #nazwy[i] <- gsub("","", nazwy[[i]])
        
        
        
        cat(" Kolumna nr: ", i,"z", ncol(nazwy), "została opracowana\n")
        cat("Zakończono...\n\n\n")
        
      }
      
      if(zapis==TRUE){
        
        cat("Rozpoczęto proces zapisu do pliku...\n")
        #write.csv(x = nazwy, file = lokalizacja_zapisu)
        writexl::write_xlsx(nazwy, plik_do_zapisu,col_names = F)
        
        cat("Zapisano plik w lokalizacji ", plik_do_zapisu)
      }
      
      
      nazwy[1:16]
      
    }
    
    
    
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <<- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      nazwy(plik_do_poprawy = inFile$datapath, zapis = F)
      
    })
    
    observeEvent(input$do, {
      
      c <- choose.dir()
      c <- gsub("\\\\","/", c)
      
      
      
      nazwy(plik_do_poprawy = inFile$datapath, plik_do_zapisu = paste(paste(c),"/",inFile$name, sep=""), zapis = T)
      
      
      output$zapis <-  renderText({3
        
        paste("Zapisano plik w lokalizacji: ", c)
      })
      
      
    })
    
  }
  
  # Run the application
  shinyApp(ui = ui, server = server, )
  
}

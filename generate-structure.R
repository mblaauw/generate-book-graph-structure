PUBLISHERS = c('12 Provinciën','Allert de Lange','De Amsterdamsche Keurkamer','AnkhHermes','De Arbeiderspers','Ambo/Anthos uitgevers','Ars Aequi','Athenaeum', '- Polak & Van Gennep','Atlas','De Banier','Bert Bakker','De Bezige Bij','Bohn Stafleu van Loghum','BoomBornmeer','A.W. Bruna Uitgevers','Business Contact','BZZTôH','Callenbach','Colomba','Conserve','Contact','Van Dale','De Contrabas','Eburon','Reed Elsevier','Epsilon Uitgaven','Erven J. Bijleveld','Extrapool','Gereformeerde Bijbelstichting','De Geus UitgeverijDe Harmonie','Van Holkema & Warendorf','IJzer','Home Academy Publishers','Importantia Publishing','IOS Press','Katholieke Bijbelstichting','Uitgeverij kleine Uil','Kluitman','Kosmos Uitgevers','Lemniscaat','Literaire Uitgeverij De Beuk','Luitingh-Sijthoff','Medema','Media Business Press','Moria','Mouria','NDC VBK','Nieuw Amsterdam','Nijgh & Van Ditmar','Olympus non-fictie','G.A. van Oorschot','PandaPassage','PCM Uitgevers','Pegasus','Podium','Prometheus','Querido','Rothschild & Bach','SduServire','Spectrum Uitgeverij','Stichting Historie der Techniek', '- Stiel Uitgeverij','Uitgeverij Holland','SUN','SWP Uitgeverij','Telegraaf Media Groep','Teologia','Terra Lannoo','THB –Thieme','Tirion','Triona Pers','Vassallucci','L.J. Veen Uitgeverij','Venus','VNU','Waanders','WEKA Uitgeverij','Wereldbibliotheek','Wolters Kluwer','Uitgeverij Zwijsen')
WRITERS = c('Bertus Aafjes','Han B. Aalberse *Johannes van Keulen*','Thomas van Aalten','Justa Abbing','Kader Abdolah *Hossein Sadjadi Ghaemmaghami Farahani*','Rogier van Aerde','Jan van Aken','Joseph Alberdingk Thijm','Albert Alberts','Arnold Aletrino','Jo van Ammers-Küller','Mark van Andel','Peter van Andel','Threes Anna *Threes Schreurs*','Milo Anstadt','René Appel','Saskia Appel','Jan Arends','Frank Martinus Arion','Armando *Herman Dirk van Dodeweerd*','Appie Baantjer','Inge Bak','Gerbrand Bakker','Johan Ballegeer','Steven Barends','Benno Barnard','Nicolaas Beets','Kees van Beijnum','Belcampo *Herman Pieter Schönfeld Wichers*','Abdelkader Benali','Arie van den Berg','Walter van den Berg','H.C. ten Berge','Marjan Berk','J. Bernlef *Hendrik Jan Marsman*','Hanna Bervoets','Adelheid van Beuningen','Huub Beurskens','Xander Michiel Beute','Martien Beversluis','Naima El Bezaz','Wim de Bie','Maarten Biesheuvel','Willem Bilderdijk','Alfred Birney','Anna Blaman','Jakobus Cornelis Bloem *J.C. Bloem*','Marion Bloem','Jan Blokker','Esther Blom','Herman Pieter de Boer','Jo Boer','Elle Gerrit Bolhuis','Godfried Bomans','Oscar van den Boogaard','Alex Boogers','Graa Boomsma','Johan de Boose','Ferdinand Bordewijk','F. van den Bosch','Iris Boter','Hafid Bouazza','Ina Boudier-Bakker','Mies Bouhuys','Roos Boum','Beitske Bouwman','Matthijs van Boxsel','Marian Boyer')

MHmakeRandomString <- function(n=1, lenght=12) {
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

generateRandomBookStats <- function(n) {
  for (i in 1:n) {
    
    print(i)
    
    # book_title
    book_title = MHmakeRandomString(n=1, lenght=16)
    book_author = WRITERS[sample(1:length(WRITERS),1)]
    book_publisher = PUBLISHERS[sample(1:length(PUBLISHERS),1)]
    
    # generate date
    first <- as.POSIXct(strptime("2007/01/01", "%Y/%m/%d"))
    last <- as.POSIXct(strptime("2014/05/01", "%Y/%m/%d"))
    dt <- last-first

    # Generate pub date
    book_date <- first + (as.numeric(dt) * runif(1, 25e1, 75000))
    
    # generate descriptive stats
    book_words = sample(10000:90000,1)
    book_letters = round(book_words / sample(1:12,1))
    book_sylables = round(book_words / sample(1:6,1))
        
    # readability stats
    book_r1 = runif(1, 2.0, 25.0)
    book_r2 = runif(1, 2.0, 25.0)
    book_r3 = runif(1, 2.0, 25.0)
    book_r4 = runif(1, 2.0, 25.0)
    
    # lexical diversity stats stats
    book_l1 = runif(1, 2.0, 20.0)
    book_l2 = runif(1, 2.0, 20.0)
    book_l3 = runif(1, 2.0, 20.0)
    book_l4 = runif(1, 2.0, 20.0)
    
    # rankin 
    book_rank1 = runif(1, 0.0, 10.0)
    
    result =    data.frame(book_title, book_author, book_publisher,
                           book_words, book_letters, book_sylables,
                           book_r1, book_r2, book_r3, book_r4,
                           book_l1, book_l2, book_l3, book_l4,
                           book_rank1)
 
    # build output set   
    if (exists("final_result") == T) {
      final_result = rbind(final_result, result)
    } else {
      final_result = result
    }

  }
  names(final_result) = c('title', 'author','publisher',
                          'words','letters','sylables',
                          'r1','r2','r3','r4',
                          'l1','l2','l3','l4',
                          'rank')
  
  return(final_result)
}

generateSimilarity <- function(df_books) {
  
  sourceList = df_books
  targetList = df_books
  
  df_result = data.frame(stringsAsFactors=FALSE, source='x', target='x', similar=0)
  
  for (i in 1:nrow(sourceList)) {
    for (j in 1:nrow(targetList)) {
      if (i != j) {
        df_result[nrow(df_result)+1,] = c(as.character(sourceList[i,]), as.character(targetList[j,]), runif(1, 0.0000, 0.9400))
        print (sourceList[i,])
      }
    }
  } 
  # clean first row before return result
  df_result = df_result[-1,]
  
  return(df_result)
}


BOOKS = generateRandomBookStats(n=50)
BOOKS_SIM = generateSimilarity(data.frame(BOOKS$title))

write.csv(BOOKS_SIM,file='book_similarity.csv')

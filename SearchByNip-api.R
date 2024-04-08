library(httr)
library(xml2)

# Function to login to BIR11 API
login_to_bir11 <- function(username) {
  url <- "https://wyszukiwarkaregon.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc"
  
  # SOAP request body for login
  soap_body <- sprintf('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/PUBL/2014/07">
                          <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing"> 
                          <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To> 
                          <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/Zaloguj</wsa:Action> 
                          </soap:Header>
                          <soap:Body>
                            <ns:Zaloguj>
                              <ns:pKluczUzytkownika>%s</ns:pKluczUzytkownika> 
                            </ns:Zaloguj>
                          </soap:Body>
                        </soap:Envelope>', username)
  
  # Make the SOAP request
  response <- POST(url, 
                   body = soap_body, 
                   encode = "raw",
                   add_headers("Content-Type" = "application/soap+xml;charset=UTF-8"))
  
  
  # Parse the response
  if (response$status == 200) {
    print(content(response, "text"))
    soap_response <- content(response, "text", encoding = "UTF-8")
    
    session_id <- regmatches(soap_response, regexpr("(?<=<ZalogujResult>)(.*?)(?=</ZalogujResult>)", soap_response, perl=TRUE))
    
    return(session_id)
  } else {
    stop("Error: Unable to login to BIR11 API.")
  }
}

extract_content <- function(text, tag) {
  pattern <- paste0("<", tag, ">(.*?)</", tag, ">")
  matches <- regmatches(text, gregexpr(pattern, text))
  
  return(matches[[1]])
}

# Function to search for company data by NIP
search_company_by_nip <- function(session_id, nip) {
  url <- "https://wyszukiwarkaregon.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc"
  
  # SOAP request body for company search
  soap_body <- sprintf('<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" 
                        xmlns:ns="http://CIS/BIR/PUBL/2014/07" 
                        xmlns:dat="http://CIS/BIR/PUBL/2014/07/DataContract">
                          <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing"> 
                          <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To> 
                          <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DaneSzukajPodmioty</wsa:Action> 
                          </soap:Header>
                          <soap:Body>
                            <ns:DaneSzukajPodmioty> 
                              <ns:pParametryWyszukiwania> <!--Optional:--> 
                              <dat:Nip>%s</dat:Nip> 
                          </ns:pParametryWyszukiwania> 
                        </ns:DaneSzukajPodmioty>
                          </soap:Body>
                        </soap:Envelope>', nip)
  
  # Make the SOAP request
  response <- POST(url, 
                   add_headers("Content-Type" = "application/soap+xml;charset=UTF-8", 
                               "sid" = session_id), 
                   body = soap_body)
  
  # Parse the response
  if (response$status == 200) {
    soap_response <- rawToChar(response$content)
    
    # Extract content within <DaneSzukajPodmiotyResult> tags
    result <- extract_content(soap_response, "DaneSzukajPodmiotyResult")
    
    # Convert the extracted content from HTML entities to plain text
    result_plain_text <- xml2::xml_text(xml2::read_html(result))
    
    return(result_plain_text)
  } else {
    stop("Error: Unable to search for company data.")
  }
}

extract_values <- function(text, tag){
  pattern <- paste0("<", tag, ">(.*?)</", tag, ">")
  tags_remove <-  paste("<", tag, ">|</", tag, ">", sep = "")
  matches <- regmatches(text, gregexpr(pattern, text))
  string_without_tags <- gsub(tags_remove, "", matches[[1]])
  return(string_without_tags)
}

full_report <- function(session_id, regon, typ) {
  url <- "https://wyszukiwarkaregon.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc"
  
  if(typ == "F"){
    full_report_body <- sprintf(
      '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/PUBL/2014/07">
    <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
    <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To>
    <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DanePobierzPelnyRaport
    </wsa:Action> </soap:Header>
    <soap:Body>
    <ns:DanePobierzPelnyRaport>
    <ns:pRegon>%s</ns:pRegon>
    <ns:pNazwaRaportu>BIR11OsFizycznaDzialalnoscCeidg</ns:pNazwaRaportu>
    </ns:DanePobierzPelnyRaport> </soap:Body> </soap:Envelope>', regon
    )
  }else{
    full_report_body <- sprintf(
      '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/PUBL/2014/07">
    <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
    <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To>
    <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DanePobierzPelnyRaport
    </wsa:Action> </soap:Header>
    <soap:Body>
    <ns:DanePobierzPelnyRaport>
    <ns:pRegon>%s</ns:pRegon>
    <ns:pNazwaRaportu>BIR11OsPrawna</ns:pNazwaRaportu>
    </ns:DanePobierzPelnyRaport> </soap:Body> </soap:Envelope>', regon
    )
  }
  
  
  response <- POST(url, 
                   add_headers("Content-Type" = "application/soap+xml;charset=UTF-8", 
                               "sid" = session_id), 
                   body = full_report_body)
  

  # Parse the response
  if (response$status == 200) {
    soap_response <- rawToChar(response$content)
    
    # Extract content within <DaneSzukajPodmiotyResult> tags
    result <- extract_content(soap_response, "DanePobierzPelnyRaportResult")
    
    # Convert the extracted content from HTML entities to plain text
    result_plain_text <- xml2::xml_text(xml2::read_html(result))
    
    return(result_plain_text)
  } else {
    stop("Error: Unable to search for company data.")
  }
}


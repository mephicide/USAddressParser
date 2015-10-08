package org.vickery.addressparser

import scala.util.parsing.combinator.RegexParsers

/**
 * 123 S main st. town state
 * Created by vickery on 9/17/15.
 */
class AddressParser(val city: Option[String], val state: Option[String], val zip: Option[Integer], val ignoreEverythingPriorToPostDirMode: Boolean = false) extends RegexParsers{

  override val skipWhitespace = false
  override val whiteSpace = """[ \t\n\r,.]""".r

  def fractionalNumber: Parser[String] = """[0-9]+/[0-9]+""".r

  def standaloneStreetNumber: Parser[String] = punctuatedNumber | individualNumber

  def completeStreetNumber: Parser[String] = (individualNumber~fractionalNumber | standaloneStreetNumber) ^^ {
    case indiv~frac => indiv + " " + frac
    case standalone: String => standalone
  }

  def punctuatedNumber: Parser[String] = """[0-9]+\.[0-9]+|[0-9]+\-[0-9]+[a-zA-Z]?|[0-9]+\-[a-zA-Z]|[0-9]+[a-zA-Z0-9]+|[a-zA-Z][0-9]+[a-zA-Z0-9]+""".r
  def individualNumber: Parser[String] = """[0-9]+""".r

  def directionality: Parser[String] =  multiDirectional | singleDirectional

  def singleDirectional: Parser[String] = """(?i)(North|South|East|West|N|E|S)""".r

  def multiDirectional: Parser[String] = """(?i)(NE|NW|SE|SW|Northeast|Northwest|Southeast|Southwest)""".r

  def streetName: Parser[String] = log(rep1sep(log(not(streetType) ~> regularName)("notStreetRegName"), whiteSpace))("repeatedName") <~ log(streetType)("ignoredType") ^^ {
    case matched: List[String] => matched.mkString(" ")
  }

  def streetType: Parser[String] = """(?i)((ALLEE|ALLEY|ALLY|ALY|ANEX|ANNEX|ANNX|ANX|ARCADE|ARC|AVENUE|AVENU|AVEN|AVE|AVNUE|AVN|AV|BAYOO|BAYOU|BCH|BEACH|BEND|BGS|BG|BLFS|BLF|BLUFFS|BLUFF|BLUF|BLVD|BND|BOTTM|BOTTOM|BOT|BOULEVARD|BOULV|BOUL|BRANCH|BRDGE|BRG|BRIDGE|BRKS|BRK|BRNCH|BROOKS|BROOK|BR|BTM|BURGS|BURG|BYPASS|BYPAS|BYPA|BYPS|BYP|BYU|CAMP|CANYN|CANYON|CAPE|CAUSEWAY|CAUSWA|CENTERS|CENTER|CENTRE|CENTR|CENT|CEN|CIRCLES|CIRCLE|CIRCL|CIRC|CIRS|CIR|CLB|CLFS|CLF|CLIFFS|CLIFF|CLUB|CMNS|CMN|CMP|CNTER|CNTR|CNYN|COMMONS|COMMON|CORNERS|CORNER|CORS|COR|COURSE|COURTS|COURT|COVES|COVE|CPE|CP|CRCLE|CRCL|CREEK|CRESCENT|CREST|CRES|CRK|CROSSING|CROSSROADS|CROSSROAD|CRSENT|CRSE|CRSNT|CRSSNG|CRST|CSWY|CTRS|CTR|CTS|CT|CURVE|CURV|CVS|CV|CYN|DALE|DAM|DIVIDE|DIV|DL|DM|DRIVES|DRIVE|DRIV|DRS|DRV|DR|DVD|DV|ESTATES|ESTATE|ESTS|EST|EXPRESSWAY|EXPRESS|EXPR|EXPW|EXPY|EXP|EXTENSIONS|EXTENSION|EXTNSN|EXTN|EXTS|EXT|FALLS|FALL|FERRY|FIELDS|FIELD|FLATS|FLAT|FLDS|FLD|FLS|FLTS|FLT|FORDS|FORD|FORESTS|FOREST|FORGES|FORGE|FORG|FORKS|FORK|FORT|FRDS|FRD|FREEWAY|FREEWY|FRGS|FRG|FRKS|FRK|FRRY|FRST|FRT|FRWAY|FRWY|FRY|FT|FWY|GARDENS|GARDEN|GARDN|GATEWAY|GATEWY|GATWAY|GDNS|GDN|GLENS|GLEN|GLNS|GLN|GRDEN|GRDNS|GRDN|GREENS|GREEN|GRNS|GRN|GROVES|GROVE|GROV|GRVS|GRV|GTWAY|GTWY|HARBORS|HARBOR|HARBR|HARB|HAVEN|HBRS|HBR|HEIGHTS|HIGHWAY|HIGHWY|HILLS|HILL|HIWAY|HIWY|HLLW|HLS|HL|HOLLOWS|HOLLOW|HOLWS|HOLW|HRBOR|HTS|HT|HVN|HWAY|HWY|INLET|INLT|ISLANDS|ISLAND|ISLES|ISLE|ISLNDS|ISLND|ISS|IS|JCTION|JCTNS|JCTN|JCTS|JCT|JUNCTIONS|JUNCTION|JUNCTN|JUNCTON|KEYS|KEY|KNLS|KNL|KNOLLS|KNOLL|KNOL|KYS|KY|LAKES|LAKE|LANDING|LAND|LANE|LCKS|LCK|LDGE|LDG|LF|LGTS|LGT|LIGHTS|LIGHT|LKS|LK|LNDG|LNDNG|LN|LOAF|LOCKS|LOCK|LODGE|LODG|LOOPS|LOOP|MALL|MANORS|MANOR|MDWS|MDW|MEADOWS|MEADOW|MEDOWS|MEWS|MILLS|MILL|MISSION|MISSN|MLS|ML|MNRS|MNR|MNTAIN|MNTNS|MNTN|MNT|MOTORWAY|MOUNTAINS|MOUNTAIN|MOUNTIN|MOUNT|MSN|MSSN|MTIN|MTNS|MTN|MTWY|MT|NCK|NECK|OPAS|ORCHARD|ORCHRD|ORCH|OVAL|OVERPASS|OVL|PARKS|PARKWAYS|PARKWAY|PARKWY|PARK|PASSAGE|PASS|PATHS|PATH|PIKES|PIKE|PINES|PINE|PKWAY|PKWYS|PKWY|PKY|PLACE|PLAINS|PLAIN|PLAZA|PLNS|PLN|PLZA|PLZ|PL|PNES|PNE|POINTS|POINT|PORTS|PORT|PRAIRIE|PRK|PRR|PRTS|PRT|PR|PSGE|PTS|PT|RADIAL|RADIEL|RADL|RAD|RAMP|RANCHES|RANCH|RAPIDS|RAPID|RDGE|RDGS|RDG|RDS|RD|REST|RIDGES|RIDGE|RIVER|RIVR|RIV|RNCHS|RNCH|ROADS|ROAD|ROUTE|ROW|RPDS|RPD|RST|RTE|RUE|RUN|RVR|SHLS|SHL|SHOALS|SHOAL|SHOARS|SHOAR|SHORES|SHORE|SHRS|SHR|SKWY|SKYWAY|SMT|SPGS|SPG|SPNGS|SPNG|SPRINGS|SPRING|SPRNGS|SPRNG|SPURS|SPUR|SQRE|SQRS|SQR|SQS|SQUARES|SQUARE|SQU|SQ|STATION|STATN|STA|STN|STRAVENUE|STRAVEN|STRAVN|STRAV|STRA|STREAM|STREETS|STREET|STREME|STRM|STRT|STRVNUE|STRVN|STR|STS|ST|SUMITT|SUMIT|SUMMIT|TERRACE|TERR|TER|THROUGHWAY|TPKE|TRACES|TRACE|TRACKS|TRACK|TRAFFICWAY|TRAILER|TRAILS|TRAIL|TRAK|TRCE|TRFY|TRKS|TRK|TRLRS|TRLR|TRLS|TRL|TRNPK|TRWY|TUNEL|TUNLS|TUNL|TUNNELS|TUNNEL|TUNNL|TURNPIKE|TURNPK|UNDERPASS|UNIONS|UNION|UNS|UN|UPAS|VALLEYS|VALLEY|VALLY|VDCT|VIADCT|VIADUCT|VIA|VIEWS|VIEW|VILLAGES|VILLAGE|VILLAG|VILLE|VILLG|VILLIAGE|VILL|VISTA|VIST|VIS|VLGS|VLG|VLLY|VLYS|VLY|VL|VSTA|VST|VWS|VW|WALKS|WALK|WALL|WAYS|WAY|WELLS|WELL|WLS|WL|WY|XING|XRDS|XRD){1,2})""".r

  def cityMatch: Parser[String] = regularName

  def regularName: Parser[String] = """(?i)([a-zA-Z\-0-9]+)""".r

  def stateMatch: Parser[String] = """(?i)(alabama|al|alaska|ak|americansamoa|as|arizona|az|arkansas|ar|california|ca|colorado|co|connecticut|ct|delaware|de|districtofcolumbia|dc|washingtondistrictofcolumbia|washingtondc|federatedstatesofmicronesia|fm|florida|fl|georgia|ga|guam|gu|hawaii|hi|idaho|id|illinois|il|indiana|in|iowa|ia|kansas|ks|kentucky|ky|louisiana|la|maine|me|marshallislands|mh|maryland|md|massachusetts|ma|michigan|mi|minnesota|mn|mississippi|ms|missouri|mo|montana|mt|nebraska|ne|nevada|nv|newhampshire|nh|newjersey|nj|newmexico|nm|newyork|ny|northcarolina|nc|northdakota|nd|northernmarianaislands|mp|ohio|oh|oklahoma|ok|oregon|or|palau|pw|pennsylvania|pa|puertorico|pr|rhodeisland|ri|southcarolina|sc|southdakota|sd|tennessee|tn|texas|tx|utah|ut|vermont|vt|virginislands|vi|virginia|va|washington|wa|westvirginia|wv|wisconsin|wi|wyoming|wy|ae|armedforcespacific|ap|armedforcesamericas|aa)""".r

  def zipMatch: Parser[Integer] = """[0-9]{5}""".r ^^ {case matched: String => matched.toInt}

  def secondary: Parser[String] = """(?i)(Apartment|APT|Basement|BSMT|Building|BLDG|Department|DEPT|Floor|FL|Front|FRNT|Hanger|HNGR|Key|KEY|Lobby|LBBY|Lot|LOT|Lower|LOWR|Office|OFC|Penthouse|PH|Pier|PIER|Rear|REAR|Room|RM|Side|SIDE|Slip|SLIP|Space|SPC|Stop|STOP|Suite|STE|Trailer|TRLR|Unit|UNIT|Upper|UPPR|Box|#)""".r

  def correctForNamelessStreet(addressTokens: List[String], lastSuccessfulInd: Integer, build: Address): Option[String] = {
    val finalInd = List.range(0,addressTokens.length).foldLeft(addressTokens.length)((cum, cur) => {
      if(build.postDirection.isDefined){
        if(build.postDirection.get.toLowerCase.startsWith(addressTokens(cur).toLowerCase)) {
          if(cur < cum)
            cur
          else
            cum
        }
        else
          cum
      }
      else if(build.internalNumber.isDefined && build.internalNumber.get.toLowerCase.startsWith(addressTokens(cur).toLowerCase)) {
        if (cur < cum)
          cur
        else
          cum
      }
      else {
        cum
      }
    })

    if(finalInd==addressTokens.length){
     if(ignoreEverythingPriorToPostDirMode)
       Some(addressTokens.slice(lastSuccessfulInd+1, finalInd).mkString(" ").trim)
     else
       None
    }
    else
      Some(addressTokens.slice(lastSuccessfulInd+1, finalInd).mkString(" ").trim)
  }

  def buildFinalAddress(currentBuild: Address,
                        addressTokens: List[String],
                        lastSuccessfulInd: Integer,
                        previousParseInd: Integer): Option[Address] = {

    val streetNameSlice = addressTokens.slice(lastSuccessfulInd+1, previousParseInd)
    if(
    (streetNameSlice.count((el) => parseAll(individualNumber, el).successful)>1)) {
      None
    }
    else if(streetNameSlice.isEmpty && (currentBuild.internalNumber.isDefined || currentBuild.postDirection.isDefined || ignoreEverythingPriorToPostDirMode)) {
      val newStName = correctForNamelessStreet(addressTokens, lastSuccessfulInd, currentBuild)
      if(newStName.isDefined)
        Some(currentBuild.withStreetName(newStName.get))
      else
        None
    }else if(streetNameSlice.isEmpty) {
      None
    }
    else {
      Some(currentBuild.withStreetName(streetNameSlice.mkString(" ").trim))
    }
  }

  def parseForwardWithFSM(addressTokens: List[String],
                          currentInd: Integer,
                          lastSuccessfulInd: Integer,
                          currentBuild: Address,
                          previousParseInd: Integer,
                          currentState: AddressFSMState): Option[Address] = {

    currentState match {
      case START => {
        val streetNumToken = getTwoTokenItem(reverse = true, addressTokens, currentInd, individualNumber, fractionalNumber, standaloneStreetNumber, noOpValidator)
        if(streetNumToken.isDefined) {
            val nextBuild = currentBuild.withStreetNumber(streetNumToken.get._1)
            parseForwardWithFSM(addressTokens,streetNumToken.get._2+1, streetNumToken.get._2, nextBuild, previousParseInd, LookingForPreDirectional)
        }
        else
          None
      }
      case LookingForPreDirectional => {
        val preDirToken = getTwoTokenItem(true, addressTokens, currentInd, singleDirectional, singleDirectional, directionality, directionValidator)
        if(preDirToken.isDefined) {
          if(preDirToken.get._2 == (previousParseInd-1)) { //then we consumed the entire name
            if(currentInd == preDirToken.get._2) { //then we only have a direction and it has to be the street name
              buildFinalAddress(currentBuild, addressTokens, currentInd-1, previousParseInd)
            }
            else { //then we have a directional and the actual street name
              val nextBuild = currentBuild.withPreDirection(addressTokens(currentInd))
              buildFinalAddress(nextBuild, addressTokens, currentInd, previousParseInd)
            }
          }
          else {
            val nextBuild = currentBuild.withPreDirection(preDirToken.get._1)
            buildFinalAddress(nextBuild, addressTokens, preDirToken.get._2, previousParseInd)
          }
        }
        else {
          buildFinalAddress(currentBuild, addressTokens, lastSuccessfulInd, previousParseInd)
        }
      }
      case _ => None
    }
  }

  def parseWithDefiniteRemainder(str: String, definiteAddress: Address): Option[Address] = {
    val splits = str.split(whiteSpace.regex).filter(!_.isEmpty).toList
    if(splits.length<2)
      None
    else {
      if(splits.length >=3 &&
        (splits(0) equalsIgnoreCase "PO") &&
        (splits(1) equalsIgnoreCase "BOX")) {

        val stType = splits(0) + " " + splits(1)
        Some(definiteAddress.withStreetNumber(splits(2)).withStreetName(stType))
      }
      else {
        val streetNumToken = getTwoTokenItem(reverse = true, splits, 0, individualNumber, fractionalNumber, standaloneStreetNumber, noOpValidator)
        if(streetNumToken.isDefined) {
          Some(definiteAddress.withStreetNumber(streetNumToken.get._1).withStreetName(splits.slice(streetNumToken.get._2+1, splits.length).mkString(" ")))
        }
        else
          None
      }
    }
  }

  /**
   * PO Box 2345 city city state zip OR
   * 2345 PO Box city city state zip
   * @param splitList
   */
  def parsePOBox(splitList: List[String], currentBuild: Address) = {
    if(ignoreEverythingPriorToPostDirMode) {
      if(splitList.length >= 3) {
        if(splitList(0).matches("""[0-9]+""") && (splitList(1).equalsIgnoreCase("po")  || splitList(1).equalsIgnoreCase("p.o.")) && splitList(2).equalsIgnoreCase("box")) {
          Some(currentBuild.
            withStreetNumber(splitList(0)).
            withstreetType("PO Box"))

        } else if(splitList(2).matches("""[0-9]+""") && (splitList(0).equalsIgnoreCase("po")  || splitList(0).equalsIgnoreCase("p.o.")) && splitList(1).equalsIgnoreCase("box") && splitList(splitList.length-1).forall(_.isDigit)) {
          Some(currentBuild.
            withStreetNumber(splitList(2)).
            withstreetType("PO Box"))
        }
        else {
          None
        }
      }
      else None
    }
    else {
      if(splitList.length >= 6) {
        if(splitList(0).matches("""[0-9]+""") && (splitList(1).equalsIgnoreCase("po")  || splitList(1).equalsIgnoreCase("p.o.")) && splitList(2).equalsIgnoreCase("box")) {
          Some(new Address().
            withStreetNumber(splitList(0)).
            withstreetType("PO Box").
            withZip(splitList(splitList.length-1).toInt).
            withState(splitList(splitList.length-2)).
            withCity(splitList.slice(3, splitList.length-2).mkString(" ")))

        } else if(splitList(2).matches("""[0-9]+""") && (splitList(0).equalsIgnoreCase("po")  || splitList(0).equalsIgnoreCase("p.o.")) && splitList(1).equalsIgnoreCase("box") && splitList(splitList.length-1).forall(_.isDigit)) {
          Some(new Address().
            withStreetNumber(splitList(2)).
            withstreetType("PO Box").
            withZip(splitList(splitList.length-1).toInt).
            withState(splitList(splitList.length-2)).
            withCity(splitList.slice(3, splitList.length-2).mkString(" ")))
        }
        else {
          None
        }
      }
      else None
    }
  }

  def parseWithKnownCityStateZip(str: String) = {
    var splitList = Address.stateReplace(str).split("""[\s,]""").filter(x => !x.isEmpty)

    val startingAddr = new Address().withCity(city.get).withState(state.get).withZip(zip.get)

    val poBox = parsePOBox(splitList.toList, startingAddr)
    if(poBox.isDefined)
    {
      Some(poBox.get)
    }
    else {
      splitList = Address.stateReplace(str).split("""[\s,.]""").filter(x => !x.isEmpty)
      val reverseResult = parseWithFSM(splitList.reverse.toList, startingAddr, 0, -1, LookingForPostDirectional)
      if (reverseResult.isDefined) {
        parseForwardWithFSM(splitList.toList, 0, -1, reverseResult.get._1, (splitList.length - (reverseResult.get._2 + 1)), START)
      }
      else
        parseForwardWithFSM(splitList.toList, 0, -1, startingAddr, splitList.length, START)
    }
  }

  def parse(str: String): Option[Address] = {
    val splitList = Address.stateReplace(str).split("""[\s,.]""").filter(x => !x.isEmpty)

    val poBox = parsePOBox(splitList.toList, new Address())
    if(poBox.isDefined)
    {
      Some(poBox.get)
    }
    else
    {
      val reverseParseResult = parseWithFSM(splitList.reverse.toList, new Address(), currentInd = 0, lastSuccessfulInd = -1, START)
      if(reverseParseResult.isDefined) {
        parseForwardWithFSM(splitList.toList, currentInd = 0, lastSuccessfulInd = -1, previousParseInd = (splitList.length - (reverseParseResult.get._2 +1)), currentBuild = reverseParseResult.get._1, currentState = START)
      }
      else if(city.isDefined && state.isDefined && zip.isDefined){
        val reverseResult = parseWithFSM(splitList.reverse.toList, new Address().withCity(city.get).withState(state.get).withZip(zip.get), 0, -1, LookingForPostDirectional)
        if(reverseResult.isDefined) {
          parseForwardWithFSM(splitList.toList, 0, -1, reverseResult.get._1, (splitList.length - (reverseResult.get._2+1)), START)
        }
        else
          None
      }
      else
        None
    }
  }

  def getTwoTokenItem(reverse: Boolean,
                      addressTokens: List[String],
                      currentInd: Integer,
                      validator1: Parser[String],
                      validator2: Parser[String],
                      completeValidator: Parser[String],
                      twoParseValidation: (String, String) => Boolean): Option[(String,Integer)] = {

    val firstTok = parseAll(validator1, addressTokens(currentInd))
    if(firstTok.successful && currentInd<(addressTokens.length-1)) {
      val secondTok = parseAll(validator2, addressTokens(currentInd+1))
      if(secondTok.successful) {
        var continue: Boolean = false
        if(reverse)
          continue = twoParseValidation(firstTok.get, secondTok.get)
        else
          continue = twoParseValidation(secondTok.get, firstTok.get)

        if(continue) {
          if(reverse)
            Some((firstTok.get + " " + secondTok.get, currentInd+1)).asInstanceOf[Option[(String, Integer)]]
          else
            Some((secondTok.get + " " + firstTok.get, currentInd+1)).asInstanceOf[Option[(String, Integer)]]
        }
        else {
          finishTwoTokenItem(addressTokens, completeValidator, currentInd)
        }
      }
      else {
        finishTwoTokenItem(addressTokens, completeValidator, currentInd)
      }
    }
    else {
      finishTwoTokenItem(addressTokens, completeValidator, currentInd)
    }
  }

  def finishTwoTokenItem(addressTokens: List[String], completeValidator: Parser[String], currentInd: Integer): Option[(String, Integer)] = {
    val completeTok = parseAll(completeValidator, addressTokens(currentInd))
    if(completeTok.successful) {
      Some((completeTok.get, currentInd))
    }
    else
      None //this should be impossbile
  }

  def parseWithFSM(addressTokens: List[String], currentBuild: Address, currentInd: Integer, lastSuccessfulInd: Integer, currentState: AddressFSMState): Option[(Address, Integer)] = {
    if(currentInd>=addressTokens.length)
      None
    else
      currentState match {
        case START => {
          val currentTok = addressTokens(currentInd)
          val parsed = parseAll(zipMatch, currentTok)
          if(parsed.successful)
            parseWithFSM(addressTokens, currentBuild.withZip(parsed.get), currentInd+1, currentInd, LookingForState)
          else if(zip.isDefined)
            parseWithFSM(addressTokens, currentBuild.withZip(zip.get), currentInd,lastSuccessfulInd, LookingForState)
          else
            None
        }
        case LookingForState => {
          val currentTok = addressTokens(currentInd)
          val parsed = parseAll(stateMatch, currentTok)
          if(parsed.successful)
            parseWithFSM(addressTokens,currentBuild.withState(addressTokens(currentInd)), currentInd+1, currentInd, LookingForCity)
          else if (state.isDefined)
            parseWithFSM(addressTokens, currentBuild.withState(state.get), currentInd,lastSuccessfulInd, LookingForCity)
          else
            None
        }
        case LookingForCity => {
          val currentTok = addressTokens(currentInd)
          val parsed = getTwoTokenItem(reverse = false, addressTokens, currentInd, """[a-zA-Z0-9\-]+""".r, secondary, secondary, noOpValidator)//parseAll(secondary, currentTok)
          val beginningInd = lastSuccessfulInd + 1
          if(parsed.isDefined) {
            if((currentInd - beginningInd) >=1) {
              val nextBuild = currentBuild.withInternalNumber(parsed.get._1).withCity(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim)
              parseWithFSM(addressTokens, nextBuild, parsed.get._2+1, parsed.get._2, LookingForPostDirectional)
            }
            else if (city.isDefined || (currentBuild.canonicalState equals "DC")){
              //then we have "Suite 1234 MD" or "Bldg A"
              val nextBuild = currentBuild.withCity(city.getOrElse("DC")).withInternalNumber(parsed.get._1)
              parseWithFSM(addressTokens, nextBuild, parsed.get._2+1, parsed.get._2, LookingForPostDirectional)
            }
            else
              None
          }
          else {
            //the current token was not a secondary address indicator.  Try post directional
            val parsedPostDir = parseAll(directionality, currentTok)
            if(parsedPostDir.successful) {
              if((currentInd - beginningInd) >= 1) {
                val dirPortion = getTwoTokenItem(reverse = false, addressTokens, currentInd,  singleDirectional, singleDirectional, directionality, directionValidator)
                if(dirPortion.isDefined) {
                  val nextBuild = currentBuild.withPostDirection(dirPortion.get._1).withCity(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim)
                  parseWithFSM(addressTokens, nextBuild, dirPortion.get._2+1, dirPortion.get._2, LookingForHighwayNumber)
                }
                else
                  None
              }
              else if(city.isDefined || (currentBuild.canonicalState equals "DC")){
                val dirPortion = getTwoTokenItem(reverse = false, addressTokens, currentInd,  singleDirectional, singleDirectional, directionality, directionValidator)
                if(dirPortion.isDefined) {
                  val nextBuild = currentBuild.withCity(city.getOrElse("DC")).withPostDirection(dirPortion.get._1)
                  parseWithFSM(addressTokens, nextBuild, dirPortion.get._2+1, dirPortion.get._2, LookingForHighwayNumber)
                }
                else
                  None
              }
              else {
                None
              }
            }
            else {
              val parsedHighwayNumber = parseAll(standaloneStreetNumber|"""[a-zA-Z]{1}""".r, currentTok)
              if (parsedHighwayNumber.successful) {
                if(!(currentInd==addressTokens.length-1) && parseAll(streetType, addressTokens(currentInd+1)).successful) { //make sure if we find a number that the street type is next
                  if ((currentInd - beginningInd) >= 1) {
                    val nextBuild = currentBuild.withHighwayNumber(addressTokens(currentInd)).withCity(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim)
                    parseWithFSM(addressTokens, nextBuild, currentInd + 1, currentInd, LookingForStreetType)
                  }
                  else if (city.isDefined || (currentBuild.canonicalState equals "DC")) {
                    val nextBuild = currentBuild.withCity(city.getOrElse("DC")).withHighwayNumber(addressTokens(currentInd))
                    parseWithFSM(addressTokens, nextBuild, currentInd + 1, currentInd, LookingForStreetType)
                  }
                  else {
                    None
                  }
                }
                else {
                  parseWithFSM(addressTokens, currentBuild, currentInd + 1, lastSuccessfulInd, LookingForCity)
                }
              }
              else
              {
                val parsedStreetType = parseAll(streetType, currentTok)
                if (parsedStreetType.successful) {
                  if ((currentInd - beginningInd) >= 1) {
                    val nextBuild = currentBuild.withstreetType(addressTokens(currentInd)).withCity(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim)
                    Some((nextBuild, currentInd))
                  }
                  else if (city.isDefined || (currentBuild.canonicalState equals "DC")) {
                    val nextBuild = currentBuild.withCity(city.getOrElse("DC")).withstreetType(addressTokens(currentInd))
                    Some((nextBuild, currentInd))
                  }
                  else {
                    None
                  }
                }
                else {
                  //this must be a regular part of the city
                  parseWithFSM(addressTokens, currentBuild, currentInd + 1, lastSuccessfulInd, LookingForCity)
                }
              }
            }
          }
        }
        case LookingForPostDirectional => {
          val currentTok = addressTokens(currentInd)
          val parsed = parseAll(directionality, currentTok)
          if(parsed.successful) {
            val dirPortion = getTwoTokenItem(reverse = false, addressTokens, currentInd,  singleDirectional, singleDirectional, directionality, directionValidator)
            if(dirPortion.isDefined) {
              val nextBuild = currentBuild.withPostDirection(dirPortion.get._1)
              parseWithFSM(addressTokens, nextBuild, dirPortion.get._2+1, dirPortion.get._2, LookingForHighwayNumber)
            }
            else
              None
          }
          else {
            parseWithFSM(addressTokens, currentBuild, currentInd, lastSuccessfulInd, LookingForHighwayNumber)
          }
        }
        case LookingForHighwayNumber => {
          val currentTok = addressTokens(currentInd)
          val parsedHighwayNumber = getTwoTokenItem(false, addressTokens, currentInd,standaloneStreetNumber|"""[a-zA-Z]""".r, streetType, (standaloneStreetNumber|"""[a-zA-Z]""".r)~streetType ^^ { case first~second => first}, noOpValidator)
         // val parsedHighwayNumber = parseAll(standaloneStreetNumber|"""[a-zA-Z]""".r, currentTok)
          if (parsedHighwayNumber.isDefined) {
            val nextBuild = currentBuild.withHighwayNumber(parsedHighwayNumber.get._1)
            parseWithFSM(addressTokens, nextBuild, currentInd+1, currentInd, LookingForStreetType)
          }
          else
            parseWithFSM(addressTokens, currentBuild, currentInd, lastSuccessfulInd, LookingForStreetType)
        }
        case LookingForStreetType => {
          val currentTok = addressTokens(currentInd)
          val parsed = parseAll(streetType, currentTok)
          if(parsed.successful) {
            val nextBuild = currentBuild.withstreetType(addressTokens(currentInd))
            Some((nextBuild, currentInd))
          }
          else {
            if(ignoreEverythingPriorToPostDirMode)
              parseWithFSM(addressTokens, currentBuild, currentInd+1, lastSuccessfulInd, LookingForPostDirectional)
            else
              Some((currentBuild, lastSuccessfulInd))
          }
        }
        case _ => Some(currentBuild, lastSuccessfulInd)
      }
  }

  def directionValidator: (String, String) => Boolean = {
    (first, second) => {
      first.toLowerCase match {
        case "north" => ((second.toLowerCase equals "east") || (second.toLowerCase equals "west"))
        case "south" => ((second.toLowerCase equals "east") || (second.toLowerCase equals "west"))
        case _ => false
      }
    }
  }

  def noOpValidator: (String, String) => Boolean = {
    (first, second) => true
  }
}

sealed abstract class AddressFSMState

case object START extends AddressFSMState
case object LookingForState extends AddressFSMState
case object LookingForCity extends AddressFSMState
case object LookingForPostDirectional extends AddressFSMState
case object LookingForHighwayNumber extends AddressFSMState
case object LookingForStreetType extends AddressFSMState
case object LookingForPreDirectional extends AddressFSMState
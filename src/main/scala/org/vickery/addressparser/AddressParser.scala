package org.vickery.addressparser

import fastparse.Utils._
import fastparse.core.ParseCtx
import fastparse.core.Parsed.Success

//import fastparse.core.{ParseCtx, Parser}

/**
 * 123 S main st. town state
 * Created by vickery on 9/17/15.
 */
class AddressParser(var city: Option[String], var state: Option[String], var zip: Option[Integer], var ignoreEverythingPriorToPostDirMode: Boolean = false){

  import fastparse.all._

  //override val skipWhitespace = false
  val whiteSpace = CharIn(List('\t', '\n', ' ')).rep(1)

  val fractionalNumber: Parser[String] = P(individualNumber ~ "/" ~ individualNumber) map {case (s1, s2) => s1 + "/" + s2}

  val standaloneStreetNumber: Parser[String] = P(punctuatedNumber | individualNumber)

  val completeStreetNumber: Parser[String] = P(((individualNumber~whiteSpace~fractionalNumber) map { case (s1,s2) => s1 + s2 }) | standaloneStreetNumber)

  val punctuatedNumber: Parser[String] = (P(individualNumber ~ "." ~ individualNumber) map {case(s1,s2) => s1+"."+s2}) |
                                          (P(individualNumber ~ "-" ~ individualNumber ~ CharIn('a'to'z', 'A'to'Z').rep(1).?.!) map {case (s1, s2, s3) => s1+"-"+s2+s3}) |
                                          (P(individualNumber ~ "-" ~ CharIn('a'to'z', 'A'to'Z').rep(1).!) map {case (s1,s2) => s1+ "-" +s2}) |
                                          (P(individualNumber ~ CharIn('a'to'z', 'A'to'Z', '0'to'9').rep(1).!) map {case(s1,s2) => s1+s2}) |
                                          (P(CharIn('a'to'z', 'A'to'Z').! ~ individualNumber ~ CharIn('a'to'z', 'A'to'Z', '0'to'9').rep(1).!) map {case (s1,s2,s3) => s1+s2+s3})

  val individualNumber: Parser[String] = P(CharIn('0'to'9').rep(1).!)

  val directionality: Parser[String] =  P(multiDirectional | singleDirectional)

  case class StringInIgnoreCase(strings: String*) extends Parser[Unit]{

    private[this] val trie = new TrieNode(strings.map(_.toLowerCase))

    def parseRec(cfg: ParseCtx, index: Int) = {
      val length = trie.query(cfg.input.toLowerCase, index)
      if (length != -1) success(cfg.success, (), index + length + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = {
      s"StringIn(${strings.map(literalize(_)).mkString(", ")})"
    }
  }

  val singleDirectional: Parser[String] = P(StringInIgnoreCase("North","South","East","West","N","E","S","W").!)

  val multiDirectional: Parser[String] = StringInIgnoreCase("NE","NW","SE","SW","Northeast","Northwest","Southeast","Southwest").!

  val streetName: Parser[String] = P(!streetType ~ regularName).rep(1, whiteSpace) ~ whiteSpace ~ streetType map {
    case (ss, s) => ss.mkString(" ") + " " + s
  }

  val streetType: Parser[String] = StringInIgnoreCase("ALLEE","ALLEY","ALLY","ALY","ANEX","ANNEX","ANNX","ANX","ARCADE","ARC","AVENUE","AVENU","AVEN","AVE","AVNUE","AVN","AV","BAYOO","BAYOU","BCH","BEACH","BEND","BGS","BG","BLFS","BLF","BLUFFS","BLUFF","BLUF","BLVD","BND","BOTTM","BOTTOM","BOT","BOULEVARD","BOULV","BOUL","BRANCH","BRDGE","BRG","BRIDGE","BRKS","BRK","BRNCH","BROOKS","BROOK","BR","BTM","BURGS","BURG","BYPASS","BYPAS","BYPA","BYPS","BYP","BYU","CAMP","CANYN","CANYON","CAPE","CAUSEWAY","CAUSWA","CENTERS","CENTER","CENTRE","CENTR","CENT","CEN","CIRCLES","CIRCLE","CIRCL","CIRC","CIRS","CIR","CLB","CLFS","CLF","CLIFFS","CLIFF","CLUB","CMNS","CMN","CMP","CNTER","CNTR","CNYN","COMMONS","COMMON","CORNERS","CORNER","CORS","COR","COURSE","COURTS","COURT","COVES","COVE","CPE","CP","CRCLE","CRCL","CREEK","CRESCENT","CREST","CRES","CRK","CROSSING","CROSSROADS","CROSSROAD","CRSENT","CRSE","CRSNT","CRSSNG","CRST","CSWY","CTRS","CTR","CTS","CT","CURVE","CURV","CVS","CV","CYN","DALE","DAM","DIVIDE","DIV","DL","DM","DRIVES","DRIVE","DRIV","DRS","DRV","DR","DVD","DV","ESTATES","ESTATE","ESTS","EST","EXPRESSWAY","EXPRESS","EXPR","EXPW","EXPY","EXP","EXTENSIONS","EXTENSION","EXTNSN","EXTN","EXTS","EXT","FALLS","FALL","FERRY","FIELDS","FIELD","FLATS","FLAT","FLDS","FLD","FLS","FLTS","FLT","FORDS","FORD","FORESTS","FOREST","FORGES","FORGE","FORG","FORKS","FORK","FORT","FRDS","FRD","FREEWAY","FREEWY","FRGS","FRG","FRKS","FRK","FRRY","FRST","FRT","FRWAY","FRWY","FRY","FT","FWY","GARDENS","GARDEN","GARDN","GATEWAY","GATEWY","GATWAY","GDNS","GDN","GLENS","GLEN","GLNS","GLN","GRDEN","GRDNS","GRDN","GREENS","GREEN","GRNS","GRN","GROVES","GROVE","GROV","GRVS","GRV","GTWAY","GTWY","HARBORS","HARBOR","HARBR","HARB","HAVEN","HBRS","HBR","HEIGHTS","HIGHWAY","HIGHWY","HILLS","HILL","HIWAY","HIWY","HLLW","HLS","HL","HOLLOWS","HOLLOW","HOLWS","HOLW","HRBOR","HTS","HT","HVN","HWAY","HWY","INLET","INLT","INTERSTATE","ISLANDS","ISLAND","ISLES","ISLE","ISLNDS","ISLND","ISS","IS","JCTION","JCTNS","JCTN","JCTS","JCT","JUNCTIONS","JUNCTION","JUNCTN","JUNCTON","KEYS","KEY","KNLS","KNL","KNOLLS","KNOLL","KNOL","KYS","KY","LAKES","LAKE","LANDING","LAND","LANE","LCKS","LCK","LDGE","LDG","LF","LGTS","LGT","LIGHTS","LIGHT","LKS","LK","LNDG","LNDNG","LN","LOAF","LOCKS","LOCK","LODGE","LODG","LOOPS","LOOP","MALL","MANORS","MANOR","MDWS","MDW","MEADOWS","MEADOW","MEDOWS","MEWS","MILLS","MILL","MISSION","MISSN","MLS","ML","MNRS","MNR","MNTAIN","MNTNS","MNTN","MNT","MOTORWAY","MOUNTAINS","MOUNTAIN","MOUNTIN","MOUNT","MSN","MSSN","MTIN","MTNS","MTN","MTWY","MT","NCK","NECK","OPAS","ORCHARD","ORCHRD","ORCH","OVAL","OVERPASS","OVL","PARKS","PARKWAYS","PARKWAY","PARKWY","PARK","PASSAGE","PASS","PATHS","PATH","PIKES","PIKE","PINES","PINE","PKWAY","PKWYS","PKWY","PKY","PLACE","PLAINS","PLAIN","PLAZA","PLNS","PLN","PLZA","PLZ","PL","PNES","PNE","POINTS","POINT","PORTS","PORT","PRAIRIE","PRK","PRR","PRTS","PRT","PR","PSGE","PTS","PT","RADIAL","RADIEL","RADL","RAD","RAMP","RANCHES","RANCH","RAPIDS","RAPID","RDGE","RDGS","RDG","RDS","RD","REST","RIDGES","RIDGE","RIVER","RIVR","RIV","RNCHS","RNCH","ROADS","ROAD","ROUTE","ROW","RPDS","RPD","RST","RTE","RUE","RUN","RVR","SHLS","SHL","SHOALS","SHOAL","SHOARS","SHOAR","SHORES","SHORE","SHRS","SHR","SKWY","SKYWAY","SMT","SPGS","SPG","SPNGS","SPNG","SPRINGS","SPRING","SPRNGS","SPRNG","SPURS","SPUR","SQRE","SQRS","SQR","SQS","SQUARES","SQUARE","SQU","SQ","STATION","STATN","STA","STN","STRAVENUE","STRAVEN","STRAVN","STRAV","STRA","STREAM","STREETS","STREET","STREME","STRM","STRT","STRVNUE","STRVN","STR","STS","ST","SUMITT","SUMIT","SUMMIT","TERRACE","TERR","TER","THROUGHWAY","TPKE","TRACES","TRACE","TRACKS","TRACK","TRAFFICWAY","TRAILER","TRAILS","TRAIL","TRAK","TRCE","TRFY","TRKS","TRK","TRLRS","TRLR","TRLS","TRL","TRNPK","TRWY","TUNEL","TUNLS","TUNL","TUNNELS","TUNNEL","TUNNL","TURNPIKE","TURNPK","UNDERPASS","UNIONS","UNION","UNS","UN","UPAS","VALLEYS","VALLEY","VALLY","VDCT","VIADCT","VIADUCT","VIA","VIEWS","VIEW","VILLAGES","VILLAGE","VILLAG","VILLE","VILLG","VILLIAGE","VILL","VISTA","VIST","VIS","VLGS","VLG","VLLY","VLYS","VLY","VL","VSTA","VST","VWS","VW","WALKS","WALK","WALL","WAYS","WAY","WELLS","WELL","WLS","WL","WY","XING","XRDS","XRD").!

  val cityMatch: Parser[String] = regularName

  val regularName: Parser[String] = P(CharIn('a'to'z', '0'to'9', 'A'to'Z', List('-')).rep(1).!) map {case ss => ss.mkString("")}

  val stateMatch: Parser[String] = StringInIgnoreCase("alabama","al","alaska","ak","americansamoa","as","arizona","az","arkansas","ar","california","ca","colorado","co","connecticut","ct","delaware","de","districtofcolumbia","dc","washingtondistrictofcolumbia","washingtondc","federatedstatesofmicronesia","fm","florida","fl","georgia","ga","guam","gu","hawaii","hi","idaho","id","illinois","il","indiana","in","iowa","ia","kansas","ks","kentucky","ky","louisiana","la","maine","me","marshallislands","mh","maryland","md","massachusetts","ma","michigan","mi","minnesota","mn","mississippi","ms","missouri","mo","montana","mt","nebraska","ne","nevada","nv","newhampshire","nh","newjersey","nj","newmexico","nm","newyork","ny","northcarolina","nc","northdakota","nd","northernmarianaislands","mp","ohio","oh","oklahoma","ok","oregon","or","palau","pw","pennsylvania","pa","puertorico","pr","rhodeisland","ri","southcarolina","sc","southdakota","sd","tennessee","tn","texas","tx","utah","ut","vermont","vt","virginislands","vi","virginia","va","washington","wa","westvirginia","wv","wisconsin","wi","wyoming","wy","ae","armedforcespacific","ap","armedforcesamericas","aa").!

  val zipMatch: Parser[Integer] = P(CharIn('0'to'9').! ~ CharIn('0'to'9').! ~ CharIn('0'to'9').! ~ CharIn('0'to'9').! ~ CharIn('0'to'9').!) map { case (d1,d2,d3,d4,d5) => (d1+d2+d3+d4+d5).toInt}

  val secondary: Parser[String] = P(StringInIgnoreCase("Apartment","APT","Basement","BSMT","Building","BLDG","Department","DEPT","Floor","FL","Front","FRNT","Hanger","HNGR","Key","KEY","Lobby","LBBY","Lot","LOT","Lower","LOWR","Office","OFC","Penthouse","PH","Pier","PIER","Rear","REAR","Room","RM","Side","SIDE","Slip","SLIP","Space","SPC","Stop","STOP","Suite","STE","Trailer","TRLR","Unit","UNIT","Upper","UPPR","Box").! | P("#" ~ CharIn('a'to'z','A'to'Z','0'to'9', List('-')).rep(1)).! | P("#").!)

  val highwayNumber: Parser[String] = P(standaloneStreetNumber | CharIn('a'to'z', 'A'to'Z').!)

  val highwayInfoComplete: Parser[String] = P(highwayNumber.! ~ whiteSpace ~ streetType.!) map {
    case (hn, st) => hn
  }

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
      else if(build.highwayNumber.isDefined && build.streetType.get.toLowerCase.startsWith(addressTokens(cur).toLowerCase)) {
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

  def switchPredirectionAndStreetName(currentBuild: Address): Option[Address] = {
    Some(new Address(currentBuild.streetNum,
                      currentBuild.internalNumber,
                      currentBuild.preDirection,
                      currentBuild.streetType,
                      currentBuild.streetName,
                      currentBuild.postDirection,
                      currentBuild.city,
                      currentBuild.state,
                      currentBuild.zip,
                      currentBuild.highwayNumber))
  }

  def correctForHighwayWithDirectionalName(add: Address): Option[Address] = {
    if(add.streetName.isDefined) {
      if(successful(directionality.parse(add.streetName.get), add.streetName.get) &&
          add.highwayNumber.isDefined) {
        switchPredirectionAndStreetName(add)
      }
      else
        Some(add)
    }
    else
      Some(add)
  }

  def applyFinalCorrections(result: Option[Address]): Option[Address] = {
    if(result.isDefined) {
      correctForHighwayWithDirectionalName(result.get)
    }
    else
      result
  }

  def buildFinalAddress(currentBuild: Address,
                        addressTokens: List[String],
                        lastSuccessfulInd: Integer,
                        previousParseInd: Integer): Option[Address] = {

    val streetNameSlice = addressTokens.slice(lastSuccessfulInd+1, previousParseInd)
    var result: Option[Address] = None
    if(
    (streetNameSlice.count((el) => successful(individualNumber.parse(el), el))>1)) {
      result = None
    }
    else if(!(currentBuild.streetNum.isDefined) || currentBuild.streetNum.get.matches("[0.]+"))
    {
      result = None
    }
    else if(streetNameSlice.isEmpty && (currentBuild.internalNumber.isDefined || currentBuild.postDirection.isDefined || ignoreEverythingPriorToPostDirMode)) {
      val newStName = correctForNamelessStreet(addressTokens, lastSuccessfulInd, currentBuild)
      if(newStName.isDefined){
        if(newStName.get.isEmpty && currentBuild.preDirection.isDefined || currentBuild.postDirection.isDefined || successful(streetType.parse(newStName.get.toUpperCase), newStName.get))
            result = switchPredirectionAndStreetName(currentBuild)
        else
          result = Some(currentBuild.withStreetName(newStName.get.trim))
      }
      else
        result = None
    }else if(streetNameSlice.isEmpty) {
      result = None
    }
    else {
      if(streetNameSlice(0).matches("[a-zA-Z]") && streetNameSlice.length > 1) {
        //this might be a 123 A Main St. case ...
        val newStreetNum = s"${currentBuild.streetNum.get}-${streetNameSlice(0)}"
        val newStreetName = streetNameSlice.slice(1, streetNameSlice.length).mkString(" ").trim
        result = Some(currentBuild.withStreetNumber(newStreetNum).withStreetName(newStreetName))
      }
      else {
        result = Some(currentBuild.withStreetName(streetNameSlice.mkString(" ").trim))
      }
    }

    return applyFinalCorrections(result)
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
           /* if(currentInd == preDirToken.get._2) { //then we only have a direction and it has to be the street name
              buildFinalAddress(currentBuild, addressTokens, currentInd-1, previousParseInd)
            }
            else { *///then we have a directional and the actual street name
              val nextBuild = currentBuild.withPreDirection(addressTokens(currentInd))
              buildFinalAddress(nextBuild, addressTokens, currentInd, previousParseInd)
            //}
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
    val splits = str.split("""[\r\n\t ]""").filter(!_.isEmpty).toList
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

  def createNewAddress = new Address().withCity(city.getOrElse("")).withState(state.getOrElse("")).withZip(zip.getOrElse(-1))

  private def cleanAddressNum(s: String): String = if(s.matches("[0-9]+[.][00]+")) s.substring(0, s.indexOf(".")) else s

  def parseWithKnownCityStateZip(str: String) = {
    if(str.trim.isEmpty)
      None
    else {
      var splitList = Address.stateReplace(str).split("""[\s,]""").filter(x => !x.isEmpty)

      if(splitList.length<=1){
        None
      }
      else {
        val startingAddr = createNewAddress

        val poBox = parsePOBox(splitList.toList, startingAddr)
        if(poBox.isDefined)
        {
          Some(poBox.get)
        }
        else {
          splitList = str.toLowerCase.split("""[\s,]""").filter(x => !x.isEmpty && !(x.trim.replaceAll("0+", "").isEmpty))
          val splitListNew = cleanAddressNum(splitList(0)):: splitList.slice(1, splitList.length).map(_.replaceAll("[.]", "")).toList
          val reverseResult = if(ignoreEverythingPriorToPostDirMode)
                                  parseWithFSM(splitListNew.reverse, startingAddr, 0, -1, LookingForPostDirectional)
                                else
                                  parseWithFSM(splitListNew.reverse, startingAddr, 0, -1, LookingForCity)

          if (reverseResult.isDefined) {
            parseForwardWithFSM(splitListNew, 0, -1, reverseResult.get._1, (splitList.length - (reverseResult.get._2 + 1)), START)
          }
          else
            parseForwardWithFSM(splitList.toList, 0, -1, createNewAddress, splitList.length, START)
        }
      }
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

    if(currentInd>=addressTokens.length)
      None
    else
    {
      val firstTok = validator1.parse(addressTokens(currentInd))
      if(successful(firstTok, addressTokens(currentInd)) && currentInd<(addressTokens.length-1)) {
        val secondTok = validator2.parse(addressTokens(currentInd+1))
        if(successful(secondTok, addressTokens(currentInd+1))) {
          var continue: Boolean = false
          if(reverse)
            continue = twoParseValidation(firstTok.get.value, secondTok.get.value)
          else
            continue = twoParseValidation(secondTok.get.value, firstTok.get.value)

          if(continue) {
            if(reverse)
              Some((firstTok.get.value + " " + secondTok.get.value, currentInd+1)).asInstanceOf[Option[(String, Integer)]]
            else
              Some((secondTok.get.value + " " + firstTok.get.value, currentInd+1)).asInstanceOf[Option[(String, Integer)]]
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

  }

  def finishTwoTokenItem(addressTokens: List[String], completeValidator: Parser[String], currentInd: Integer): Option[(String, Integer)] = {
    val completeTok = completeValidator.parse(addressTokens(currentInd))
    if(successful(completeTok, addressTokens(currentInd))) {
      Some((completeTok.get.value, currentInd))
    }
    else
      None //this should be impossbile
  }

  def successful(parsed: Parsed[_], currentTok: String): Boolean = parsed.isInstanceOf[Success[_]] && parsed.index==currentTok.length

  def parseWithFSM(addressTokens: List[String], currentBuild: Address, currentInd: Integer, lastSuccessfulInd: Integer, currentState: AddressFSMState): Option[(Address, Integer)] = {
    if(currentInd>=addressTokens.length)
      None
    else
      currentState match {
        case START => {
          val currentTok = addressTokens(currentInd)
          val parsed = zipMatch.parse(currentTok)
          if(successful(parsed, currentTok))
            parseWithFSM(addressTokens, currentBuild.withZip(parsed.get.value), currentInd+1, currentInd, LookingForState)
          else if(zip.isDefined)
            parseWithFSM(addressTokens, currentBuild.withZip(zip.get), currentInd,lastSuccessfulInd, LookingForState)
          else
            None
        }
        case LookingForState => {
          val currentTok = addressTokens(currentInd)
          val parsed = stateMatch.parse(currentTok)
          if(successful(parsed, currentTok))
            parseWithFSM(addressTokens,currentBuild.withState(addressTokens(currentInd)), currentInd+1, currentInd, LookingForCity)
          else if (state.isDefined)
            parseWithFSM(addressTokens, currentBuild.withState(state.get), currentInd,lastSuccessfulInd, LookingForCity)
          else
            None
        }
        case LookingForCity => {
          val currentTok = addressTokens(currentInd)
          val parsed = getTwoTokenItem(reverse = false, addressTokens, currentInd, regularName, secondary, secondary, noOpValidator)//parseAll(secondary, currentTok)
          val beginningInd = lastSuccessfulInd + 1
          if(parsed.isDefined) {
            if((currentInd - beginningInd) >=1) {
              val nextBuild = currentBuild.withInternalNumber(parsed.get._1).withCity(city.getOrElse(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim))
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
            val parsedPostDir = directionality.parse(currentTok)
            if(successful(parsedPostDir, currentTok)) {
              if((currentInd - beginningInd) >= 1) {
                val dirPortion = getTwoTokenItem(reverse = false, addressTokens, currentInd,  singleDirectional, singleDirectional, directionality, directionValidator)
                if(dirPortion.isDefined) {
                  val nextBuild = currentBuild.withPostDirection(dirPortion.get._1).withCity(city.getOrElse(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim))
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
              val parsedHighwayNumber = highwayNumber.parse(currentTok)
              if (successful(parsedHighwayNumber, currentTok)) {
                if(!(currentInd==addressTokens.length-1) && successful(streetType.parse(addressTokens(currentInd+1)), addressTokens(currentInd+1))) { //make sure if we find a number that the street type is next
                  if ((currentInd - beginningInd) >= 1) {
                    val nextBuild = currentBuild.withHighwayNumber(addressTokens(currentInd)).withCity(city.getOrElse(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim))
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
                val parsedStreetType = streetType.parse(currentTok)
                if (successful(parsedStreetType, currentTok)) {
                  if ((currentInd - beginningInd) >= 1) {
                    val nextBuild = currentBuild.withstreetType(addressTokens(currentInd)).withCity(city.getOrElse(addressTokens.slice(beginningInd, currentInd).reverse.mkString(" ").trim))
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
          val parsed = directionality.parse(currentTok)
          if(successful(parsed, currentTok) && currentInd < addressTokens.length-2) { //can't be a post dir if there isn't at least a number and a name as well
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
          val parsedHighwayNumber = getTwoTokenItem(false, addressTokens, currentInd,highwayNumber, streetType, highwayNumber.! ~ whiteSpace ~ streetType.! map { case (first, second) => first}, noOpValidator)
         // val parsedHighwayNumber = parseAll(standaloneStreetNumber|"""[a-zA-Z]""".r, currentTok)
          if (parsedHighwayNumber.isDefined) {
            val splits = parsedHighwayNumber.get._1.split("\\s+")
            if(splits.length==0)
              None
            else if(splits.length==1){
              val nextBuild = currentBuild.withHighwayNumber(splits(0))
              parseWithFSM(addressTokens, nextBuild, currentInd+1, currentInd, LookingForStreetType)
            }
            else {
              val nextBuild = currentBuild.withHighwayNumber(splits(1))
              parseWithFSM(addressTokens, nextBuild, currentInd+1, currentInd, LookingForStreetType)
            }
          }
          else
            parseWithFSM(addressTokens, currentBuild, currentInd, lastSuccessfulInd, LookingForStreetType)
        }
        case LookingForStreetType => {
          val currentTok = addressTokens(currentInd)
          val parsed = streetType.parse(currentTok)
          if(successful(parsed, currentTok)) {
            val nextBuild = currentBuild.withstreetType(addressTokens(currentInd))
            Some((nextBuild, currentInd))
          }
          else {
            if(ignoreEverythingPriorToPostDirMode && !currentBuild.postDirection.isDefined)
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
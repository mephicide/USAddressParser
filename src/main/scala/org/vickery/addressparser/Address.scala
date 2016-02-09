package org.vickery.addressparser

/**
 * For parsing addresses into well-formed objects.
 *
 * 123 main st. NW mytown NH 11111
 * 123 main st. mytown
 * Created by vickery on 9/17/15.
 */
case class Address(val streetNum: Option[String],
              val internalNumber: Option[String],
              val streetName: Option[String],
              val streetType: Option[String],
              val preDirection: Option[String],
                   val postDirection: Option[String],
               val city: Option[String],
               val state: Option[String],
               val zip: Option[Integer],
                    val highwayNumber: Option[String]) {

  def this() = this(None, None, None,None,None,None,None,None,None,None)

  def withStreetNumber(newNum: String) = new Address(Some(newNum), internalNumber, streetName, streetType, preDirection, postDirection, city, state, zip, highwayNumber)
  def withInternalNumber(newNum: String) = new Address(streetNum, Some(newNum), streetName, streetType, preDirection, postDirection, city, state, zip, highwayNumber)
  def withStreetName(newSN: String) = new Address(streetNum, internalNumber, Some(newSN), streetType, preDirection, postDirection, city, state, zip, highwayNumber)
  def withHighwayNumber(newNum: String) = new Address(streetNum, internalNumber, streetName, streetType, preDirection, postDirection, city, state, zip, Some(newNum))
  def withstreetType(newType: String) = new Address(streetNum, internalNumber, streetName, Some(newType), preDirection, postDirection, city, state, zip, highwayNumber)
  def withPreDirection(newPre: String) = new Address(streetNum, internalNumber, streetName, streetType, Some(newPre), postDirection, city, state, zip, highwayNumber)
  def withPostDirection(newPost: String) = new Address(streetNum, internalNumber, streetName, streetType, preDirection, Some(newPost), city, state, zip, highwayNumber)
  def withCity(newCity: String) = new Address(streetNum, internalNumber, streetName, streetType, preDirection, postDirection, Some(newCity), state, zip, highwayNumber)
  def withState(newState: String) = new Address(streetNum, internalNumber, streetName, streetType, preDirection, postDirection, city, Some(newState), zip, highwayNumber)
  def withZip(newZip: Integer) = new Address(streetNum, internalNumber, streetName, streetType, preDirection, postDirection, city, state, if(newZip == -1) None else Some(newZip), highwayNumber)

  override def toString = "Num(" + streetNum.getOrElse("") + ") " +
    "PreDir(" + preDirection.getOrElse("null") + ") " +
    "Name(" + streetName.getOrElse("null") + ") " +
    "Type(" + streetType.getOrElse("null") + ") " +
    "HWY(" + highwayNumber.getOrElse("null") + ") " +
    "PostDir(" + postDirection.getOrElse("null") + ") " +
    "Secondary(" + internalNumber.getOrElse("null") + ") " +
    "City(" + city.getOrElse("null") + ") " +
    "State(" + state.getOrElse("null") + ") " +
    "Zip(" + zip.getOrElse("null") + ")"

  def canonicalStreetNumber = streetNum.get

  def canonicalDirection(dir: Option[String]) = {
    val key = dir.getOrElse("").replaceAll("\\s", "").toLowerCase
    directionIndexByName.getOrElse(key, key.toUpperCase)
  }

  def canonicalPreDirection = canonicalDirection(preDirection)

  def canonicalName(name: Option[String]) = {
    name.getOrElse("").split("""\s""").filter(!_.isEmpty).map(_.toLowerCase.capitalize).mkString(" ")
  }

  def canonicalStreetName = {
    val almost = canonicalName(streetName)
    if(almost.matches("""[0-9]+"""))
      (almost.last+"") match {
        case "0" => almost + "th"
        case "1" => almost + "st"
        case "2" => almost + "nd"
        case "3" => almost + "rd"
        case "4" => almost + "th"
        case "5" => almost + "th"
        case "6" => almost + "th"
        case "7" => almost + "th"
        case "8" => almost + "th"
        case "9" => almost + "th"
      }
    else
      almost
  }
  def canonicalStreetType = {
    if(streetName.isDefined && !(streetName.get.isEmpty) && streetType.isDefined && (streetName.get contains streetType.get))
      ""
    else {
      if(streetType.isDefined && streetType.get.equalsIgnoreCase("po box")) {
        "PO Box"
      }
      else {
        streetTypeMapping.getOrElse(streetType.getOrElse("").toUpperCase, "").toLowerCase.capitalize
      }
    }

  }

  def masterAddressName = {
    canonicalStreetNumber + " " +
      (if(canonicalPreDirection.isEmpty) "" else canonicalPreDirection + " ") +
      (if(canonicalStreetName.isEmpty) "" else canonicalStreetName + " ") +
      (if(canonicalStreetType.isEmpty) "" else canonicalStreetType + " ") +
      (if(canonicalHighWayNumber.isEmpty) "" else canonicalHighWayNumber + " ") +
      (if(canonicalPostDirection.isEmpty) "" else canonicalPostDirection + " ") +
      canonicalCity + " " + canonicalState + " " + canonicalZip
  }

  def canonicalPostDirection = canonicalDirection(postDirection)
  def canonicalHighWayNumber = {
    if(highwayNumber.isDefined) {
      val num = highwayNumber.get
      val splits = num.split("\\s+")
      if(splits.length>0) {
        if(splits.length==2)
          splits(1)
        else
          splits(0)
      }
      else
        ""
    }
    else
      ""
  }

  def canonicalSecondaryAddress = {
    val internalComponents = internalNumber.getOrElse("").split("\\s+").filter(!_.isEmpty)

    if(internalComponents.length==2)
      secondaryDesignatorMapping.getOrElse(internalComponents(0).toLowerCase, "") + " " + internalComponents(1)
    else if(internalComponents.length==1)
      secondaryDesignatorMapping.getOrElse(internalComponents(0).toLowerCase, "")
    else
      ""
  }

  def canonicalCity = {
    val almost = canonicalName(city)
    if((almost.isEmpty) && (canonicalState equals "DC")) "Washington" else almost
  }

  def canonicalState = {
    stateMapping.getOrElse(state.getOrElse("").toLowerCase, "")
  }

  def canonicalZip: String = {
    val almost: Integer = zip.getOrElse(-1)

    if(almost == -1)
      ""
    else if(almost < 10000)
      "0" + almost
    else
      ""+almost
  }

  def toCanonicalString = canonicalStreetNumber + " " +
                          (if(canonicalPreDirection.isEmpty) "" else (canonicalPreDirection + " ")) +
                            canonicalStreetName + " " + canonicalStreetType + " " +
                           (if(canonicalHighWayNumber.isEmpty) "" else (canonicalHighWayNumber + " ")) +
                          (if(canonicalPostDirection.isEmpty) "" else (canonicalPostDirection + " ")) +
                          (if(canonicalSecondaryAddress.isEmpty) "" else (canonicalSecondaryAddress + " ")) +
                          (if(canonicalCity equals "DC") "" else (canonicalCity + " ")) +
                          canonicalState + " " + canonicalZip

  val directionIndexByName = Map("north" -> "N",
    "south" -> "S",
    "east" -> "E",
    "west" -> "W",
    "northeast" -> "NE",
    "northwest" -> "NW",
    "southeast" -> "SE",
    "southwest" -> "SW")

  val streetTypeMapping = Map("ALLEE" -> "ALY", "ALLEY" -> "ALY", "ALLY" -> "ALY", "ALY" -> "ALY", "ALY" -> "ALY",
    "ANEX" -> "ANX", "ANNEX" -> "ANX", "ANNX" -> "ANX", "ANX" -> "ANX", "ANX" -> "ANX",
    "ARC" -> "ARC", "ARCADE" -> "ARC", "ARC" -> "ARC",
    "AV" -> "AVE", "AVE" -> "AVE", "AVEN" -> "AVE", "AVENU" -> "AVE", "AVENUE" -> "AVE", "AVN" -> "AVE", "AVNUE" -> "AVE", "AVE" -> "AVE",
    "BAYOO" -> "BYU", "BAYOU" -> "BYU", "BYU" -> "BYU",
    "BCH" -> "BCH", "BEACH" -> "BCH", "BCH" -> "BCH",
    "BEND" -> "BND", "BND" -> "BND", "BND" -> "BND",
    "BLF" -> "BLF", "BLUF" -> "BLF", "BLUFF" -> "BLF", "BLF" -> "BLF",
    "BLUFFS" -> "BLFS", "BLFS" -> "BLFS",
    "BOT" -> "BTM", "BTM" -> "BTM", "BOTTM" -> "BTM", "BOTTOM" -> "BTM", "BTM" -> "BTM",
    "BLVD" -> "BLVD", "BOUL" -> "BLVD", "BOULEVARD" -> "BLVD", "BOULV" -> "BLVD", "BLVD" -> "BLVD",
    "BR" -> "BR", "BRNCH" -> "BR", "BRANCH" -> "BR", "BR" -> "BR",
    "BRDGE" -> "BRG", "BRG" -> "BRG", "BRIDGE" -> "BRG", "BRG" -> "BRG",
    "BRK" -> "BRK", "BROOK" -> "BRK", "BRK" -> "BRK",
    "BROOKS" -> "BRKS", "BRKS" -> "BRKS",
    "BURG" -> "BG", "BG" -> "BG",
    "BURGS" -> "BGS", "BGS" -> "BGS",
    "BYP" -> "BYP", "BYPA" -> "BYP", "BYPAS" -> "BYP", "BYPASS" -> "BYP", "BYPS" -> "BYP", "BYP" -> "BYP",
    "CAMP" -> "CP", "CP" -> "CP", "CMP" -> "CP", "CP" -> "CP",
    "CANYN" -> "CYN", "CANYON" -> "CYN", "CNYN" -> "CYN", "CYN" -> "CYN",
    "CAPE" -> "CPE", "CPE" -> "CPE", "CPE" -> "CPE",
    "CAUSEWAY" -> "CSWY", "CAUSWA" -> "CSWY", "CSWY" -> "CSWY", "CSWY" -> "CSWY",
    "CEN" -> "CTR", "CENT" -> "CTR", "CENTER" -> "CTR", "CENTR" -> "CTR", "CENTRE" -> "CTR", "CNTER" -> "CTR", "CNTR" -> "CTR", "CTR" -> "CTR", "CTR" -> "CTR",
    "CENTERS" -> "CTRS", "CTRS" -> "CTRS",
    "CIR" -> "CIR", "CIRC" -> "CIR", "CIRCL" -> "CIR", "CIRCLE" -> "CIR", "CRCL" -> "CIR", "CRCLE" -> "CIR", "CIR" -> "CIR",
    "CIRCLES" -> "CIRS", "CIRS" -> "CIRS",
    "CLF" -> "CLF", "CLIFF" -> "CLF", "CLF" -> "CLF",
    "CLFS" -> "CLFS", "CLIFFS" -> "CLFS", "CLFS" -> "CLFS",
    "CLB" -> "CLB", "CLUB" -> "CLB", "CLB" -> "CLB",
    "COMMON" -> "CMN", "CMN" -> "CMN",
    "COMMONS" -> "CMNS", "CMNS" -> "CMNS",
    "COR" -> "COR", "CORNER" -> "COR", "COR" -> "COR",
    "CORNERS" -> "CORS", "CORS" -> "CORS", "CORS" -> "CORS",
    "COURSE" -> "CRSE", "CRSE" -> "CRSE", "CRSE" -> "CRSE",
    "COURT" -> "CT", "CT" -> "CT", "CT" -> "CT",
    "COURTS" -> "CTS", "CTS" -> "CTS", "CTS" -> "CTS",
    "COVE" -> "CV", "CV" -> "CV", "CV" -> "CV",
    "COVES" -> "CVS", "CVS" -> "CVS",
    "CREEK" -> "CRK", "CRK" -> "CRK", "CRK" -> "CRK",
    "CRESCENT" -> "CRES", "CRES" -> "CRES", "CRSENT" -> "CRES", "CRSNT" -> "CRES", "CRES" -> "CRES",
    "CREST" -> "CRST", "CRST" -> "CRST",
    "CROSSING" -> "XING", "CRSSNG" -> "XING", "XING" -> "XING", "XING" -> "XING",
    "CROSSROAD" -> "XRD", "XRD" -> "XRD",
    "CROSSROADS" -> "XRDS", "XRDS" -> "XRDS",
    "CURVE" -> "CURV", "CURV" -> "CURV",
    "DALE" -> "DL", "DL" -> "DL", "DL" -> "DL",
    "DAM" -> "DM", "DM" -> "DM", "DM" -> "DM",
    "DIV" -> "DV", "DIVIDE" -> "DV", "DV" -> "DV", "DVD" -> "DV", "DV" -> "DV",
    "DR" -> "DR", "DRIV" -> "DR", "DRIVE" -> "DR", "DRV" -> "DR", "DR" -> "DR",
    "DRIVES" -> "DRS", "DRS" -> "DRS",
    "EST" -> "EST", "ESTATE" -> "EST", "EST" -> "EST",
    "ESTATES" -> "ESTS", "ESTS" -> "ESTS", "ESTS" -> "ESTS",
    "EXP" -> "EXPY", "EXPR" -> "EXPY", "EXPRESS" -> "EXPY", "EXPRESSWAY" -> "EXPY", "EXPW" -> "EXPY", "EXPY" -> "EXPY", "EXPY" -> "EXPY",
    "EXT" -> "EXT", "EXTENSION" -> "EXT", "EXTN" -> "EXT", "EXTNSN" -> "EXT", "EXT" -> "EXT",
    "EXTS" -> "EXTS", "EXTS" -> "EXTS",
    "FALL" -> "FALL", "FALL" -> "FALL",
    "FALLS" -> "FLS", "FLS" -> "FLS", "FLS" -> "FLS",
    "FERRY" -> "FRY", "FRRY" -> "FRY", "FRY" -> "FRY", "FRY" -> "FRY",
    "FIELD" -> "FLD", "FLD" -> "FLD", "FLD" -> "FLD",
    "FIELDS" -> "FLDS", "FLDS" -> "FLDS", "FLDS" -> "FLDS",
    "FLAT" -> "FLT", "FLT" -> "FLT", "FLT" -> "FLT",
    "FLATS" -> "FLTS", "FLTS" -> "FLTS", "FLTS" -> "FLTS",
    "FORD" -> "FRD", "FRD" -> "FRD", "FRD" -> "FRD",
    "FORDS" -> "FRDS", "FRDS" -> "FRDS",
    "FOREST" -> "FRST", "FORESTS" -> "FRST", "FRST" -> "FRST", "FRST" -> "FRST",
    "FORG" -> "FRG", "FORGE" -> "FRG", "FRG" -> "FRG", "FRG" -> "FRG",
    "FORGES" -> "FRGS", "FRGS" -> "FRGS",
    "FORK" -> "FRK", "FRK" -> "FRK", "FRK" -> "FRK",
    "FORKS" -> "FRKS", "FRKS" -> "FRKS", "FRKS" -> "FRKS",
    "FORT" -> "FT", "FRT" -> "FT", "FT" -> "FT", "FT" -> "FT",
    "FREEWAY" -> "FWY", "FREEWY" -> "FWY", "FRWAY" -> "FWY", "FRWY" -> "FWY", "FWY" -> "FWY", "FWY" -> "FWY",
    "GARDEN" -> "GDN", "GARDN" -> "GDN", "GRDEN" -> "GDN", "GRDN" -> "GDN", "GDN" -> "GDN",
    "GARDENS" -> "GDNS", "GDNS" -> "GDNS", "GRDNS" -> "GDNS", "GDNS" -> "GDNS",
    "GATEWAY" -> "GTWY", "GATEWY" -> "GTWY", "GATWAY" -> "GTWY", "GTWAY" -> "GTWY", "GTWY" -> "GTWY", "GTWY" -> "GTWY",
    "GLEN" -> "GLN", "GLN" -> "GLN", "GLN" -> "GLN",
    "GLENS" -> "GLNS", "GLNS" -> "GLNS",
    "GREEN" -> "GRN", "GRN" -> "GRN", "GRN" -> "GRN",
    "GREENS" -> "GRNS", "GRNS" -> "GRNS",
    "GROV" -> "GRV", "GROVE" -> "GRV", "GRV" -> "GRV", "GRV" -> "GRV",
    "GROVES" -> "GRVS", "GRVS" -> "GRVS",
    "HARB" -> "HBR", "HARBOR" -> "HBR", "HARBR" -> "HBR", "HBR" -> "HBR", "HRBOR" -> "HBR", "HBR" -> "HBR",
    "HARBORS" -> "HBRS", "HBRS" -> "HBRS",
    "HAVEN" -> "HVN", "HVN" -> "HVN", "HVN" -> "HVN",
    "HT" -> "HTS", "HTS" -> "HTS", "HTS" -> "HTS",
    "HIGHWAY" -> "HWY", "HIGHWY" -> "HWY", "HIWAY" -> "HWY", "HIWY" -> "HWY", "HWAY" -> "HWY", "HWY" -> "HWY", "HWY" -> "HWY",
    "HILL" -> "HL", "HL" -> "HL", "HL" -> "HL",
    "HILLS" -> "HLS", "HLS" -> "HLS", "HLS" -> "HLS",
    "HLLW" -> "HOLW", "HOLLOW" -> "HOLW", "HOLLOWS" -> "HOLW", "HOLW" -> "HOLW", "HOLWS" -> "HOLW", "HOLW" -> "HOLW",
    "INLT" -> "INLT", "INLT" -> "INLT",
    "INTERSTATE" -> "INTERSTATE",
    "IS" -> "IS", "ISLAND" -> "IS", "ISLND" -> "IS", "IS" -> "IS",
    "ISLANDS" -> "ISS", "ISLNDS" -> "ISS", "ISS" -> "ISS", "ISS" -> "ISS",
    "ISLE" -> "ISLE", "ISLES" -> "ISLE", "ISLE" -> "ISLE",
    "JCT" -> "JCT", "JCTION" -> "JCT", "JCTN" -> "JCT", "JUNCTION" -> "JCT", "JUNCTN" -> "JCT", "JUNCTON" -> "JCT", "JCT" -> "JCT",
    "JCTNS" -> "JCTS", "JCTS" -> "JCTS", "JUNCTIONS" -> "JCTS", "JCTS" -> "JCTS",
    "KEY" -> "KY", "KY" -> "KY", "KY" -> "KY",
    "KEYS" -> "KYS", "KYS" -> "KYS", "KYS" -> "KYS",
    "KNL" -> "KNL", "KNOL" -> "KNL", "KNOLL" -> "KNL", "KNL" -> "KNL",
    "KNLS" -> "KNLS", "KNOLLS" -> "KNLS", "KNLS" -> "KNLS",
    "LK" -> "LK", "LAKE" -> "LK", "LK" -> "LK",
    "LKS" -> "LKS", "LAKES" -> "LKS", "LKS" -> "LKS",
    "LAND" -> "LAND", "LAND" -> "LAND",
    "LANDING" -> "LNDG", "LNDG" -> "LNDG", "LNDNG" -> "LNDG", "LNDG" -> "LNDG",
    "LANE" -> "LN", "LN" -> "LN", "LN" -> "LN",
    "LGT" -> "LGT", "LIGHT" -> "LGT", "LGT" -> "LGT",
    "LIGHTS" -> "LGTS", "LGTS" -> "LGTS",
    "LF" -> "LF", "LOAF" -> "LF", "LF" -> "LF",
    "LCK" -> "LCK", "LOCK" -> "LCK", "LCK" -> "LCK",
    "LCKS" -> "LCKS", "LOCKS" -> "LCKS", "LCKS" -> "LCKS",
    "LDG" -> "LDG", "LDGE" -> "LDG", "LODG" -> "LDG", "LODGE" -> "LDG", "LDG" -> "LDG",
    "LOOP" -> "LOOP", "LOOPS" -> "LOOP", "LOOP" -> "LOOP",
    "MALL" -> "MALL", "MALL" -> "MALL",
    "MNR" -> "MNR", "MANOR" -> "MNR", "MNR" -> "MNR",
    "MANORS" -> "MNRS", "MNRS" -> "MNRS", "MNRS" -> "MNRS",
    "MEADOW" -> "MDW", "MDW" -> "MDW",
    "MDW" -> "MDWS", "MDWS" -> "MDWS", "MEADOWS" -> "MDWS", "MEDOWS" -> "MDWS", "MDWS" -> "MDWS",
    "MEWS" -> "MEWS", "MEWS" -> "MEWS",
    "MILL" -> "ML", "ML" -> "ML",
    "MILLS" -> "MLS", "MLS" -> "MLS",
    "MISSN" -> "MSN", "MSSN" -> "MSN", "MSN" -> "MSN",
    "MOTORWAY" -> "MTWY", "MTWY" -> "MTWY",
    "MNT" -> "MT", "MT" -> "MT", "MOUNT" -> "MT", "MT" -> "MT",
    "MNTAIN" -> "MTN", "MNTN" -> "MTN", "MOUNTAIN" -> "MTN", "MOUNTIN" -> "MTN", "MTIN" -> "MTN", "MTN" -> "MTN", "MTN" -> "MTN",
    "MNTNS" -> "MTNS", "MOUNTAINS" -> "MTNS", "MTNS" -> "MTNS",
    "NCK" -> "NCK", "NECK" -> "NCK", "NCK" -> "NCK",
    "ORCH" -> "ORCH", "ORCHARD" -> "ORCH", "ORCHRD" -> "ORCH", "ORCH" -> "ORCH",
    "OVAL" -> "OVAL", "OVL" -> "OVAL", "OVAL" -> "OVAL",
    "OVERPASS" -> "OPAS", "OPAS" -> "OPAS",
    "PARK" -> "PARK", "PRK" -> "PARK", "PARK" -> "PARK",
    "PARKS" -> "PARK", "PARK" -> "PARK",
    "PARKWAY" -> "PKWY", "PARKWY" -> "PKWY", "PKWAY" -> "PKWY", "PKWY" -> "PKWY", "PKY" -> "PKWY", "PKWY" -> "PKWY",
    "PARKWAYS" -> "PKWY", "PKWYS" -> "PKWY", "PKWY" -> "PKWY",
    "PASS" -> "PASS", "PASS" -> "PASS",
    "PASSAGE" -> "PSGE", "PSGE" -> "PSGE",
    "PATH" -> "PATH", "PATHS" -> "PATH", "PATH" -> "PATH",
    "PIKE" -> "PIKE", "PIKES" -> "PIKE", "PIKE" -> "PIKE",
    "PINE" -> "PNE", "PNE" -> "PNE",
    "PINES" -> "PNES", "PNES" -> "PNES", "PNES" -> "PNES",
    "PL" -> "PL", "PL" -> "PL",
    "PLAIN" -> "PLN", "PLN" -> "PLN", "PLN" -> "PLN",
    "PLAINS" -> "PLNS", "PLNS" -> "PLNS", "PLNS" -> "PLNS",
    "PLAZA" -> "PLZ", "PLZ" -> "PLZ", "PLZA" -> "PLZ", "PLZ" -> "PLZ",
    "POINT" -> "PT", "PT" -> "PT", "PT" -> "PT",
    "POINTS" -> "PTS", "PTS" -> "PTS", "PTS" -> "PTS",
    "PORT" -> "PRT", "PRT" -> "PRT", "PRT" -> "PRT",
    "PORTS" -> "PRTS", "PRTS" -> "PRTS", "PRTS" -> "PRTS",
    "PR" -> "PR", "PRAIRIE" -> "PR", "PRR" -> "PR", "PR" -> "PR",
    "RAD" -> "RADL", "RADIAL" -> "RADL", "RADIEL" -> "RADL", "RADL" -> "RADL", "RADL" -> "RADL",
    "RAMP" -> "RAMP", "RAMP" -> "RAMP",
    "RANCH" -> "RNCH", "RANCHES" -> "RNCH", "RNCH" -> "RNCH", "RNCHS" -> "RNCH", "RNCH" -> "RNCH",
    "RAPID" -> "RPD", "RPD" -> "RPD", "RPD" -> "RPD",
    "RAPIDS" -> "RPDS", "RPDS" -> "RPDS", "RPDS" -> "RPDS",
    "REST" -> "RST", "RST" -> "RST", "RST" -> "RST",
    "RDG" -> "RDG", "RDGE" -> "RDG", "RIDGE" -> "RDG", "RDG" -> "RDG",
    "RDGS" -> "RDGS", "RIDGES" -> "RDGS", "RDGS" -> "RDGS",
    "RIV" -> "RIV", "RIVER" -> "RIV", "RVR" -> "RIV", "RIVR" -> "RIV", "RIV" -> "RIV",
    "RD" -> "RD", "ROAD" -> "RD", "RD" -> "RD",
    "ROADS" -> "RDS", "RDS" -> "RDS", "RDS" -> "RDS",
    "ROUTE" -> "RTE", "RTE" -> "RTE",
    "ROW" -> "ROW", "ROW" -> "ROW",
    "RUE" -> "RUE", "RUE" -> "RUE",
    "RUN" -> "RUN", "RUN" -> "RUN",
    "SHL" -> "SHL", "SHOAL" -> "SHL", "SHL" -> "SHL",
    "SHLS" -> "SHLS", "SHOALS" -> "SHLS", "SHLS" -> "SHLS",
    "SHOAR" -> "SHR", "SHORE" -> "SHR", "SHR" -> "SHR", "SHR" -> "SHR",
    "SHOARS" -> "SHRS", "SHORES" -> "SHRS", "SHRS" -> "SHRS", "SHRS" -> "SHRS",
    "SKYWAY" -> "SKWY", "SKWY" -> "SKWY",
    "SPG" -> "SPG", "SPNG" -> "SPG", "SPRING" -> "SPG", "SPRNG" -> "SPG", "SPG" -> "SPG",
    "SPGS" -> "SPGS", "SPNGS" -> "SPGS", "SPRINGS" -> "SPGS", "SPRNGS" -> "SPGS", "SPGS" -> "SPGS",
    "SPUR" -> "SPUR", "SPUR" -> "SPUR",
    "SPURS" -> "SPUR", "SPUR" -> "SPUR",
    "SQ" -> "SQ", "SQR" -> "SQ", "SQRE" -> "SQ", "SQU" -> "SQ", "SQUARE" -> "SQ", "SQ" -> "SQ",
    "SQRS" -> "SQS", "SQUARES" -> "SQS", "SQS" -> "SQS",
    "STA" -> "STA", "STATION" -> "STA", "STATN" -> "STA", "STN" -> "STA", "STA" -> "STA",
    "STRA" -> "STRA", "STRAV" -> "STRA", "STRAVEN" -> "STRA", "STRAVENUE" -> "STRA", "STRAVN" -> "STRA", "STRVN" -> "STRA", "STRVNUE" -> "STRA", "STRA" -> "STRA",
    "STREAM" -> "STRM", "STREME" -> "STRM", "STRM" -> "STRM", "STRM" -> "STRM",
    "STREET" -> "ST", "STRT" -> "ST", "ST" -> "ST", "STR" -> "ST", "ST" -> "ST",
    "STREETS" -> "STS", "STS" -> "STS",
    "SMT" -> "SMT", "SUMIT" -> "SMT", "SUMITT" -> "SMT", "SUMMIT" -> "SMT", "SMT" -> "SMT",
    "TER" -> "TER", "TERR" -> "TER", "TERRACE" -> "TER", "TER" -> "TER",
    "THROUGHWAY" -> "TRWY", "TRWY" -> "TRWY",
    "TRACE" -> "TRCE", "TRACES" -> "TRCE", "TRCE" -> "TRCE", "TRCE" -> "TRCE",
    "TRACK" -> "TRAK", "TRACKS" -> "TRAK", "TRAK" -> "TRAK", "TRK" -> "TRAK", "TRKS" -> "TRAK", "TRAK" -> "TRAK",
    "TRAFFICWAY" -> "TRFY", "TRFY" -> "TRFY",
    "TRAIL" -> "TRL", "TRAILS" -> "TRL", "TRL" -> "TRL", "TRLS" -> "TRL", "TRL" -> "TRL",
    "TRAILER" -> "TRLR", "TRLR" -> "TRLR", "TRLRS" -> "TRLR", "TRLR" -> "TRLR",
    "TUNEL" -> "TUNL", "TUNL" -> "TUNL", "TUNLS" -> "TUNL", "TUNNEL" -> "TUNL", "TUNNELS" -> "TUNL", "TUNNL" -> "TUNL", "TUNL" -> "TUNL",
    "TRNPK" -> "TPKE", "TURNPIKE" -> "TPKE", "TURNPK" -> "TPKE", "TPKE" -> "TPKE",
    "UNDERPASS" -> "UPAS", "UPAS" -> "UPAS",
    "UN" -> "UN", "UNION" -> "UN", "UN" -> "UN",
    "UNIONS" -> "UNS", "UNS" -> "UNS",
    "VALLEY" -> "VLY", "VALLY" -> "VLY", "VLLY" -> "VLY", "VLY" -> "VLY", "VLY" -> "VLY",
    "VALLEYS" -> "VLYS", "VLYS" -> "VLYS", "VLYS" -> "VLYS",
    "VDCT" -> "VIA", "VIA" -> "VIA", "VIADCT" -> "VIA", "VIADUCT" -> "VIA", "VIA" -> "VIA",
    "VIEW" -> "VW", "VW" -> "VW", "VW" -> "VW",
    "VIEWS" -> "VWS", "VWS" -> "VWS", "VWS" -> "VWS",
    "VILL" -> "VLG", "VILLAG" -> "VLG", "VILLAGE" -> "VLG", "VILLG" -> "VLG", "VILLIAGE" -> "VLG", "VLG" -> "VLG", "VLG" -> "VLG",
    "VILLAGES" -> "VLGS", "VLGS" -> "VLGS", "VLGS" -> "VLGS",
    "VILLE" -> "VL", "VL" -> "VL", "VL" -> "VL",
    "VIS" -> "VIS", "VIST" -> "VIS", "VISTA" -> "VIS", "VST" -> "VIS", "VSTA" -> "VIS", "VIS" -> "VIS",
    "WALK" -> "WALK", "WALK" -> "WALK",
    "WALKS" -> "WALK", "WALK" -> "WALK",
    "WALL" -> "WALL", "WALL" -> "WALL",
    "WY" -> "WAY", "WAY" -> "WAY", "WAY" -> "WAY",
    "WAYS" -> "WAYS", "WAYS" -> "WAYS",
    "WELL" -> "WL", "WL" -> "WL",
    "WELLS" -> "WLS", "WLS" -> "WLS",
  "#" -> "#")

  val stateMapping = Map("california" -> "CA",
    "ma" -> "MA",
    "in" -> "IN",
    "newhampshire" -> "NH",
    "id" -> "ID",
    "americansamoa" -> "AS",
    "idaho" -> "ID",
    "nm" -> "NM",
    "missouri" -> "MO",
    "oregon" -> "OR",
    "tennessee" -> "TN",
    "mh" -> "MH",
    "wyoming" -> "WY",
    "or" -> "OR",
    "oklahoma" -> "OK",
    "ia" -> "IA",
    "il" -> "IL",
    "kansas" -> "KS",
    "tn" -> "TN",
    "kentucky" -> "KY",
    "arkansas" -> "AR",
    "pr" -> "PR",
    "mo" -> "MO",
    "connecticut" -> "CT",
    "me" -> "ME",
    "az" -> "AZ",
    "ak" -> "AK",
    "mississippi" -> "MS",
    "pennsylvania" -> "PA",
    "gu" -> "GU",
    "vt" -> "VT",
    "wa" -> "WA",
    "florida" -> "FL",
    "districtofcolumbia" -> "DC",
    "newyork" -> "NY",
    "sd" -> "SD",
    "montana" -> "MT",
    "nevada" -> "NV",
    "massachusetts" -> "MA",
    "ky" -> "KY",
    "colorado" -> "CO",
    "southdakota" -> "SD",
    "nj" -> "NJ",
    "maine" -> "ME",
    "tx" -> "TX",
    "westvirginia" -> "WV",
    "washingtondistrictofcolumbia" -> "DC",
    "mi" -> "MI",
    "md" -> "MD",
    "nv" -> "NV",
    "ne" -> "NE",
    "mn" -> "MN",
    "michigan" -> "MI",
    "puertorico" -> "PR",
    "ks" -> "KS",
    "vermont" -> "VT",
    "washington" -> "WA",
    "arizona" -> "AZ",
    "ok" -> "OK",
    "alabama" -> "AL",
    "indiana" -> "IN",
    "minnesota" -> "MN",
    "ct" -> "CT",
    "guam" -> "GU",
    "northernmarianaislands" -> "MP",
    "iowa" -> "IA",
    "oh" -> "OH",
    "utah" -> "UT",
    "ar" -> "AR",
    "fl" -> "FL",
    "wi" -> "WI",
    "co" -> "CO",
    "delaware" -> "DE",
    "mt" -> "MT",
    "illinois" -> "IL",
    "palau" -> "PW",
    "georgia" -> "GA",
    "pw" -> "PW",
    "texas" -> "TX",
    "alaska" -> "AK",
    "dc" -> "DC",
    "washingtondc" -> "DC",
    "newmexico" -> "NM",
    "ohio" -> "OH",
    "nd" -> "ND",
    "pa" -> "PA",
    "fm" -> "FM",
    "federatedstatesofmicronesia" -> "FM",
    "ga" -> "GA",
    "louisiana" -> "LA",
    "virginia" -> "VA",
    "virginislands" -> "VI",
    "maryland" -> "MD",
    "nh" -> "NH",
    "hi" -> "HI",
    "as" -> "AS",
    "northcarolina" -> "NC",
    "wy" -> "WY",
    "southcarolina" -> "SC",
    "vi" -> "VI",
    "la" -> "LA",
    "ca" -> "CA",
    "ut" -> "UT",
    "newjersey" -> "NJ",
    "northdakota" -> "ND",
    "nebraska" -> "NE",
    "hawaii" -> "HI",
    "al" -> "AL",
    "rhodeisland" -> "RI",
    "marshallislands" -> "MH",
    "wv" -> "WV",
    "va" -> "VA",
    "nc" -> "NC",
    "ny" -> "NY",
    "sc" -> "SC",
    "ri" -> "RI",
    "wisconsin" -> "WI",
    "ms" -> "MS",
    "de" -> "DE")

  val secondaryDesignatorMapping = Map("unit" -> "UNIT",
    "basement" -> "BSMT",
    "department" -> "DEPT",
    "slip" -> "SLIP",
    "lowr" -> "LOWR",
    "uppr" -> "UPPR",
    "lower" -> "LOWR",
    "slip" -> "SLIP",
    "apartment" -> "APT",
    "ph" -> "PH",
    "frnt" -> "FRNT",
    "lot" -> "LOT",
    "stop" -> "STOP",
    "pier" -> "PIER",
    "trailer" -> "TRLR",
    "upper" -> "UPPR",
    "bldg" -> "BLDG",
    "lbby" -> "LBBY",
    "side" -> "SIDE",
    "floor" -> "FL",
    "penthouse" -> "PH",
    "bsmt" -> "BSMT",
    "rear" -> "REAR",
    "suite" -> "STE",
    "hngr" -> "HNGR",
    "rear" -> "REAR",
    "lobby" -> "LBBY",
    "side" -> "SIDE",
    "space" -> "SPC",
    "front" -> "FRNT",
    "hanger" -> "HNGR",
    "fl" -> "FL",
    "key" -> "KEY",
    "apt" -> "APT",
    "dept" -> "DEPT",
    "ste" -> "STE",
    "trlr" -> "TRLR",
    "office" -> "OFC",
    "lot" -> "LOT",
    "ofc" -> "OFC",
    "pier" -> "PIER",
    "unit" -> "UNIT",
    "stop" -> "STOP",
    "room" -> "RM",
    "rm" -> "RM",
    "building" -> "BLDG",
    "spc" -> "SPC",
    "key" -> "KEY",
    "box" -> "BOX")
}

object Address {

  val stateReplacements = Map("american samoa" -> "americansamoa",
    "district of columbia" -> "districtofcolumbia",
    "washington district of columbia" -> "washingtondistrictofcolumbia",
    "washington dc" -> "washingtondc",
    "federated states of micronesia" -> "federatedstatesofmicronesia",
    "marshall islands" -> "marshallislands",
    "new hampshire" -> "newhampshire",
    "new jersey" -> "newjersey",
    "new mexico" -> "newmexico",
    "new york" -> "newyork",
    "north carolina" -> "northcarolina",
    "north dakota" -> "northdakota",
    "northern mariana islands" -> "northernmarianaislands",
    "puerto rico" -> "puertorico",
    "rhode island" -> "rhodeisland",
    "south carolina" -> "southcarolina",
    "south dakota" -> "southdakota",
    "virgin islands" -> "virginislands",
    "west virginia" -> "westvirginia")

  def stateReplace(str: String): String = {
    var answer = str.toLowerCase
    stateReplacements.foreach((tuple) => tuple match {
      case (orig, replacement) => {
        answer = answer.replaceAll(orig, replacement)
      }
    })
    answer
  }
}
import org.scalatest.{FlatSpec, Matchers}
import org.vickery.addressparser.AddressParser
import org.vickery.addressparser.Address
import fastparse.core.Parsed.Success

/**
 * Created by vickery on 10/8/15.
 */
class TestParser extends FlatSpec with Matchers
{
  val optZip: Option[Integer] = Some(21212)
  val optCity = Some("citytown")
  val optState = Some("MD")

  val noSpecParser = new AddressParser(None, None, None)
  val justZipParser = new AddressParser(None, None, optZip)
  val stateAndZipParser = new AddressParser(None, optState, optZip)
  val completeParser = new AddressParser(optCity, optState, optZip, true)

  val streetNumbers = List("123", "123 1/2", "123.5", "123-5")
  val streetNameTypes = List("main", "north main", "main miggity main main", "123rd", "123")
  val streetTypes = List("st", "st.", "ST.", "st.,", "street")
  val directions = List("north", "N", "NE", "north east", "northeast")
  val secondaryTypes = List("apt", "SUITE", "STE", "#")
  val secondaryNumbers = List("320", "B")

  "street-type-1" should "parse" in {
    completeParser.streetType.parse("st").isInstanceOf[Success[_]] should be (true)
    completeParser.streetType.parse("ALLEY").isInstanceOf[Success[_]] should be (true)
    completeParser.streetType.parse("street").isInstanceOf[Success[_]] should be (true)
    completeParser.streetType.parse("stREet").isInstanceOf[Success[_]] should be (true)
  }

  "123 main st. " should "be parsed" in {
    val addressToParse = "123 main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be(Some(new Address(Some("123"),
      None,
      Some("main"),
      Some("st"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 1/2 main st." should "be parsed" in {
    val addressToParse = "123 1/2 main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123 1/2"),
      None,
      Some("main"),
      Some("st"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123-2 main st." should "be parsed" in {
    val addressToParse = "123-2 main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123-2"),
      None,
      Some("main"),
      Some("st"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 1/2 south main st." should "be parsed" in {
    val addressToParse = "123 1/2 south main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123 1/2"),
      None,
      Some("main"),
      Some("st"),
      Some("south"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123-2 south main st." should "be parsed" in {
    val addressToParse = "123-2 south main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123-2"),
      None,
      Some("main"),
      Some("st"),
      Some("south"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "1230S3 south main st." should "be parsed" in {
    val addressToParse = "1230S3 south main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("1230s3"),
      None,
      Some("main"),
      Some("st"),
      Some("south"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "A1230S3 south main st." should "be parsed" in {
    val addressToParse = "A1230S3 south main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("a1230s3"),
      None,
      Some("main"),
      Some("st"),
      Some("south"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 north south main st." should "be parsed" in {
    val addressToParse = "123 north south main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("south main"),
      Some("st"),
      Some("north"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123.5 north south main st." should "be parsed" in {
    val addressToParse = "123.5 north south main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123.5"),
      None,
      Some("south main"),
      Some("st"),
      Some("north"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 north south east main st." should "be parsed" in {
    val addressToParse = "123 north south east main st."
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("south east main"),
      Some("st"),
      Some("north"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 northeast main st. southwest" should "be parsed" in {
    val addressToParse = "123 northeast main st. southwest"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("main"),
      Some("st"),
      Some("northeast"),
      Some("southwest"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 north east main st. south west" should "be parsed" in {
    val addressToParse = "123 north east main st. south west"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("main"),
      Some("st"),
      Some("north east"),
      Some("south west"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 123 st. south west" should "be parsed" in {
    val addressToParse = "123 123 st. south west"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("123"),
      Some("st"),
      None,
      Some("south west"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 123rd st. south west" should "be parsed" in {
    val addressToParse = "123 123rd st. south west"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("123rd"),
      Some("st"),
      None,
      Some("south west"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 east 123rd st. south west" should "be parsed" in {
    val addressToParse = "123 east 123rd st. south west"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("123rd"),
      Some("st"),
      Some("east"),
      Some("south west"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 main st. bldg 345" should "be parsed, ignoring the secondary designator" in {
    val addressToParse = "123 main st. suite 345"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("main"),
      Some("st"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 main st. #345" should "be parsed, ignoring the secondary designator" in {
    val addressToParse = "123 main st. #345"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("main"),
      Some("st"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 avenue of the americas" should "be parsed" in {
    val addressToParse = "123 avenue of the americas"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("avenue of the americas"),
      Some("avenue"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 de la boo boo" should "be parsed" in {
    val addressToParse = "123 de la boo boo"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("de la boo boo"),
      None,
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "Washington DC, when parsed empty" should "be 'Washington'" in {
    val addressToParse = "123 grace st north east"
    new AddressParser(Some(""), Some("DC"), Some(12345)).parseWithKnownCityStateZip(addressToParse).get.canonicalCity should be ("Washington")
  }

  "Washington DC, when parsed as 'Washington'" should "be 'Washington'" in {
    val addressToParse = "123 grace st north east"
    new AddressParser(Some("washington"), Some("DC"), Some(12345)).parseWithKnownCityStateZip(addressToParse).get.canonicalCity should be ("Washington")
  }

  "123 grace st north east" should "be parsed" in {
    val addressToParse = "123 grace st north east"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("grace"),
      Some("st"),
      None,
      Some("north east"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 county road 3100" should "be parsed" in {
    val addressToParse = "123 county road 3100"
    val ans = completeParser.parseWithKnownCityStateZip(addressToParse)
    ans should be (Some(new Address(Some("123"),
      None,
      Some("county"),
      Some("road"),
      None,
      None,
      optCity,
      optState,
      optZip,
      Some("3100"))))

    ans.get.canonicalHighWayNumber should be ("3100")
    ans.get.masterAddressName should be ("123 County Rd 3100 Citytown MD 21212")
  }

  "123 1/2 highway 3200" should "be parsed" in {
    val addressToParse = "123 1/2 highway 3200"
    val ans = completeParser.parseWithKnownCityStateZip(addressToParse)
    ans should be (Some(new Address(Some("123 1/2"),
      None,
      Some(""),
      Some("highway"),
      None,
      None,
      optCity,
      optState,
      optZip,
      Some("3200"))))

    ans.get.canonicalStreetType should be ("Hwy")
    ans.get.canonicalStreetName should be ("")
    ans.get.masterAddressName should be ("123 1/2 Hwy 3200 Citytown MD 21212")
  }

  "PO Box 3300" should "be parsed" in {
    val addressToParse = "PO Box 3300"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("3300"),
      None,
      None,
      Some("PO Box"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "3300 P.O. Box" should "be parsed" in {
    val addressToParse = "3300 P.O. Box "
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("3300"),
      None,
      None,
      Some("PO Box"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "84 Po Box" should "be parsed" in {
    val addressToParse = "84 Po Box "
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("84"),
      None,
      None,
      Some("PO Box"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("PO Box")
  }

  "28 ROUTE 39" should "be parsed" in {
    val addressToParse = "28 ROUTE 39 "
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("28"),
      None,
      Some(""),
      Some("route"),
      None,
      None,
      optCity,
      optState,
      optZip,
      Some("39"))))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalHighWayNumber should be ("39")
  }

  "12990 N Highway 183" should "be parsed" in {
    val addressToParse = "12990 N Highway 183"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("12990"),
      None,
      None,
      Some("highway"),
      Some("n"),
      None,
      optCity,
      optState,
      optZip,
      Some("183"))))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalHighWayNumber should be ("183")
  }

  "73051 Hwy 111" should "be parsed" in {
    val addressToParse = "73051 Hwy 111"
    val result = completeParser.parseWithKnownCityStateZip(addressToParse)
    result  should be (Some(new Address(Some("73051"),
      None,
      Some(""),
      Some("hwy"),
      None,
      None,
      optCity,
      optState,
      optZip,
      Some("111"))))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalHighWayNumber should be ("111")
  }

  "6425 S Interstate 35 Ste 105" should "be parsed" in {
    val addressToParse = "6425 S Interstate 35 Ste 105"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("6425"),
      None,
      None,
      Some("interstate"),
      Some("s"),
      None,
      optCity,
      optState,
      optZip,
      Some("35"))))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalHighWayNumber should be ("35")
    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Interstate")
  }

  "647 W OLD US HIGHWAY 90" should "be parsed" in {
    val addressToParse = "647 W OLD US HIGHWAY 90"
    val ans = completeParser.parseWithKnownCityStateZip(addressToParse)
    ans  should be (Some(new Address(Some("647"),
      None,
      Some("old us"),
      Some("highway"),
      Some("w"),
      None,
      optCity,
      optState,
      optZip,
      Some("90"))))

    ans.get.canonicalHighWayNumber should be ("90")
    ans.get.masterAddressName should be ("647 W Old Us Hwy 90 Citytown MD 21212")
  }

  "Empty addresses" should "not parse" in {
    val addressToParse = " "
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (None)

    val otherAddToParse = ""
    completeParser.parseWithKnownCityStateZip(otherAddToParse) should be (None)
  }

  "Only street name" should "not parse" in {
    val addressToParse = " 154th"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (None)

    val otherAddToParse = "broad"
    completeParser.parseWithKnownCityStateZip(otherAddToParse) should be (None)

    val otherAddToParse1 = "0"
    completeParser.parseWithKnownCityStateZip(otherAddToParse1) should be (None)
  }

  "777 Avenue of the Americas" should "parse" in {
    val addressToParse = "777 AVENUE OF THE AMERICAS"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("777"),
      None,
      Some("avenue of the americas"),
      Some("avenue"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  /*
  TODO
      "777 Avenue W" should "parse" in {
    val addressToParse = "777 AVENUE W"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("777"),
      None,
      Some("avenue w"),
      Some("avenue"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }*/

  "777 Brockton Ave" should "parse" in {
    val addressToParse = "777 Brockton ave"
    val newParser = new AddressParser(Some("Abingdon"), Some("MA"), Some("02531".toInt))
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("777"),
      None,
      Some("brockton"),
      Some("ave"),
      None,
      None,
      Some("Abingdon"),
      Some("MA"),
      Some("02531".toInt),
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalZip should be ("02531")
  }

  "Canonical cities" should "be properly capitalized" in {
    val addressToParse = "777 Brockton ave"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), Some("02531".toInt))
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("777"),
      None,
      Some("brockton"),
      Some("ave"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      Some("02531".toInt),
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalCity should be ("Abingdon")
  }

  "Undefined Zips" should "be parsed" in {
    val addressToParse = "777 Brockton ave"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), None)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("777"),
      None,
      Some("brockton"),
      Some("ave"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      None,
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalZip should be ("")
  }

  "Place suffix" should "be parsed" in {
    val addressToParse = "777 Brockton place"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), None)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("777"),
      None,
      Some("brockton"),
      Some("place"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      None,
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Pl")
  }

  "Broad Branch Rd" should "be parsed" in {
    val addressToParse = "4464 BROAD BRANCH ROAD"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), None)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("4464"),
      None,
      Some("broad branch"),
      Some("road"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      None,
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Rd")
  }

  "1301 New Jersey Ave NW" should "be parsed" in {
    val addressToParse = "1301 New Jersey Ave NW"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), None)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("1301"),
      None,
      Some("new jersey"),
      Some("ave"),
      None,
      Some("nw"),
      Some("ABINGDON"),
      Some("MA"),
      None,
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Ave")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetName should be ("New Jersey")
  }

  "4447 E STREET SE" should "be parsed" in {
    val addressToParse = "4447 E STREET SE"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), None)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("4447"),
      None,
      Some("e"),
      Some("street"),
      None,
      Some("se"),
      Some("ABINGDON"),
      Some("MA"),
      None,
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("St")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalPreDirection should be ("")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalPostDirection should be ("SE")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetName should be ("E")
  }

  "5411 EAST AVE" should "be parsed" in {
    val addressToParse = "5411 EAST AVE"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), Some(12345), true)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("5411"),
      None,
      Some("east"),
      Some("ave"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      Some(12345),
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Ave")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalPreDirection should be ("")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalPostDirection should be ("")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetName should be ("East")
  }

  "5411 A EAST AVE" should "be parsed" in {
    val addressToParse = "5411 A EAST AVE"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), Some(12345), true)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("5411-a"),
      None,
      Some("east"),
      Some("ave"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      Some(12345),
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetNumber should be ("5411-A")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Ave")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalPreDirection should be ("")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalPostDirection should be ("")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetName should be ("East")
  }

  "727 HICKORY LOT RD" should "be parsed" in {
    val addressToParse = "727 HICKORY LOT RD"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), Some(12345), true)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("727"),
      None,
      Some("hickory lot"),
      Some("rd"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      Some(12345),
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Rd")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetName should be ("Hickory Lot")
  }

  "4010 BROENING HWY" should "be parsed" in {
    val addressToParse = "4010 BROENING HWY"
    val newParser = new AddressParser(Some("ABINGDON"), Some("MA"), Some(12345), true)
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("4010"),
      None,
      Some("broening"),
      Some("hwy"),
      None,
      None,
      Some("ABINGDON"),
      Some("MA"),
      Some(12345),
      None)))

    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("Hwy")
    newParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetName should be ("Broening")
  }

  "Empty street contents" should "Not throw an exception" in {
    val addressToParse = "525 1/2 Unit B"
    val newParser = new AddressParser(Some("Glendale"), Some("AZ"), Some("91206".toInt))
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("525 1/2"),
      Some("unit b"),
      Some(""),
      None,
      None,
      None,
      Some("Glendale"),
      Some("AZ"),
      Some("91206".toInt),
      None)))
  }

  "110 Riverway Apt 7" should "parse" in {
    val addressToParse = "110 Riverway Apt 7"
    val newParser = new AddressParser(Some("Boston"), Some("MA"), Some("02215".toInt))
    newParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("110"),
      Some("apt 7"),
      Some("riverway"),
      None,
      None,
      None,
      Some("Boston"),
      Some("MA"),
      Some("02215".toInt),
      None)))
  }

  "0.00000000 BELLA COLLINA ST" should "not parse" in {
    val addressToParse = "0.00000000 BELLA COLLINA ST"
    val newParser = new AddressParser(Some("Chulavista"), Some("CA"), Some("12345".toInt))
    newParser.parseWithKnownCityStateZip(addressToParse) should be (None)
  }

  "240.00000000 N Coast Hwy 101" should "parse" in {
    val addressToParse = "240.00000000 N Coast Hwy 101"

     completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("240"),
      None,
      Some("coast"),
      Some("hwy"),
      Some("n"),
      None,
      optCity,
      optState,
      optZip,
      Some("101"))))
  }

  "525 1/2" should "parse with empty street in case we want to test this later" in {
    val addressToParse = "525 1/2"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("525 1/2"),
     None,
      Some(""),
      None,
      None,
      None,
     optCity,
      optState,
      optZip,
      None)))
  }

  "525 123" should "parse" in {
    val addressToParse = "525 123"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("525"),
      None,
      Some("123"),
      None,
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "525 N ST NW" should "parse" in {
    val addressToParse = "525 N ST NW"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("525"),
      None,
      Some("n"),
      Some("st"),
      None,
      Some("nw"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "525 W AVE 40" should "parse" in {
    val addressToParse = "525 W AVE 40"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("525"),
      None,
      None,
      Some("ave"),
      Some("w"),
      None,
      optCity,
      optState,
      optZip,
      Some("40"))))
  }

  "5032 East Coyotes Diagonal" should "parse" in {
    val addressToParse = "5032 East Coyotes Diagonal"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("5032"),
      None,
      Some("coyotes diagonal"),
      None,
      Some("east"),
      None,
      optCity,
      optState,
      optZip,
      None)))
  }

  "5032 Coyotes Diagonal West" should "parse" in {
    val addressToParse = "5032 Coyotes Diagonal West"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("5032"),
      None,
      Some("coyotes diagonal"),
      None,
      None,
      Some("west"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "5032 East Coyotes Diagonal West" should "parse" in {
    val addressToParse = "5032 East Coyotes Diagonal West"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("5032"),
      None,
      Some("coyotes diagonal"),
      None,
      Some("east"),
      Some("west"),
      optCity,
      optState,
      optZip,
      None)))
  }

  "123 St Louis St" should "parse" in {
    val addressToParse = "123 St Louis St"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("st louis"),
      Some("st"),
      None,
      None,
      optCity,
      optState,
      optZip,
      None)))
  }


  /* val addressToParse = streetNumbers(0) + " " + directions(0) + " " + streetNameTypes(0) + " " + streetTypes(0) + " 3900 " + directions(1) + " " + secondaryTypes(0) + " " + secondaryNumbers(0)

   Console.println("Parsing " + addressToParse)
   completeParser.parse(addressToParse
   ) should be (new Address(Some(streetNumbers(0)),
     Some(secondaryNumbers(0)),
     Some(streetNameTypes(0)),
     Some(streetTypes(0)),
     Some(directions(0)),
     Some(directions(0)),
     optCity,
     optState,
     optZip,
     Some("3900")))*/

}

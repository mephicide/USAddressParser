import org.scalatest.{FlatSpec, Matchers}
import org.vickery.addressparser.AddressParser
import org.vickery.addressparser.Address

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

  //TODO city state zip stuff if we need it
  //val cityNames = List("citytown", "city town")

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
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123"),
      None,
      Some("county"),
      Some("road"),
      None,
      None,
      optCity,
      optState,
      optZip,
      Some("road 3100"))))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalHighWayNumber should be ("3100")
  }

  "123 1/2 highway 3200" should "be parsed" in {
    val addressToParse = "123 1/2 highway 3200"
    completeParser.parseWithKnownCityStateZip(addressToParse) should be (Some(new Address(Some("123 1/2"),
      None,
      Some("highway 3200"),
      Some("highway"),
      None,
      None,
      optCity,
      optState,
      optZip,
      Some("highway 3200"))))

    completeParser.parseWithKnownCityStateZip(addressToParse).get.canonicalStreetType should be ("")
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

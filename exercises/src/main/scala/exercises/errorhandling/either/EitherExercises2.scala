package exercises.errorhandling.either

object EitherExercises2 {

  case class User(username: Username, countryOfResidence: Country)

  case class Username(value: String)

  sealed abstract class Country(val code: String)
  object Country {
    val all: List[Country] = List(France, Germany, Switzerland, UnitedKingdom)

    case object France        extends Country("FRA")
    case object Germany       extends Country("DEU")
    case object Switzerland   extends Country("CHE")
    case object UnitedKingdom extends Country("GBR")
  }

  // 1. Implement `validateCountry` which takes a 3-letter country code and returns
  // the matching `Country` value (see Alpha-3 code format).
  // `validateCountry` can fail for two reasons:
  // * string format is invalid - we expect 3 upper case letters
  // * the country is not supported by the application
  // For example,
  // validateCountry("FRA") == Right(France)
  // validateCountry("UK")  == Left(InvalidFormat("UK"))
  // validateCountry("ARG") == Left(NotSupported("ARG")), ARG represents Argentina
  def validateCountry(countryCode: String): Either[FormError, Country] = {
    if(countryCode.length < 3 || !countryCode.forall(char => char.isLetter && char.isUpper)
    ) {
      return Left(InvalidFormat(countryCode))
    }
    Country.all
      .find(_.code == countryCode)
      .toRight(NotSupported(countryCode))
  }

  // 2. Implement `checkUsernameSize` which checks if a username is
  // at least 5 characters long. For example,
  // checkUsernameSize("bob_2167") == Right(())
  // checkUsernameSize("bob_2")    == Right(())
  // checkUsernameSize("bo")       == Left(TooSmall(2))
  def checkUsernameSize(username: String): Either[TooSmall, Unit] =
    Either.cond(username.length >= 5, right = (), left= TooSmall(username.length))

  // 3. Implement `checkUsernameCharacters` which checks if all characters are valid
  // according to the function `isValidUsernameCharacter`. For example,
  // checkUsernameCharacters("_abc-123_")  == Right(())
  // checkUsernameCharacters("foo!~23}AD") == Left(InvalidCharacters(List('!','~','}')))
  def checkUsernameCharacters(username: String): Either[InvalidCharacters, Unit] =
    username.filterNot(isValidUsernameCharacter).toList match {
      case Nil => Right()
      case chars => Left(InvalidCharacters(chars))
    }

  def isValidUsernameCharacter(c: Char): Boolean =
    c.isLetter || c.isDigit || c == '_' || c == '-'

  // 4. Implement `validateUsername` which verifies the username size and content
  // is correct according to `checkUsernameSize` and `checkUsernameCharacters`.
  // For example,
  // validateUsername("bob_2167")   == Right(Username("bob_2167"))
  // validateUsername("bo")         == Left(TooSmall(2))
  // validateUsername("foo!~23}AD") == Left(InvalidCharacters(List('!','~','}')))
  def validateUsername(username: String): Either[FormError, Username] =
    for {
      _ <- checkUsernameSize(username)
      _ <- checkUsernameCharacters(username)
    } yield Username(username)

  // 5. Implement `validateUser` which verifies that both the username and the country
  // of residence are correct according to `validateUsername` and `validateCountry`.
  // What should be the return type of `validateUser`?
  // validateUser("bob_2167", "FRA") --> Success User(Username("bob_2167"), France)
  // validateUser("bo", "FRA")       --> Failure
  def validateUser(usernameStr: String, countryStr: String): Either[FormError, User] = // Either[???, User]
    for {
      country <- validateCountry(countryStr)
      username <- validateUsername(usernameStr)
    } yield User(username, country)

  sealed trait FormError
  case class InvalidFormat(country: String) extends FormError
  case class NotSupported(country: String)  extends FormError

  case class TooSmall(inputLength: Int)          extends FormError
  case class InvalidCharacters(char: List[Char]) extends FormError


}
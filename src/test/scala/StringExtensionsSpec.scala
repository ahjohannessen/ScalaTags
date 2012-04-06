import org.scalatest._
import matchers.ShouldMatchers
import ScalaTags.StringExtensions._

class StringExtensionsSpec extends FunSpec with ShouldMatchers {

	describe("String's nullOrEmpty") {

		it("should be true when empty") {
			"".isNullOrEmpty should be (true)
		}

		it("should be true when null") {
			val string : String = null
			string.isNullOrEmpty should be (true)
		}
	}

	describe("String's notNullOrEmpty") {

		it("should be true when empty") {
			"".isNotNullOrEmpty should be (false)
		}

		it("should be true when null") {
			val string : String = null
			string.isNotNullOrEmpty should be (false)
		}
	}
}
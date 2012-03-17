import ScalaTags.ScalaTag
import org.scalatest._
import matchers.ShouldMatchers

class ScalaTagSpec extends FunSpec
	with BeforeAndAfter
	with ShouldMatchers {

	var tag: ScalaTag = _;

	before {
		tag = new ScalaTag("Div")
	}

	describe("A ScalaTag") {

		it("should lowercase tags") {
			tag.tagName() should equal("div")
		}

		it("should be able to change tag") {
			tag.tagName("Span").tagName() should equal("span")
		}

		it("should not allow change to invalid tag") {
			intercept[IllegalArgumentException](tag.tagName(""))
			intercept[IllegalArgumentException](tag.tagName(null))
		}

		it("should throw an exception when instantiated with invalid tag") {
			intercept[IllegalArgumentException](tag = new ScalaTag(""))
			intercept[IllegalArgumentException](tag = new ScalaTag(null))
		}

		describe("children") {

			it("should be none on creation") {
				tag.allChildren().size should equal(0)
			}

			it("should be able to prepend a child") {
				tag.prepend(new ScalaTag("div"))
				tag.prepend(new ScalaTag("span"))

				tag.firstChild().tagName should equal("span")
			}

			it("should be able to append a child") {
				tag.append(new ScalaTag("div"))
				tag.append(new ScalaTag("span"))

				tag.firstChild().tagName should equal("div")
			}

			it("should be able to prepend many children") {
				tag.prepend(new ScalaTag("div"), new ScalaTag("span"))
					.firstChild().tagName should equal("div")
			}

			it("should be able to append many children") {
				tag.append(new ScalaTag("div"), new ScalaTag("span"))
					.firstChild().tagName should equal("div")
			}

			it("should provide you with an iterable for its children") {
				val child = new ScalaTag("div")
				tag.prepend(child)

				tag.allChildren should contain(child)
			}
		}

		it("should provide a way for callback") {
			tag.modify(x => x.render(false)).render should be (false)
		}

		describe("style") {

			it("should be customizable") {
				tag.style("k", "v").style("k") should equal(Some("v"))
				tag.hasStyle("k") should equal(true)
			}

			it("should return none/false when no key") {
				tag.style("nk") should equal(None)
				tag.hasStyle("nk") should equal(false)
			}

			it("should be able to hide") {
				tag.hide().style("display") should be (Some("none"))
			}
		}

		describe("text") {

			it("should be empty by default") {
				tag.text should be ("")
			}

			it("should be stored") {
				tag.text("scala").text should be ("scala")
			}
		}

		describe("companion's empty method") {

			it("should create a span tag") {
				ScalaTag.empty().tagName() should equal("span")
			}

			it("should create a tag that does not render") {
				ScalaTag.empty().render() should equal(false)
			}
		}

		describe("companion's withParent method") {

			it("should append to parent when some") {
				val child = ScalaTag.withParent("child", tag)

				child.tagName() should equal("child")
				tag.firstChild() should equal(child)
			}

			it("should handle null") {
				val taggy = ScalaTag.withParent("taggy", null)
				taggy.tagName should equal("taggy")
			}
		}

		describe("constructor accepts an action that") {

			it("should configure the instance") {

				new ScalaTag("child", t => t.render(false))
					.render() should be(false)
			}
		}
	}
}
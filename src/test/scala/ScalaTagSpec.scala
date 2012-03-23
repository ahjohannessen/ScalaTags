import ScalaTags.ScalaTag
import org.scalatest._
import matchers.ShouldMatchers

class ScalaTagSpec extends FunSpec with BeforeAndAfter
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
			tag.modify(x => x.render(false)).render should be(false)
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
				tag.hide().style("display") should be(Some("none"))
			}
		}

		describe("text") {

			it("should be empty by default") {
				tag.text should be("")
			}

			it("should be stored") {
				tag.text("scala").text should be("scala")
			}
		}

		describe("when adding single css class") {

			it("should not be possible to add multiple classes separated by space") {
				intercept[IllegalArgumentException] {
					tag addClass ("red green blue")
				}
			}

			it("should be possible to store json in css that contains spaces") {
				tag.addClass("{ }")
			}

			it("should add the class") {
				val css = "blah"
				tag hasClass css should be(false)
				tag addClass css hasClass css should be(true)
			}
		}

		describe("when adding multiple css classes") {

			it("should add all") {
				val classes = List("a", "b", "c")
				tag addClasses(classes)

				classes foreach(tag hasClass _ should be (true))
			}
		}

		describe("when adding an attribute") {

			it("should add it as string") {
				tag attr("a", 3.14) attr "a" should be ("3.14")
			}

			it("should remove attribute when value is null") {
				tag attr("a", "v") attr "a" should be ("v")
				tag attr("a", null) attr "a" should be ("")
			}

			it("should remove attribute when value is empty string") {
				tag attr("a", "v") attr "a" should be ("v")
				tag attr("a", "") attr "a" should be ("")
			}

			it("should add css when class attribute") {
				tag attr("class", "a") hasClass "a" should be (true)
			}

			it("should add multiple css when space separated class attribute") {
				tag attr("class", "x z")
				tag hasClass "x" should be (true)
				tag hasClass "z" should be (true)
			}
		}

		describe("when removing an attribute") {

			it("should reset css classes if css attribute") {

				val classes = List("a", "b", "c")
				tag addClasses classes
				tag removeAttr "class"

				classes foreach (tag hasClass _ should be (false))
			}

			it("should reset css styles if style attribute") {

				val styles = List("color:#FFFFFF;", "width:100px;")
				tag addClasses styles
				tag removeAttr "style"

				styles foreach (tag hasStyle _ should be (false))
			}

			it("should only remove attribute when html attribute") {

				tag attr("href", "www.scala-lang.org")
				tag attr("title", "scala is fun")
				tag removeAttr "title"

				tag attr "title" should be ("")
				tag attr "href" should be ("www.scala-lang.org")
			}
		}

		describe("when adding multiple classes") {

			it("should add all") {
				val classes = List("a", "b", "c")
				tag addClasses (classes)

				classes foreach (tag hasClass _ should be(true))
			}

			describe("when removing a class") {

				it("should remove the class") {
					val css = "monty"
					tag addClass css removeClass css hasClass css should be(false)
				}

				it("should happily do nothing if class does not exist") {
					tag removeClass "scalatag"
				}
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
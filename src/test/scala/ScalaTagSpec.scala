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

				tag style("color", "#FFFFFF") style("width", "100px")
				tag removeAttr "style"

				tag hasStyle "color" should be (false)
				tag hasStyle "width" should be (false)
			}

			it("should only remove attribute when html attribute") {

				tag attr("href", "www.scala-lang.org")
				tag attr("title", "scala is fun")
				tag removeAttr "title"

				tag attr "title" should be ("")
				tag attr "href" should be ("www.scala-lang.org")
			}
		}

		describe("when checking attribute membership") {

			it("should return true when has css class attribute") {
				tag addClass "a" hasAttr "class" should be (true)
			}

			it("should return false when no css class attribute") {
				tag style("color", "#FFFFFF") hasAttr "class" should be (false)
			}

			it("should return true when has style attribute") {
				tag style("color", "#FFFFFF") hasAttr "style" should be (true)
			}

			it("should return false when no style attribute") {
				tag addClass "b" hasAttr "style" should be (false)
			}

			it("should return true when has metadata attribute") {
				tag metadata ("title", "scala") hasAttr ScalaTag.metadataAttribute should be (true)
			}

			it("should return false when no metadata attribute") {
				tag style("color", "#FFFFFF") hasAttr ScalaTag.metadataAttribute should be (false)
			}

			it("should return true when has html attribute") {
				tag attr ("title", "scala") hasAttr "title" should be (true)
			}			
			
			it("should return false when no html attribute") {
				tag style("color", "#FFFFFF") hasAttr "color" should be (false)
			}
		}

		describe("when adding multiple classes") {

			it("should add all") {
				val classes = List("a", "b", "c")
				tag addClasses (classes)

				classes foreach (tag hasClass _ should be(true))
			}
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

		describe("metadata") {

			it("should be stored") {
				tag metadata("test", 42)
				tag hasMetadata "test" should be (true)
				tag metadata "test" should be (Some(42))
			}

			it("should return none/false when no key") {
				tag metadata "1" should be (None)
				tag hasMetadata "1" should be (false)
			}

			it("should be able to return typed data"){
				tag metadata("person", Person("ahj", 34))
				tag.metadataTyped[Person]("person") should be (Some(Person("ahj", 34)))
			}

			it("should return none when no typed data"){
				tag.metadataTyped[Person]("person") should be (None)
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

case class Person(var name: String, var age: Int) {}
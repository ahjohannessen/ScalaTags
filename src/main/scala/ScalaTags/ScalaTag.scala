package ScalaTags

import collection.mutable._

class ScalaTag(divTag: String) {
	validateTag(divTag)

	private val children = ListBuffer[ScalaTag]()
	private val cssClasses = new HashSet[String]()
	private val customStyles = new HashMap[String, String]()
	private val htmlAttributes = new HashMap[String, Any]()
	
	private var tag: String = divTag.toLowerCase
	private var doRender: Boolean = true;
	private var content = ""

	val cssClassAttribute = "class"
	val cssStyleAttribute = "style"
	val dataPrefix = "data-"

	def this(divTag: String, action: ScalaTag => Unit) = {
		this(divTag)
		action(this)
	}

	def tagName() = {
		tag
	}

	def tagName(divTag: String) = {
		validateTag(divTag)
		tag = divTag.toLowerCase
		this
	}

	def render() = {
		doRender
	}

	def render(shouldRender: Boolean) = {
		doRender = shouldRender
		this
	}

	def allChildren() = {
		children.toIterable
	}

	def append(tags: ScalaTag*) = {
		children appendAll tags
		this
	}

	def prepend(tag: ScalaTag*) = {
		children prependAll tag
		this
	}

	def firstChild(): ScalaTag = {
		children.head
	}

	def style(key: String, value: String) = {
		customStyles(key) = value
		this
	}

	def style(key: String) = {
		customStyles get key
	}

	def hasStyle(key: String) = {
		customStyles contains key
	}

	def hide() = {
		style("display", "none")
	}

	def modify(action: ScalaTag => Unit) = {
		action(this)
		this
	}

	def text(txt: String) = {
		content = txt
		this
	}

	def text() = {
		content
	}

	def addClass(className: String) = {
		val (valid, msg) = isValidClassName(className)
		require(valid, msg)
		cssClasses add className
		this
	}

	def removeClass(className: String) = {
		cssClasses remove className
		this
	}

	def hasClass(className: String) = {
		cssClasses contains className
	}

	def addClasses(classes: scala.Iterable[String]) = {
		classes foreach addClass
		this
	}

	def attr(attribute: String) = {
		(htmlAttributes get attribute) getOrElse ""
	}

	def attr(attribute: String, value: Any) = {

		(shouldRemoveAttr(value), isCssClassAttr(attribute)) match {
			case (true, false) => removeAttr(attribute)
			case (false, true) => addClasses(value.toString.split(' '))
			case (false, false) => htmlAttributes(attribute) = value.toString
		}
		this
	}

	def removeAttr(attribute: String) = {
		
		if(isCssClassAttr(attribute)) {

			cssClasses clear()

		} else if (isCssStyleAttr(attribute)) {

			customStyles clear()

		} else {

			htmlAttributes remove attribute
		}
		this
	}

	private def shouldRemoveAttr(value: Any) = {
		value == null || value == ""
	}

	private def isCssClassAttr(attribute: String) = {
		attribute equalsIgnoreCase cssClassAttribute
	}

	private def isCssStyleAttr(attribute: String) = {
		attribute equalsIgnoreCase cssStyleAttribute
	}

	private def isValidClassName(name: String) = {
		(
			(name.startsWith("{") && name.endsWith("}")) || !name.contains(' '),
			"CSS class names cannot contain spaces. Problem class was %s" format name
		)
	}

	private def validateTag(divTag: String) {
		require(divTag != null && !divTag.isEmpty)
	}
}

object ScalaTag {

	def withParent(divTag: String, parent: ScalaTag): ScalaTag = {
		new ScalaTag(divTag, t => if (parent != null) parent.append(t))
	}

	def empty(): ScalaTag = {
		new ScalaTag("span").render(false);
	}
}
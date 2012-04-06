package ScalaTags

import collection.mutable._
import StringExtensions._

class ScalaTag(divTag: String) {
	validateTag(divTag)

	private val children = ListBuffer[ScalaTag]()
	private val cssClasses = new HashSet[String]()
	private val customStyles = new HashMap[String, String]()
	private val htmlAttributes = new HashMap[String, Any]()
	private val metaData = new HashMap[String, Any]()
	
	private var tag: String = divTag.toLowerCase
	private var content = ""

	private var encodeInnerText = true
	private var doRender: Boolean = true
	private var isAuthorized: Boolean = true
	private var ignoreClosingTag = false

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

	def encoded() = {
		encodeInnerText
	}

	def encoded(encodeInnerText: Boolean) = {
		this.encodeInnerText = encodeInnerText
		this
	}

	def authorized() = {
		isAuthorized
	}

	def authorized(isAuthorized: Boolean) = {
		this.isAuthorized = isAuthorized
		this
	}

	def replaceChildren(tags: ScalaTag*) {
		children clear()
		children appendAll tags
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

	def wrapWith(wrapper: ScalaTag) : ScalaTag = {
		wrapper.prepend(this)
	}

	def wrapWith(tag: String) : ScalaTag = {
		val wrapper = new ScalaTag(tag)
		wrapper render(render())
		wrapper authorized(authorized())
		wrapWith(wrapper)
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

	private def shouldRemoveAttr(value: Any) = {
		value == null || value == ""
	}

	def removeAttr(attribute: String) = {
		
		if(isCssClassAttr(attribute)) {

			cssClasses clear()

		} else if (isCssStyleAttr(attribute)) {

			customStyles clear()

		} else if (isMetadataAttribute(attribute)) {

			metaData clear()

		} else {

			htmlAttributes remove attribute
		}
		this
	}

	def hasAttr(attribute: String) : Boolean = {

		if(isCssClassAttr(attribute)) return !cssClasses.isEmpty
		if(isCssStyleAttr(attribute)) return !customStyles.isEmpty
		if(isMetadataAttribute(attribute)) return !metaData.isEmpty
		htmlAttributes contains attribute
	}

	def hasMetadata(key: String) = {
		metaData contains key
	}

	def metadata(key: String, value: Any) = {
		metaData(key) = value
		this
	}
	
	def metadata(key: String) : Option[Any] = {
		metaData get key
	}

	def metadataTyped[T](key: String) : Option[T] = {
		val value = metadata(key)
		if(value.isDefined && value.get.isInstanceOf[T]) {
			return value.asInstanceOf[Option[T]]
		}
		None
	}

	def title() = {
		attr("title")
	}
	
	def title(value: String) = {
		attr("title", value)
		this
	}

	def isInputElement = {
		divTag == "input" || divTag == "select" || divTag == "textarea"
	}

	def noClosingTag = {
		ignoreClosingTag = true
		this
	}

	def hasClosingTag = {
		!ignoreClosingTag
	}

	private def isCssClassAttr(attribute: String) = {
		attribute equalsIgnoreCase ScalaTag.cssClassAttribute
	}

	private def isCssStyleAttr(attribute: String) = {
		attribute equalsIgnoreCase ScalaTag.cssStyleAttribute
	}

	private def isMetadataAttribute(attribute: String) = {
		attribute equalsIgnoreCase ScalaTag.metadataAttribute
	}

	private def isValidClassName(name: String) = {
		(
			(name.startsWith("{") && name.endsWith("}")) || !name.contains(' '),
			"CSS class names cannot contain spaces. Problem class was %s" format name
		)
	}

	private def validateTag(divTag: String) {
		require(divTag.isNotNullOrEmpty)
	}
}

object ScalaTag {

	private val cssClassAttribute = "class"
	private val cssStyleAttribute = "style"
	private val dataPrefix = "data-"
	private var metadataSuffix = ":"

	def useMetadataSuffix(suffix: String) {
		require(suffix.isNotNullOrEmpty)
		metadataSuffix = suffix
	}

	def metadataAttribute() = {
		dataPrefix + metadataSuffix
	}

	def withParent(divTag: String, parent: ScalaTag): ScalaTag = {
		new ScalaTag(divTag, t => if (parent != null) parent.append(t))
	}

	def empty(): ScalaTag = {
		new ScalaTag("span").render(false);
	}
}
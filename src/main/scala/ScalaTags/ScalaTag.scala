package ScalaTags

import collection.mutable.{HashSet, HashMap, ListBuffer}


class ScalaTag(divTag: String) {
	validateTag(divTag)

	private val children = ListBuffer[ScalaTag]()
	private var tag: String = divTag.toLowerCase
	private var doRender: Boolean = true;

	private var content = ""
	private val cssClasses = new HashSet[String]()
	private val customStyles = new HashMap[String, String]()

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
package ScalaTags

object StringExtensions {

	implicit def notNullOrEmpty(value: String) = new {
		def isNotNullOrEmpty = !value.isNullOrEmpty
	}

	implicit def nullOrEmpty(value: String) = new {
		def isNullOrEmpty = value == null || value.isEmpty
	}
}

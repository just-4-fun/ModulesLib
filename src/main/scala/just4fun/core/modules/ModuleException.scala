package just4fun.core.modules

import javax.annotation.processing.SupportedSourceVersion


abstract class ModuleException extends Exception


class ModuleServiceException(msg: String = null)(implicit  module: Module) extends ModuleException {
	import StateParams._
	override def getMessage: String = s"Module is not able to serve ${if (msg != null) msg else if (module.isSuspended) ": it is suspended" else if (module.isUnavailable) ": it is destroying" else  s"in ${module.getState} state"}."
}


class ModuleBindException(val parentClas: Class[_], reason: String = null) extends ModuleException {
	override def getMessage: String = s"Module can not be bound to ${parentClas.getName}. ${if (reason != null) reason else ""}"
}


class SyncServerException(server: Module) extends ModuleException {
	// TODO more readable name
	override def getMessage: String = s"Module is failed because of synchronous server ${server.getClass.getName} failed with  ${server.failure.foreach(_.getMessage)}"
}

package just4fun.core.modules

import java.util.concurrent.locks.ReentrantLock
import just4fun.core.async.{FutureContextOwner, FutureX, FutureContext}

import scala.collection.mutable
import just4fun.utils.logger.Logger._

import scala.concurrent.Promise


private[modules] object ModuleSystem {
	var instance: ModuleSystem = null
	private[this] var stopID: Int = 0
	def nextStopID: Int = { stopID -= 1; stopID }
}



trait ModuleSystem extends RestoreSubsystem with FutureContextOwner {
	ModuleSystem.instance = this
	implicit val asyncContext: FutureContext
	private[this] val modules = mutable.ListBuffer[Module]()
	private[this] val detached = mutable.ListBuffer[Module]()
	private[this] var sysClient: RootModule = null
	private[this] val lock = new ReentrantLock
	//todo set false if some module started befiore system stopped
	private[this] var stopping = true
	private[this] var currentModules: mutable.HashMap[Thread, List[Module]] = mutable.HashMap()
	private[this] var currentClas: Class[_] = null
	private[this] var currentClient: Module = null
	private[this] var currentSync = false
	private[this] var currentRestore = false

	/* SYSTEM API */
	// todo more elegant
	def isSystemStopped: Boolean = sysClient == null && modules.isEmpty && detached.isEmpty
	def isSystemStopping: Boolean = stopping
	/* callbacks */
	protected[this] def onSystemStart(): Unit = ()
	protected[this] def onSystemStop(): Unit = ()
	/* internal */
	private[this] def startSystem(): Unit = {
		logV(s"*************  SYSTEM START  **************")
		stopping = false
		onSystemStart()
	}
	private[this] def canStop: Boolean = {
		logV(s"canStop locked? ${lock.isLocked};  detached.empty? ${detached.isEmpty};  mods: ${modules.map(_.getClass.getSimpleName).mkString(", ")}")
		modules.isEmpty && detached.isEmpty
	}
	private[this] def stopSystem(): Unit = {
		if (canStop && !isSystemStopped && !lock.isLocked) {
			asyncContext.stop(true)
			if (!lock.isLocked) onSystemStop()
			asyncContext.stop(true)
			sysClient = null
			logV(s"*************  SYSTEM STOP  **************")
		}
	}

	/* MODULE API */

	def hasModule[M <: Module : Manifest]: Boolean = hasModule(moduleClass)
	def hasModule[M <: Module](cls: Class[M]): Boolean = find(cls).nonEmpty

	def startModule[M <: Module : Manifest]: Unit = {
		val server = bind[M](moduleClass, systemClient)
		if (server.isRestorable) updateRestorables(server.getClass, true)
	}
	def startModule[M <: Module : Manifest](constructor: ⇒ M): Unit = {
		val server = bind[M](moduleClass, systemClient, constr = () ⇒ constructor)
	}
	def startModule[M <: Module : Manifest](stopID: Int): Unit = {
		val server = bind[M](moduleClass, systemClient)
		if (stopID == 0 && server.isRestorable) updateRestorables(server.getClass, true)
		server.started(stopID)
	}
	def startModule[M <: Module](clas: Class[M], stopID: Int = 0): Unit = {
		val server = bind(clas, systemClient, false, false, null)
		if (stopID == 0 && server.isRestorable) updateRestorables(server.getClass, true)
		server.started(stopID)
	}
	private[modules] def connectModule[M <: Module](clas: Class[M], stopID: Int): M = {
		val server = bind(clas, systemClient, false, false, null)
		server.started(stopID)
		server
	}

	/** @return true if stop has effect i.e. module is unbound. */
	def stopModule[M <: Module : Manifest]: Boolean = {
		unbind[M](moduleClass, systemClient) match {
			case Some(m) ⇒ updateRestorables(moduleClass, false); m.isUnbound
			case _ ⇒ false
		}
	}
	/** @return true if stop has effect i.e. module is unbound. */
	def stopModule[M <: Module : Manifest](stopID: Int): Boolean = {
		unbind[M](moduleClass, systemClient, stopID) match {
			case Some(m) ⇒ m.isUnbound
			case _ ⇒ false
		}
	}
	def stopModule[M <: Module](clas: Class[M], stopID: Int = 0): Boolean = {
		unbind(clas, systemClient, stopID) match {
			case Some(m) ⇒ m.isUnbound
			case _ ⇒ false
		}
	}
	/** @return true if stop has effect i.e. module is unbound. */
	private[modules] def disconnectModule[M <: Module](clas: Class[M], stopID: Int): Boolean = {
		unbind(clas, systemClient, stopID) match {
			case Some(m) ⇒ m.isUnbound
			case _ ⇒ false
		}
	}


	/* callbacks */
	protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = promise.complete()
	protected[this] def onModuleDestroy(m: Module): Unit = ()
	protected[this] def postStateUpdate(delay: Long)(implicit m: Module): Unit = {
		asyncContext.execute(m, delay, () => m.updateState())
	}
	protected[this] def cancelStateUpdate(implicit m: Module): Unit = {
		asyncContext.cancel(m)
	}
	protected[this] def currentTimeMs: Long = System.currentTimeMillis()


	/* INTERNAL API */
	private[modules] def bind[M <: Module](serverClas: Class[M], client: Module, sync: Boolean = false, restore: Boolean = false, constr: () ⇒ M = null): M = {
		logV(s"before LOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
		lock.lock()
		val holdCount = lock.getHoldCount
//		logV(s"after LOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
		try {
			if (!client.isInstanceOf[RootModule] && !modules.contains(client)) throw new ModuleBindException(serverClas, "It is not registered in system.")
			find(serverClas) match {
				case Some(server) ⇒ server.bindingAdd(client, sync)
//					logV(s"before UNLOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
					lock.unlock()
					server.onBindingAdd(client, sync)
					server
//		logV(s"Sys bind [${client.getClass.getSimpleName}] to [${m.getClass.getSimpleName}];  exists; sync? $sync; restore? $restore")
				case None ⇒ if (modules.isEmpty) startSystem()
					currentClas = serverClas
					currentClient = client
					currentSync = sync
					currentRestore = restore
					val server = try if (constr == null) serverClas.newInstance() else constr()
					catch {case e: Throwable ⇒ currentModule.fail(e); currentModule.asInstanceOf[M]}
					if (server != currentModule) logE(s"Detected a try to create ${serverClas.getName} outside the system.")// \n${currentModules.map(_.getClass.getSimpleName).mkString(", ")}")
					currentModule = null
//		logV(s"Sys Constructed [${clas.getSimpleName}]")
					server.setConstructed()
					server.onBindingAdd(client, sync)
					if (!detached.exists(isPredecessor(_, server))) prepareModule(server)
//		logV(s"Sys bind [${client.getClass.getSimpleName}] to [${m.getClass.getSimpleName}];  created; sync? $sync; restore? $restore")
					server
			}
		}
		finally if (holdCount >= lock.getHoldCount && lock.isHeldByCurrentThread) {
			logV(s"!!! unexpected UNLOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
			lock.unlock()
		}
	}
	private[modules] def attach(implicit m: Module): Boolean = {
		if (m.getClass == currentClas) {
			currentModule = m
			m.bindingAdd(currentClient, currentSync)
			currentClas = null
			currentClient = null
			modules += m
//			logV(s"before UNLOCK bind [${Thread.currentThread().getName}] :  [${m.getClass.getSimpleName}]")
			lock.unlock()
		}
		else if (!m.isInstanceOf[RootModule]) logE(s"Module [${m.getClass.getName}] is created outside the system.")
		currentRestore
	}
	private[this] def currentModule_=(m: Module): Unit = {
		val thread = Thread.currentThread()
		var list = currentModules.getOrElse(thread, Nil)
		m match {
			case null => list = list.tail
			case _ => list = m :: list
		}
		if (list.isEmpty) currentModules -= thread else currentModules += thread → list
	}
	private[this] def currentModule: Module = {
		val thread = Thread.currentThread()
		currentModules.get(thread) match {
			case None => null
			case Some(list) => list.head
		}
	}

	private[modules] def unbind[M <: Module](serverClas: Class[M], client: Module, stopID: Int = 0): Option[M] = {
		find(serverClas) match {
			case mOp@Some(m) if stopID == 0 || m.stopped(stopID) ⇒
//				logV(s"Sys unbind [${client.getClass.getSimpleName}] from [${m.getClass.getSimpleName}];  stopID= $stopID")
				m.bindingRemove(client)
				if (client == sysClient && !modules.exists(_.boundTo(sysClient))) stopping = true
				mOp
			case _ ⇒ None
		}
	}
	private[modules] def detach(implicit module: Module): Boolean = {
		lock.lock()
		try if (module.isUnbound) {
			modules -= module
			detached += module
			true
		} else false
		finally lock.unlock()
	}
	private[modules] def destroyed(implicit module: Module): Unit = {
		detached -= module
		cancelStateUpdate(module)
		modules.foreach { m ⇒
			if (isPredecessor(m, module)) prepareModule(m)
			else m.bindingRemove(module)
		}
		try onModuleDestroy(module) catch loggedE
		if (canStop) FutureX(stopSystem())
	}


	/* PRIVATE API */
	private[this] def find[M <: Module](clas: Class[M]): Option[M] = {
		modules.find(m ⇒ m.getClass == clas).asInstanceOf[Option[M]]
	}
	private[this] def prepareModule(m: Module): Unit = if (m.not(StateParams.Prepared)) {
		try onModulePrepare(new ModulePreparePromise(m)) catch loggedE
	}

	private[this] def isPredecessor(m1: Module, m2: Module): Boolean = {
		m1.getClass == m2.getClass && m1.ne(m2)
	}
	private[this] def moduleClass[M <: Module](implicit m: Manifest[M]): Class[M] = {
		m.runtimeClass.asInstanceOf[Class[M]]
	}
	private[this] def systemClient: Module = lock synchronized {
		if (sysClient == null) sysClient = new RootModule
		sysClient
	}

	/* BACK DOOR */
	private[modules] def postUpdate(delay: Long)(implicit m: Module): Unit = postStateUpdate(delay)
	private[modules] def cancelUpdate()(implicit m: Module): Unit = cancelStateUpdate
	private[modules] def now: Long = currentTimeMs
}







/* RESTORE SUBSYSTEM */
private[modules] trait RestoreSubsystem {
	mgr: ModuleSystem ⇒
	def checkRestore(): Unit = {
	}
	private def restore(modules: List[String]): Unit = modules.foreach { name ⇒
	}
	def updateRestorables(m: Class[_], yeap: Boolean): Unit = {
	}
}






/* ROOT MODULE */
private[modules] class RootModule extends Module {
	// TODO ?
//	override protected[this] lazy val lifeCycle: LifeCycle = new LifeCycle {
//		override protected[this] def onActivatingStart(primary: Boolean, complete: CompleteSelector) = complete.when(true)
//	}
	//	lifeCycle.completeActivating()
}




/* MODULE CREATEP ROMISE */
class ModulePreparePromise(val module: Module) {
	def complete(): Unit = module.setPrepared()
}




/* MODULE HOLDER */
/** Usage Contract:  Before discard object call onDestroy method. After that module will not be able to serve requests. */
trait ModuleConnector[M <: Module] {
	import ModuleSystem._
	protected[this] val moduleClass: Class[M]
	private[this] val stopID = nextStopID
	private[this] var _module: M = _
	private[this] val lock = new Object

	//todo with mmodule constructor
	protected[this] def module: M = lock synchronized {
		if (_module == null) _module = instance.connectModule(moduleClass, stopID)
		_module
	}
	protected[this] def moduleDisconnect(): Unit = lock synchronized {
		if (_module != null) instance.disconnectModule(moduleClass, stopID)
		_module = null.asInstanceOf[M]
	}
	protected[this] def onDestroy(): Unit = moduleDisconnect()
	override def finalize(): Unit = {
		moduleDisconnect()
		super.finalize()
	}
}

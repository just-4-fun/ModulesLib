package just4fun.core.modules

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import just4fun.core.async.{FutureContext, FutureContextOwner}
import just4fun.utils.logger.Logger._


private[modules] object ModuleSystem {
	var instance: ModuleSystem = null
	private[this] var stopID: Int = 0
	def nextStopID: Int = { stopID -= 1; stopID }
}



trait ModuleSystem extends FutureContextOwner {
	ModuleSystem.instance = this
	implicit val thisSystem: this.type = this
	implicit val asyncContext: FutureContext
	protected[this] val restoreAgent: ModuleRestoreAgent = null
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
	final def isSystemStopped: Boolean = sysClient == null && modules.isEmpty && detached.isEmpty
	final def isSystemStopping: Boolean = stopping
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
//		logV(s"canStop locked? ${lock.isLocked};  detached.empty? ${detached.isEmpty};  mods: ${modules.map(_.getClass.getSimpleName).mkString(", ")}")
		modules.isEmpty && detached.isEmpty
	}
	private[this] def stopSystem(): Unit = {
//		logV(s"SYS Stop: canStop? ${canStop}; notStopped? ${!isSystemStopped};  nonLocked& ${!lock.isLocked}")
		if (canStop && !isSystemStopped && !lock.isLocked) {
			asyncContext.stop(true)
			if (!lock.isLocked) onSystemStop()
			asyncContext.stop(true)
			sysClient = null
			logV(s"*************  SYSTEM STOP  **************")
		}
	}

	/* MODULE API */

	final def hasModule[M <: Module : Manifest]: Boolean = hasModule(moduleClass)
	final def hasModule[M <: Module](cls: Class[M]): Boolean = find(cls).nonEmpty

	final def startModule[M <: Module : Manifest]: Unit = {
		bind(moduleClass, systemClient)
	}
	final def startModule[M <: Module : Manifest](constructor: ⇒ M): Unit = {
		bind(moduleClass, systemClient, constr = () ⇒ constructor)
	}
	final def startModule[M <: Module : Manifest](stopID: Int): Unit = {
		bind(moduleClass, systemClient, false, false, null, stopID)
	}
	final def startModule[M <: Module](clas: Class[M], stopID: Int = 0): Unit = {
		bind(clas, systemClient, false, false, null, stopID)
	}
	private[modules] def connectModule[M <: Module](clas: Class[M], stopID: Int): M = {
		bind(clas, systemClient, false, false, null, stopID)
	}

	final def stopModule[M <: Module : Manifest]: Unit = {
		unbind[M](moduleClass, systemClient)
	}
	final def stopModule[M <: Module : Manifest](stopID: Int): Unit = {
		unbind[M](moduleClass, systemClient, stopID)
	}
	final def stopModule[M <: Module](clas: Class[M], stopID: Int = 0): Unit = {
		unbind(clas, systemClient, stopID)
	}
	private[modules] def disconnectModule[M <: Module](clas: Class[M], stopID: Int): Unit = {
		unbind(clas, systemClient, stopID)
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


	/* MODULE BIND */
	private[modules] def bind[M <: Module](serverClas: Class[M], clentModule: Module, sync: Boolean = false, restore: Boolean = false, constr: () ⇒ M = null, stopID: Int = 0): M = {
//		logV(s"before LOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
		lock.lock()
		val holdCount = lock.getHoldCount
//		logV(s"after LOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
		try {
			val client = if (clentModule == null) systemClient else clentModule
			if (!client.isInstanceOf[RootModule] && !modules.contains(client)) throw new ModuleBindException(serverClas, "It's not registered in system.")
			find(serverClas) match {
				case Some(server) ⇒ server.bindingAdd(client, sync)
//					logV(s"before UNLOCK bind [${Thread.currentThread().getName}] :  [${serverClas.getSimpleName}]")
					lock.unlock()
					if (!server.isConstructed) server.synchronized(server.wait(100))
					server.onBindingAdd(client, sync)
					if (stopID != 0) server.started(stopID)
					server
//		logV(s"Sys bind [${client.getClass.getSimpleName}] to [${m.getClass.getSimpleName}];  exists; sync? $sync; restore? $restore")
				case None ⇒ if (modules.isEmpty) startSystem()
					currentClas = serverClas
					currentClient = client
					currentSync = sync
					currentRestore = restore
					val server = try if (constr == null) serverClas.newInstance() else constr()
					catch {
						case e: Throwable ⇒ currentModule.fail(e, false)
							currentModule.asInstanceOf[M]
					}
					if (server != currentModule) throw new ModuleBindException(serverClas, s"It's constructed outside the system.")
					currentModule = null
//		logV(s"Sys Constructed [${clas.getSimpleName}]")
					server.setConstructed()
					server synchronized (server.notifyAll())
					server.onBindingAdd(client, sync)
					if (!detached.exists(isPredecessor(_, server))) prepareModule(server)
//		logV(s"Sys bind [${client.getClass.getSimpleName}] to [${m.getClass.getSimpleName}];  created; sync? $sync; restore? $restore")
					if (stopID != 0) server.started(stopID)
					else if (server.shouldRestore && client == sysClient && restoreAgent != null) restoreAgent.set(server, true)
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
	private[this] def prepareModule(m: Module): Unit = if (!m.isPrepared) {
		try onModulePrepare(new ModulePreparePromise(m)) catch loggedE
	}

	/* MODULE UNBIND */
	private[modules] def unbind[M <: Module](serverClas: Class[M], clentModule: Module, stopID: Int = 0): Option[M] = {
		val client = if (clentModule == null) systemClient else clentModule
		find(serverClas) match {
			case mOp@Some(m) if stopID == 0 || m.stopped(stopID) ⇒
//				logV(s"Sys unbind [${client.getClass.getSimpleName}] from [${m.getClass.getSimpleName}];  stopID= $stopID")
				if (!m.isConstructed) m.synchronized(m.wait(100))
				m.bindingRemove(client)
				if (client == sysClient && !modules.exists(_.isBoundTo(sysClient))) stopping = true
				mOp
			case _ ⇒ None
		}
	}
	private[modules] def detach(implicit module: Module): Boolean = {
		lock.lock()
		val ok = try if (module.hasNoBindings) {
			modules -= module
			detached += module
			true
		} else false
		finally lock.unlock()
		if (ok && module.isRestorable && restoreAgent != null) restoreAgent.set(module, false)
		ok
	}
	private[modules] def destroyed(implicit module: Module): Unit = {
		detached -= module
		cancelStateUpdate(module)
		modules.foreach { m ⇒
			if (isPredecessor(m, module)) prepareModule(m)
			else m.bindingRemove(module)
		}
		try onModuleDestroy(module) catch loggedE
		if (canStop) asyncContext.execute(() ⇒ stopSystem())
	}

	/* MODULE MISC */
	private[this] def find[M <: Module](clas: Class[M]): Option[M] = {
		modules.find(m ⇒ m.getClass == clas).asInstanceOf[Option[M]]
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






/* ROOT MODULE */
private[modules] class RootModule extends Module




/* MODULE PREPARE PROMISE */
class ModulePreparePromise(val module: Module) {
	def complete(): Unit = module.setPrepared()
}




/* MODULE CONNECTOR */
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



/* RESTORE AGENT */
abstract class ModuleRestoreAgent(implicit system: ModuleSystem) {
	private[this] var phase = 0
	private[this] var temp: Set[String] = null
	lazy val autoStart = true
	if (autoStart) start()

	protected[this] def getList: TraversableOnce[String]
	protected[this] def clearList(): Unit
	protected[this] def add(moduleClass: String): Unit
	protected[this] def remove(moduleClass: String): Unit
	protected[this] def onStart(codeToRun: () ⇒ Unit): Unit = system.asyncContext.execute { () ⇒ codeToRun() }
	protected[this] def onRestored(clas: Class[_]): Unit = ()


	final def start(): Unit = if (phase == 0) {
		phase = 1
		try onStart(exec) catch loggedE
	}
	private[this] def exec(): Unit = {
		val classNames = getList match {
			case null => Nil
			case list => if (list.nonEmpty) clearList(); list
		}
		synchronized {
			if (temp != null) temp.foreach(clas ⇒ try add(clas) catch loggedE)
			phase = 2
			temp = null
		}
		classNames.foreach { name => try restore(name) catch loggedE }
	}
	private[this] def restore(name: String): Unit = {
		val cls = Class.forName(name).asInstanceOf[Class[_ <: Module]]
//		logV(s"BEFORE  RESTORED  ${cls.getSimpleName};  not yet created? ${!system.hasModule(cls)}")
		if (!system.hasModule(cls)) {
			val m = system.bind(cls, null, false, true)
			if (!m.shouldRestore) system.unbind(cls, null)
			else onRestored(cls)
		}
//		logV(s"AFTER RESTORED  ${cls.getSimpleName}")
	}
	protected[modules] final def set(m: Module, yep: Boolean): Unit = synchronized {
		if (yep) m.setRestorable()
		val clas = m.getClass.getName
		if (phase < 2) {
			if (temp == null) temp = Set()
			if (yep) temp += clas else temp -= clas
		}
		else try if (yep) add(clas) else remove(clas) catch loggedE
	}
}

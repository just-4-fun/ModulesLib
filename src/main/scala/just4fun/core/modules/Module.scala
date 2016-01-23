package just4fun.core.modules

import scala.StringBuilder
import just4fun.core.async.FutureX.DummyImplicit2
import just4fun.core.async.{FutureContextOwner, FutureContext, FutureX}
import just4fun.utils.logger.Logger._
import just4fun.utils.logger.LoggerConfig

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.util.{Failure, Try}


/* STATES */
object ModuleState extends Enumeration {
	type ModuleState = Value
	val PREPARING, RESTING, ACTIVATING, OPERATING, DEACTIVATING, FAILED, DESTROYED = Value
}


object Module {
	private[modules] val uFlagNONE = 0
	private[modules] val uFlagREPEAT = 1
	private[modules] val uFlagBUSY = 2
	private[modules] val uFlagTOUCHED = 3
	private[modules] val uFlagRESUME = 4
	val tagState = 0x7102111
	val tagStateX = 0x7102112
	val tagParam = 0x7102113
}

/* MODULE */
trait Module extends StateProcessor with BindingProcessor with RequestProcessor with FutureContextOwner {
	import ModuleState._
	import StateParams._
	type System <: ModuleSystem
	implicit final val thisModule: this.type = this
	private[this] lazy val stopIDs = mutable.Set[Int]()
	protected[this] final val system: System = ModuleSystem.instance.asInstanceOf[System]
	protected[this] final val restored = system.attach
	protected[this] val restore = false
	implicit protected val asyncContext: FutureContext = system.asyncContext
	protected[this] val lifeCycle = new LifeCycle
	protected[this] val internal = new InternalCallbacks
	lazy final val info = new ModuleInfo

	/* INTERNAL */
	private[modules] def shouldRestore: Boolean = restore
	private[modules] def setRestorable(): Unit = on_(Restorable)
	private[modules] def isRestorable: Boolean = is(Restorable)
	private[modules] def started(id: Int): Unit = stopIDs += id
	private[modules] def stopped(id: Int): Boolean = {
		stopIDs -= id
		stopIDs.isEmpty
	}

	/* LIFECYCLE CALLBACKS */
	class LifeCycle {
		import ModuleState._
		import StateParams._
		protected[this] var restLatencyMs = 10000
		protected[this] var destroyLatencyMs = 0
		// STATE callbacks
		final def completeActivating(): Unit = if (isActivating) on_!!!(Complete)
		final def completeDeactivating(): Unit = if (isDeactivating) on_!!!(Complete)
		protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		//		protected[this] def isActivatingComplete(initializing: Boolean, durationMs: Long): Boolean = true
		protected[this] def onActivatingComplete(creating: Boolean): Unit = ()
		protected[this] def onDeactivatingStart(destroying: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		//		protected[this] def isDeactivatingComplete(destroying: Boolean, durationMs: Long): Boolean = true
		protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = ()
		protected[this] def progressBackoff: Long = calcBackoff
		protected[this] final def progressDurationMs: Long = progressDuration
		protected[this] def onFailed(): Unit = ()
		// BACK DOOR
		private[modules] def callFailed(): Unit = {
			try onFailed() catch loggedE
		}
		private[modules] def callStart(): CompleteOption = {
			if (isActivating) onActivatingStart(is(Creating), CompleteSelector) else onDeactivatingStart(is(Detached), CompleteSelector)
		}
		private[modules] def callComplete(): Unit = {
			if (isActivating) onActivatingComplete(is(Creating)) else onDeactivatingComplete(is(Detached))
		}
		private[modules] def latency = if (hasNoBindings) destroyLatencyMs else restLatencyMs
		private[modules] def backoff: Long = try progressBackoff catch loggedE(calcBackoff)
	}

	/* INTERNAL CALLBACKS */
	class InternalCallbacks {
		protected[this] def onConstructed(): Unit = ()
		protected[this] def onPrepared(): Unit = ()
		protected[this] def onDestroyed(): Unit = ()
		// BINDING callbacks
		protected[this] def onBindingAdd(moduleClas: Class[_], sync: Boolean): Unit = ()
		protected[this] def onBindingRemove(moduleClas: Class[_]): Unit = ()
		// REQUEST callbacks
		protected[this] def onRequestAdd(requestClas: Class[_]): Unit = {}
		protected[this] def onRequestRemove(requestClas: Class[_]): Unit = {}
		//
		protected[this] def onAbleToServeNow(yep: Boolean): Unit = {}
		// BACK DOOR
		private[modules] def callConstructed() = tryOrDie(onConstructed())
		private[modules] def callPrepared() = tryOrDie(onPrepared())
		private[modules] def callDestroyed() = try onDestroyed() catch loggedE
		private[modules] def callBindAdd(clas: Class[_], sync: Boolean): Unit = tryOrDie(onBindingAdd(clas, sync))
		private[modules] def callBindRemove(clas: Class[_]): Unit = tryOrDie(onBindingRemove(clas))
		private[modules] def callRequestAdd(clas: Class[_]): Unit = tryOrDie(onRequestAdd(clas))
		private[modules] def callRequestRemove(clas: Class[_]): Unit = tryOrDie(onRequestRemove(clas))
		private[modules] def callAbleToServeNow(yep: Boolean) = tryOrDie(onAbleToServeNow(yep))
		//
		final def internalBind[M <: Module](clas: Class[M], sync: Boolean): M = system.bind[M](clas, thisModule, sync)
		final def internalUnbind[M <: Module](clas: Class[M]): Unit = system.unbind[M](clas, thisModule)
	}

	/* PUBLIC INFO */
	class ModuleInfo {
		import StateParams._
		def state: ModuleState = getState
		def isPreparing: Boolean = getState == PREPARING
		def isOperating: Boolean = getState == OPERATING
		def isActivating: Boolean = getState == ACTIVATING
		def isDeactivating: Boolean = getState == DEACTIVATING
		def isResting: Boolean = getState == RESTING
		def isFailed: Boolean = getState == FAILED
		def isUnavailable: Boolean = is(Detached)
		def isSuspended: Boolean = is(Suspended)
		def isRestful: Boolean = is(Restful)
		def failure: Option[Throwable] = Module.this.failure
	}

}




/* BINDING PROCESSOR */
trait BindingProcessor {
	self: Module =>
	import StateParams._
	private[this] val clients = mutable.Set[Module]()
	private[modules] val syncClients = mutable.Set[Module]()
	private[modules] val syncServers = mutable.Set[Module]()

	protected[modules] final def hasBindings: Boolean = clients.nonEmpty
	protected[modules] final def hasNoBindings: Boolean = clients.isEmpty
	private[modules] def bindingsNum = clients.size
	protected[this] final def bind[M <: Module : Manifest]: M = macro Macros.bind[M]
	protected[this] final def bindSync[M <: Module : Manifest]: M = macro Macros.bindS[M]
	protected[this] final def unbind[M <: Module : Manifest]: Unit = {
		internal.internalUnbind(implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]])
	}
	protected[this] final def unbind[M <: Module : Manifest](s: M): Unit = {
		internal.internalUnbind(s.getClass.asInstanceOf[Class[M]])
	}
	protected[modules] final def isBoundTo(client: Module): Boolean = clients.contains(client)
	/* INTERNAL */
	private[modules] def bindingAdd(client: Module, sync: Boolean): Unit = clients += client
	private[modules] def onBindingAdd(client: Module, sync: Boolean): Unit = {
		clients.contains(client) match {
			case true ⇒
				internal.callBindAdd(client.getClass, sync)
				if (not(HasBindings)) on_!!!(HasBindings)
				if (sync) addSyncClient(client)
				if (LoggerConfig.isDebug) client.detectCyclicBinding(this, client :: Nil) match {
					case Nil =>
					case trace => logW(s"Cyclic module relations detected in chain [${trace.map(_.getClass.getSimpleName).mkString(", ")}]")
				}
			case _ ⇒ if (!sync) removeSyncClient(client)
		}
	}
	private[modules] def bindingRemove(client: Module): Unit = {
		if (clients.remove(client)) {
			internal.callBindRemove(client.getClass)
			removeSyncClient(client)
			if (clients.isEmpty) off_!!!(HasBindings)
		}
	}
	private[this] def addSyncClient(client: Module): Unit = if (syncClients.add(client)) {
		client.addSyncServer(this)
		if (isFailed) client.fail(new SyncServerException(this))
		else clientActive(client.is(Active))
	}
	private[this] def removeSyncClient(client: Module): Boolean = syncClients.remove(client) match {
		case true => client.removeSyncServer(this); clientActive(false); true
		case false => false
	}
	private[BindingProcessor] def addSyncServer(server: Module): Unit = {
		if (syncServers.add(server) && !server.isOperating) serverOperating(false)
	}
	private[BindingProcessor] def removeSyncServer(server: Module): Unit = {
		if (syncServers.remove(server)) serverOperating(true)
	}
	private[BindingProcessor] def detectCyclicBinding(server: Module, trace: List[Module]): List[Module] = {
		if (this != server) clients.foreach { client =>
			val res = if (this == client) Nil else if (server == client) client :: trace else client.detectCyclicBinding(server, client :: trace)
			if (res.nonEmpty) return res
		}
		Nil
	}

	/* EVENT API */
	protected[this] def fireEvent[T <: Module : Manifest](e: ModuleEvent[this.type, T], modules: T*): Unit = {
		(if (modules.isEmpty) clients else modules).foreach {
			case m: T => m.tryOrDie(e.onFired(this, m))
			case _ =>
		}
	}
}




/* REQUEST PROCESSOR */
trait RequestProcessor {
	self: Module =>
	import StateParams._
	//	private[this] val requests = mutable.ArrayBuffer[ModuleRequest[_]]()
	private[this] val requests = mutable.ListBuffer[ModuleRequest[_]]()
	private[this] var execCount: Int = 0

	protected final def hasRequests: Boolean = requests.synchronized(requests.nonEmpty)
	protected final def hasNoRequests: Boolean = requests.synchronized(requests.isEmpty)
	private[modules] def requestsNum = requests.size
	protected[this] final def serveRequest[T](request: ModuleRequest[T]): FutureX[T] = requestAdd(request)
	protected[this] final def cancelRequests(filter: ModuleRequest[_] => Boolean = null): Unit = requests synchronized {
		filter match {
			case null => requests.foreach(_.cancel())
			case _ => requests.withFilter(filter).foreach(_.cancel())
		}
	}
	/* wrappers */
	protected[this] final def serveOpt[T](code: => T): Option[T] = {
		if (isAbleToServeNow) Option(code) else None
	}
	protected[this] final def serveTry[T](code: => T): Try[T] = {
		if (isAbleToServeNow) Try(code) else Failure(new ModuleServiceException)
	}
	protected[this] final def serveAsync[T](code: => T)(implicit asyncContext: FutureContext): FutureX[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] final def serveAsync[T](code: => FutureX[T])(implicit asyncContext: FutureContext, d: DummyImplicit): FutureX[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] final def serveAsync[T](code: => Future[T])(implicit asyncContext: FutureContext, d: DummyImplicit, d2: DummyImplicit2 = null): FutureX[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	/* INTERNAL */
	private[this] def requestAdd[T](request: ModuleRequest[T]): FutureX[T] = {
		//DEF
		def addAndHasWork(): Boolean = requests synchronized {
			val wasEmpty = requests.isEmpty
			requests += request
			wasEmpty && not(Suspended)
		}
		//EXEC
		internal.callRequestAdd(request.getClass)
		request.module = this
		//
		if (request.isDone) requestComplete(request)
		else if (!isAbleToServe) request.cancel(new ModuleServiceException)
		else {
			if (addAndHasWork()) on_!!!(HasWork)
			if (isAbleToServeNow && notUpdating) request.activate()
		}
		request
	}
	private[modules] def requestStart(request: ModuleRequest[_]): Unit = {
		execCount += 1
	}
	private[modules] def requestComplete(request: ModuleRequest[_]): Unit = {
		//DEF
		def removeAndHasNoWork(): Boolean = requests synchronized {
			requests -= request
			requests.isEmpty match {
				case true ⇒ execCount = 0; true
				case _ ⇒ execCount -= 1; execCount == 0 && is(Suspended)
			}
		}
		//EXEC
		internal.callRequestRemove(request.getClass)
		if (removeAndHasNoWork()) off_!!!(HasWork)
	}
	private[modules] def pauseRequests(): Unit = requests.synchronized(requests.foreach(_.deactivate()))
	private[modules] def resumeRequests(): Unit = requests.synchronized(requests.foreach(_.activate()))
	private[modules] def executingRequest: Boolean = execCount > 0
}




/* STATE PROCESSOR */
trait StateProcessor {
	self: Module =>
	import Module._
	import ModuleState._
	import StateParams._
	private[this] var state: ModuleState = PREPARING
	private[this] var error: Throwable = null
	private[this] var completeOption: CompleteOption = _
	private[this] var stateStartTime = 0L
	private[this] val updateLock = new Object
	private[this] var updateFlag = uFlagNONE
	private[this] var expectUpdate = false
	//
	private[this] var params = 1 << Creating.id | 1 << AllServersOperating.id

	protected[this] final def setFailed(e: Throwable, recoverable: Boolean = true): Unit = fail(e, recoverable)
	protected[this] final def setRestful(on: Boolean): Unit = set(Restful, on)
	protected[this] final def suspendService(on: Boolean): Unit = if (is(Suspended) != on) {
		set(Suspended, on, true)
		if (state == OPERATING) on match {
			case true => pauseRequests(); internal.callAbleToServeNow(false)
			case _ => resumeRequests(); internal.callAbleToServeNow(true)
		}
		if (on && !executingRequest) off_!!!(HasWork)
		else if (!on && hasRequests) on_!!!(HasWork)
		if (on && executingRequest) logV(s"EXEC while Suspended!!!")

	}
	protected[this] final def recoverable: Boolean = not(Irrecoverable)
	protected[this] final def recover(): Boolean = state == FAILED && not(Detached) && recoverable match {
		case true ⇒ on_!!!(Recover); true
		case _ ⇒ false
	}

	final def isAbleToServeNow: Boolean = state == OPERATING && not(Suspended)
	final def isAbleToServe: Boolean = state != FAILED && not(Detached)
	// INFO
	protected[modules] final def getState: ModuleState = state
	protected[modules] final def getParams: Int = params
	protected[modules] final def isPreparing: Boolean = state == PREPARING
	protected[modules] final def isOperating: Boolean = state == OPERATING
	protected[modules] final def isActivating: Boolean = state == ACTIVATING
	protected[modules] final def isDeactivating: Boolean = state == DEACTIVATING
	protected[modules] final def isResting: Boolean = state == RESTING
	protected[modules] final def isFailed: Boolean = state == FAILED
	protected[modules] final def isUnavailable: Boolean = is(Detached)
	protected[modules] final def isConstructed: Boolean = is(Constructed)
	protected[modules] final def isPrepared: Boolean = is(Prepared)
	protected[modules] final def isSuspended: Boolean = is(Suspended)
	protected[modules] final def isRestful: Boolean = is(Restful)
	protected[modules] final def failure: Option[Throwable] = Option(error)

	/* INTERNAL */

	private[this] def set(param: StateParams, on: Boolean, silent: Boolean = false): Unit = setParam(param, on, silent)
	private[modules] def on_!!!(param: StateParams): Unit = setParam(param, true, false)
	private[modules] def on_(param: StateParams): Unit = setParam(param, true, true)
	private[modules] def off_!!!(param: StateParams): Unit = setParam(param, false, false)
	private[modules] def off_(param: StateParams): Unit = setParam(param, false, true)
	private[modules] def is(param: StateParams): Boolean = (params & (1 << param.id)) != 0
	private[modules] def not(param: StateParams): Boolean = (params & (1 << param.id)) == 0

	private[this] def setParam(param: StateParams, isOn: Boolean, silent: Boolean): Unit = {
		val prev = params
		if (isOn) params |= (1 << param.id) else params &= ~(1 << param.id)
		logV(s"[${getClass.getSimpleName}]:  [${(0 until StateParams.values.size).map(n ⇒ if ((params & (1 << n)) == 0) 0 else 1).mkString("")}];  $param= $isOn;  ${if (prev != params && !silent) ">>> " else if (prev != params) "V" else "-"}", tagParam)
		if (prev != params && !silent) updateState()
	}

	private[modules] def updateState(): Unit = {
		// EXEC
		if (nonBusy) {
			update()
//			if (hasFlag(uFlagRESUME)) logV(s"[${getClass.getSimpleName}]:  UPDATE RESUME")
			if (touched) updateState()
		}
		// DEFs
		def update(): Unit = {
			if (expectUpdate) {system.cancelUpdate(); expectUpdate = false}
			val prevState = state
			state match {
				case PREPARING => if (destroy_?) destroy_>> else if (is(Prepared)) prepared_>>
				case RESTING => if (destroy_?) destroy_>> else if (activate_?) activate_>>
				case ACTIVATING => if (activated_?) operate_>>
				case OPERATING => if (deactivate_?) deactivate_>>
				case DEACTIVATING => if (complete_?) rest_>>
				case FAILED => if (abandoned_?) destroy_>> else if (is(Recover)) recover_>>
				case DESTROYED => //todo can module or system do something after destroyed (ex:prepare)
				case _ =>
			}
			if (prevState != state) {
				stateStartTime = system.now
				if (state != DESTROYED) setFlag(uFlagTOUCHED)
			}
			else logW(s"[${getClass.getSimpleName}]:  $prevState -->  X", tagStateX)
		}
		def nonBusy = updateLock.synchronized {
			updateFlag == uFlagNONE || hasFlag(uFlagREPEAT) match {
				case true ⇒ unsetFlag(uFlagREPEAT); setFlag(uFlagBUSY); true
				case false ⇒ setFlag(uFlagTOUCHED); false
			}
		}
		def touched = updateLock.synchronized {
			hasFlag(uFlagTOUCHED) match {
				case true ⇒ unsetFlag(uFlagTOUCHED); setFlag(uFlagREPEAT); true
				case _ ⇒ if (hasFlag(uFlagRESUME) && isAbleToServeNow) resumeRequests()
					updateFlag = uFlagNONE
					false
			}
		}
		def hasFlag(f: Int) = (updateFlag & (1 << f)) != 0
		def setFlag(f: Int) = updateFlag |= 1 << f
		def unsetFlag(f: Int) = updateFlag &= ~(1 << f)
	}

	private[modules] def notUpdating: Boolean = updateLock.synchronized {
		updateFlag == uFlagNONE || {
			updateFlag |= 1 << uFlagRESUME
			false
		}
	}
	private[this] def setState(v: ModuleState): Unit = {
		logW(s"[${getClass.getSimpleName}]:  $state -->  $v", tagState)
		state = v
	}

	private[modules] def setConstructed(): Unit = {
		on_(Constructed)
		if (internal != null) internal.callConstructed()
	}
	private[modules] def setPrepared(): Unit = {
		if (internal != null) internal.callPrepared()
		on_!!!(Prepared)
	}
	private[this] def prepared_>> : Unit = {
		setState(RESTING)
	}
	//todo should it wait when is creating
	private[this] def activate_? : Boolean = changeActive(hasNoBindings || not(Restful) || hasWork || is(HasClientActive)) && canActivate
	private[this] def canActivate: Boolean = is(AllServersOperating)
	private[this] def hasWork: Boolean = hasRequests && not(Suspended)
	private[this] def activate_>> : Unit = tryOrFail {
		setState(ACTIVATING)
		startProgress()
	}
	private[this] def startProgress(): Unit = {
		completeOption = lifeCycle.callStart()
		if (completeOption == CompleteNow) on_(Complete)
	}
	private[this] def activated_? : Boolean = canActivate && complete_?
	private[this] def complete_? : Boolean = completeOption match {
		case CompleteWhen(isComplete) => tryOrFail(isComplete() || {postUpdateState(lifeCycle.backoff); false})
		case _ if is(Complete) => off_(Complete); true
		case _ => false
	}
	private[this] def operate_>> : Unit = tryOrFail {
		lifeCycle.callComplete()
		if (is(Creating)) off_(Creating)
		setState(OPERATING)
		syncClients.foreach(_.serverOperating(true))
		if (not(Suspended)) {
			resumeRequests()
			internal.callAbleToServeNow(true)
		}
	}
	private[this] def deactivate_? : Boolean = canDeactivate match {
		case true if is(Delayed) || is(Suspended) || lifeCycle.latency == 0 => off_(Delayed); hasBindings || detach
		case true => on_(Delayed); postUpdateState(lifeCycle.latency); false
		case _ => off_(Delayed); false
	}
	private[this] def canDeactivate: Boolean = hasNoWork && (hasNoBindings || (is(Restful) && not(HasClientActive)))
	private[this] def hasNoWork: Boolean = hasNoRequests || (is(Suspended) && !executingRequest)
	private[this] def detach: Boolean = is(Detached) || (system.detach match {
		case true ⇒ on_(Detached); true
		case _ ⇒ false
	})
	private[this] def deactivate_>> : Unit = tryOrFail {
		setState(DEACTIVATING)
		syncClients.foreach(_.serverOperating(false))
		startProgress()
		if (not(Suspended)) internal.callAbleToServeNow(false)
		if (is(Detached)) cancelRequests()
	}
	private[this] def rest_>> : Unit = tryOrFail {
		setState(RESTING)
		lifeCycle.callComplete()
		changeActive(false)
		asyncContext.stop(true) // todo test no requests lost
	}
	private[this] def abandoned_? : Boolean = hasNoBindings && is(Constructed) && detach
	private[this] def recover_>> : Unit = {
		setState(RESTING)
		on_(Creating)
		off_(Recover)
		error = null
		syncClients.foreach(m ⇒ if (m.isFailed) m.on_!!!(Recover))
	}
	private[modules] def tryOrFail[T](code: => T): T = try code catch {
		case err: Throwable => fail(err); null.asInstanceOf[T]
	}
	private[modules] def tryOrDie[T](code: => T): T = try code catch {
		case err: Throwable => fail(err, false); null.asInstanceOf[T]
	}
	private[modules] def fail(err: Throwable, recoverable: Boolean = true): Unit = {
		logE(err, s"Module [${getClass.getName}] is ${if(recoverable) "" else "irrecoverably"} failed in [$state] state. ")
		if (error == null && state != DESTROYED) {
			if (!recoverable) on_(Irrecoverable)
			setState(FAILED)
			error = err
			changeActive(false)
			if (lifeCycle != null) lifeCycle.callFailed()
			cancelRequests()
			if (asyncContext != null) asyncContext.stop()
			syncClients.foreach(_.fail(new SyncServerException(this)))
			internal.callAbleToServeNow(false)
		}
	}
	private[this] def destroy_? : Boolean = is(Detached) || (hasNoBindings && is(Creating) && is(Constructed) && !hasWork && detach)
	private[this] def destroy_>> : Unit = {
		setState(DESTROYED)
		if (internal != null) internal.callDestroyed()
		cancelRequests()
		if (asyncContext != null) asyncContext.exit()
		system.destroyed
	}

	private[modules] def serverOperating(on: Boolean): Unit = {
		if (!on) off_(AllServersOperating) else if (syncServers.forall(_.isOperating)) on_!!!(AllServersOperating)
	}
	private[modules] def clientActive(isOn: Boolean): Unit = if (is(HasClientActive) != isOn) {
		if (isOn || syncClients.forall(_.not(Active))) set(HasClientActive, isOn)
	}
	private[this] def changeActive(isOn: Boolean): Boolean = is(Active) != isOn match {
		case true => set(Active, isOn); syncServers.foreach(_.clientActive(isOn)); isOn
		case _ => isOn
	}

	private[modules] def postUpdateState(delay: Long): Unit = {
		if (expectUpdate) system.cancelUpdate() else expectUpdate = true
		system.postUpdate(delay)
	}
	private[modules] def progressDuration: Long = system.now - stateStartTime
	private[modules] def calcBackoff: Long = {
		val durationMs = progressDuration
		val delay = if (durationMs < 2000) 200
		else if (durationMs < 10000) 1000
		else if (durationMs < 60000) 5000
		else 10000
		if (isActivating) delay else (delay * 1.5).toLong
	}
	protected[this] def stateInfo(): String = {
		val buff = new StringBuilder
		buff ++= "state: " ++= state.toString ++= ";  bindings: " ++= bindingsNum.toString ++= ";  requests: " ++= requestsNum.toString ++= ";  params: "
		StateParams.values.foreach { param ⇒
			if (param.id > 0) buff ++= ", "
			buff ++= param.toString ++= ":"
			if ((params & (1 << param.id)) == 0) buff ++= "0" else buff ++= "1"
		}
		buff.toString()
	}
}


/* STATE PARAMS */
private[modules] object StateParams extends Enumeration {
	type StateParams = Value
	/** Up to 32 params */
	val HasBindings,// has clients bound
	HasWork,// (has requests and not suspended) or (suspended and currently has executing request)
	Restful,// mode allowing go to REST if has no work, and go to OPERATING if has
	Suspended,// for some reason can not currently execute requests
	HasClientActive,// if any of sync client is active -> should activate; all sync clients rest -> can rest
	AllServersOperating,// if all sync servers active -> can activate
	Active, // derivative from above params
	Creating, // indicates first lap from creating until OPERATING
	Detached, // detached from system, indicates last lap to DESTROYED
	Constructed, // instance constructed
	Prepared, // instance prepared
	Complete, //transition: ACTIVATING > OPERATING or DEACTIVATING > RESTING
	Delayed, // final step in delayed deactivation
	Recover, //recovering after FAILED
	Irrecoverable,// failed with irrecoverable error
	Restorable// on system crash will be restored next time with system start
	= Value
}




/* PROGRESS OPTIONS  */

object CompleteSelector extends CompleteSelector

class CompleteSelector {
	def now: CompleteOption = CompleteNow
	def manually: CompleteOption = CompleteManually
	def when(isComplete: => Boolean): CompleteOption = CompleteWhen(() => isComplete)
}

class CompleteOption
object CompleteNow extends CompleteOption
object CompleteManually extends CompleteOption
case class CompleteWhen(isComplete: () => Boolean) extends CompleteOption


/* MODULE EVENT */
abstract class ModuleEvent[-S <: Module, T <: Module : Manifest] {
	def onFired(source: S, target: T): Unit
}
